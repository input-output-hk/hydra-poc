{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Tx.Contract.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (TxOutFromOutput))
import Cardano.Ledger.Babbage.TxInfo (transTxOutV2)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Hydra.Cardano.Api (
  Tx,
  UTxO,
  serialiseToRawBytesHexText,
  toLedgerTxOut,
  toPlutusTxOut,
 )
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head (verifySnapshotSignature)
import Hydra.Contract.Head qualified as OnChain
import Hydra.Ledger.Cardano.Evaluate (propTransactionEvaluates)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  Snapshot (..),
  deriveParty,
  hashUTxO,
  headIdToCurrencySymbol,
  partyToChain,
 )
import Hydra.Tx.Contract.Abort (genAbortMutation, healthyAbortTx, propHasCommit, propHasInitial)
import Hydra.Tx.Contract.Close.CloseInitial (genCloseInitialMutation, healthyCloseInitialTx)
import Hydra.Tx.Contract.Close.CloseUnused (genCloseCurrentMutation, healthyCloseCurrentTx)
import Hydra.Tx.Contract.Close.CloseUsed (genCloseOutdatedMutation, healthyCloseOutdatedTx)
import Hydra.Tx.Contract.CollectCom (genCollectComMutation, healthyCollectComTx)
import Hydra.Tx.Contract.Commit (genCommitMutation, healthyCommitTx)
import Hydra.Tx.Contract.Contest.ContestCurrent (genContestMutation, healthyContestTx)
import Hydra.Tx.Contract.Decrement (genDecrementMutation, healthyDecrementTx)
import Hydra.Tx.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Tx.Contract.Init (genInitMutation, healthyInitTx)
import Hydra.Tx.Crypto (aggregate, sign, toPlutusSignatures)
import PlutusLedgerApi.V2 (fromBuiltin, toBuiltin)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (
  genUTxOSized,
  genUTxOWithSimplifiedAddresses,
  shrinkUTxO,
 )
import Test.Hydra.Tx.Mutation (propMutation)
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  forAll,
  forAllBlind,
  forAllShrink,
  property,
  shuffle,
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = parallel $ do
  describe "Signature validator" $ do
    prop "verifies snapshot multi-signature" prop_verifySnapshotSignatures

  describe "TxOut hashing" $ do
    modifyMaxSuccess (const 20) $ do
      prop "hashUTxO == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentOnAndOffChainHashOfTxOuts
      prop "OnChain.hashPreSerializedCommits == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentHashPreSerializedCommits
      prop "does care about ordering of TxOut" prop_hashingCaresAboutOrderingOfTxOuts

  describe "Serializing commits" $
    prop "deserializeCommit . serializeCommit === id" prop_serializingCommitRoundtrip

  describe "Init" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyInitTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyInitTx genInitMutation

  describe "Abort" $ do
    prop "is healthy" $
      conjoin
        [ propTransactionEvaluates healthyAbortTx
        , propHasCommit healthyAbortTx
        , propHasInitial healthyAbortTx
        ]
    prop "does not survive random adversarial mutations" $
      propMutation healthyAbortTx genAbortMutation
  describe "Commit" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCommitTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCommitTx genCommitMutation
  describe "CollectCom" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCollectComTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCollectComTx genCollectComMutation
  describe "Decrement" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyDecrementTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyDecrementTx genDecrementMutation
  describe "CloseInitial" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseInitialTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseInitialTx genCloseInitialMutation
  describe "CloseUnused" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseCurrentTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseCurrentTx genCloseCurrentMutation
  describe "CloseUsed" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseOutdatedTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseOutdatedTx genCloseOutdatedMutation
  describe "ContestCurrent" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyContestTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyContestTx genContestMutation
  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyFanoutTx genFanoutMutation

--
-- Properties
--

prop_serializingCommitRoundtrip :: Property
prop_serializingCommitRoundtrip =
  forAllBlind (List.head . UTxO.pairs <$> genUTxOSized 1) $ \singleUTxO ->
    let serialized = Commit.serializeCommit singleUTxO
        deserialized = serialized >>= Commit.deserializeCommit (networkIdToNetwork testNetworkId)
     in case deserialized of
          Just actual -> actual === singleUTxO
          Nothing ->
            property False
              & counterexample "roundtrip returned Nothing"
              & counterexample ("Serialized: " <> show serialized)
              & counterexample ("Deserialized: " <> show deserialized)

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let plutusTxOuts =
          rights $
            zipWith
              (\ix o -> transTxOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
              [0 ..]
              txOuts
        txOuts = map snd . sortOn fst $ UTxO.pairs utxo
     in (hashUTxO @Tx utxo === fromBuiltin (OnChain.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show txOuts)

prop_consistentHashPreSerializedCommits :: Property
prop_consistentHashPreSerializedCommits =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let unsortedUTxOPairs = UTxO.pairs utxo
        toFanoutTxOuts = mapMaybe (toPlutusTxOut . snd) $ sortOn fst unsortedUTxOPairs
        serializedCommits = mapMaybe Commit.serializeCommit unsortedUTxOPairs
        hashedCommits = OnChain.hashPreSerializedCommits serializedCommits
        hashedTxOuts = OnChain.hashTxOuts toFanoutTxOuts
     in hashedCommits
          === hashedTxOuts
          & counterexample ("Hashed commits: " <> decodeUtf8 (Base16.encode $ fromBuiltin hashedCommits))
          & counterexample ("Hashed txOuts: " <> decodeUtf8 (Base16.encode $ fromBuiltin hashedTxOuts))
          & counterexample ("Serialized commits: " <> show serializedCommits)
          & counterexample ("To fanout txOuts: " <> show toFanoutTxOuts)

prop_hashingCaresAboutOrderingOfTxOuts :: Property
prop_hashingCaresAboutOrderingOfTxOuts =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    (length utxo > 1) ==>
      let plutusTxOuts =
            rights $
              zipWith
                (\ix o -> transTxOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
                [0 ..]
                txOuts
          txOuts = snd <$> UTxO.pairs utxo
       in forAll (shuffle plutusTxOuts) $ \shuffledTxOuts ->
            (shuffledTxOuts /= plutusTxOuts) ==>
              let hashed = OnChain.hashTxOuts plutusTxOuts
                  hashShuffled = OnChain.hashTxOuts shuffledTxOuts
               in (hashed =/= hashShuffled)
                    & counterexample ("Plutus: " <> show plutusTxOuts)
                    & counterexample ("Shuffled: " <> show shuffledTxOuts)

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{headId, number, utxo, utxoToDecommit, version} :: Snapshot Tx) ->
    forAll arbitrary $ \sks ->
      let parties = deriveParty <$> sks
          onChainParties = partyToChain <$> parties
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger number
          snapshotVersion = toInteger version
          utxoHash = toBuiltin $ hashUTxO utxo
          utxoToDecommitHash = toBuiltin . hashUTxO <$> utxoToDecommit
       in verifySnapshotSignature onChainParties (headIdToCurrencySymbol headId, snapshotVersion, snapshotNumber, utxoHash, utxoToDecommitHash) signatures
            & counterexample ("headId: " <> toString (serialiseToRawBytesHexText headId))
            & counterexample ("version: " <> show snapshotVersion)
            & counterexample ("number: " <> show snapshotNumber)
            & counterexample ("utxoHash: " <> show utxoHash)
            & counterexample ("utxoToDecommitHash: " <> show utxoToDecommitHash)
            & counterexample ("off-chain message: " <> show (Base16.encode $ getSignableRepresentation snapshot))
            & counterexample ("signatures: " <> show signatures)