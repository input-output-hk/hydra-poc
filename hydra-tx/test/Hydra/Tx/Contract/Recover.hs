module Hydra.Tx.Contract.Recover where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Fixed (Milli)
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX qualified as POSIX
import Hydra.Contract.Deposit (DepositDatum (..), DepositRedeemer (Recover))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Ledger.Cardano.Evaluate (slotLength, systemStart)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx.Deposit (depositTx)
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Utils (extractInlineDatumFromTxOut)
import PlutusLedgerApi.V2 (CurrencySymbol, POSIXTime)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize, genValue)
import Test.Hydra.Tx.Mutation (
  Mutation (ChangeInput, ChangeOutput, ChangeValidityLowerBound),
  SomeMutation (..),
  modifyInlineDatum,
 )
import Test.QuickCheck (elements, oneof, suchThat)

healthyRecoverTx :: (Tx, UTxO)
healthyRecoverTx =
  (tx, lookupUTxO)
 where
  tx =
    recoverTx
      testNetworkId
      headCS
      depositTxIn
      deposits
      recoverDeadline
      recoverSlotNo

  DepositDatum (_, _, deposits) =
    fromJust $ extractInlineDatumFromTxOut @DepositDatum depositTxOut

recoverSlotNo :: SlotNo
recoverSlotNo = SlotNo $ arbitrary `generateWith` 42

recoverDeadline :: POSIXTime
recoverDeadline = posixFromUTCTime depositDeadline

depositDeadline :: UTCTime
depositDeadline =
  slotNoToUTCTime systemStart slotLength (recoverSlotNo - SlotNo 1)

depositTransaction :: Tx
depositTransaction =
  depositTx testNetworkId (mkHeadId headPolicyId) utxoToDeposit depositDeadline

utxoToDeposit :: UTxO
utxoToDeposit = genUTxOAdaOnlyOfSize 1 `generateWith` 42

headCS :: CurrencySymbol
headCS = toPlutusCurrencySymbol testPolicyId

headPolicyId :: PolicyId
headPolicyId =
  case fromPlutusCurrencySymbol headCS of
    Nothing -> error "failed to create headId from provided CurrencySymbol"
    Just policyId -> policyId

lookupUTxO :: UTxO
lookupUTxO = utxoFromTx depositTransaction

depositTxIn :: TxIn
depositTxOut :: TxOut CtxUTxO
(depositTxIn, depositTxOut) = List.head $ UTxO.pairs lookupUTxO

data RecoverMutation
  = -- | Move the deposit deadline further so that the recover lower bound is
    -- not after the deadline
    MutateDepositDeadline
  | -- | Change the recover output so that the datum commit hash does not match
    MutateRecoverOutput
  | -- | Remove the lower bound from the recover transaction
    RemoveTxValidityLowerBound
  deriving stock (Generic, Show, Enum, Bounded)

genRecoverMutation :: (Tx, UTxO) -> Gen SomeMutation
genRecoverMutation (tx, utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode DepositDeadlineNotReached) MutateDepositDeadline <$> do
        -- Could also use depositTxIn/Out directly but this way we can be sure that the depositTxIn/Out are in the UTxO
        let (depositIn, depositOut@(TxOut addr val _ rscript)) = List.head $ UTxO.pairs (resolveInputsUTxO utxo tx)
        let n = POSIX.posixSecondsToUTCTime $ realToFrac $ (arbitrary :: Gen Milli) `generateWith` 42
        let datum =
              txOutDatum $
                flip modifyInlineDatum (toTxContext depositOut) $ \case
                  DepositDatum (headCS', depositDatumDeadline, commits) ->
                    DepositDatum (headCS', depositDatumDeadline + posixFromUTCTime n, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData $ Recover 1)
    , SomeMutation (pure $ toErrorCode IncorrectDepositHash) MutateRecoverOutput <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode DepositNoLowerBoundDefined) RemoveTxValidityLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    ]