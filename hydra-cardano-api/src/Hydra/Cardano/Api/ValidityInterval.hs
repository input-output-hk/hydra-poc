{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ValidityInterval where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Allegra.Scripts qualified as Ledger
import Cardano.Api.Eon.AllegraEraOnwards (AllegraEraOnwards(..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..) ,maybeToStrictMaybe)
import Test.QuickCheck (oneof)

toLedgerValidityInterval ::
  (TxValidityLowerBound era, TxValidityUpperBound era) ->
  Ledger.ValidityInterval
toLedgerValidityInterval (lowerBound, upperBound) =
  Ledger.ValidityInterval
    { Ledger.invalidBefore =
        case lowerBound of
          TxValidityNoLowerBound -> SNothing
          TxValidityLowerBound _ s -> SJust s
    , Ledger.invalidHereafter =
        case upperBound of
          TxValidityNoUpperBound _ -> SNothing
          TxValidityUpperBound _ s -> maybeToStrictMaybe s
    }
fromLedgerValidityInterval ::
  Ledger.ValidityInterval ->
  (TxValidityLowerBound Era, TxValidityUpperBound Era)
fromLedgerValidityInterval validityInterval =
  let Ledger.ValidityInterval{Ledger.invalidBefore = invalidBefore, Ledger.invalidHereafter = invalidHereAfter} = validityInterval
      lowerBound = case invalidBefore of
        SNothing -> TxValidityNoLowerBound
        SJust s -> TxValidityLowerBound ValidityLowerBoundInBabbageEra s
      upperBound = case invalidHereAfter of
        SNothing -> TxValidityNoUpperBound ByronEraOnlyByron
        SJust s -> TxValidityUpperBound ShelleyBasedEraBabbage s
   in (lowerBound, upperBound)

instance Arbitrary (TxValidityLowerBound Era) where
  arbitrary =
    oneof
      [ pure TxValidityNoLowerBound
      , TxValidityLowerBound _ . SlotNo <$> arbitrary
      ]

instance Arbitrary (TxValidityUpperBound Era) where
  arbitrary =
    oneof
      [ pure $ TxValidityNoUpperBound _
      , TxValidityUpperBound ShelleyBasedEraBabbage . SlotNo <$> arbitrary
      ]
