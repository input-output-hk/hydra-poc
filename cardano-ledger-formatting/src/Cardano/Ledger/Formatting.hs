module Cardano.Ledger.Formatting where

import Formatting (Format, shown, int, (%), accessed, text)
import Cardano.Ledger.Api (PlutusPurpose, AsIx, Data, Tx)
import Cardano.Ledger.Plutus.ExUnits (ExUnits(..))

redeemerf :: Show (PlutusPurpose AsIx era)
          => Format r ((PlutusPurpose AsIx era, (Data era, ExUnits)) -> r)
redeemerf = accessed fst plutusPurposef <> accessed (fst . snd) dataf <> accessed (snd . snd) exUnitsf

plutusPurposef :: Show (PlutusPurpose AsIx era)
               => Format r (PlutusPurpose AsIx era -> r)
plutusPurposef = shown

exUnitsf :: Format r (ExUnits -> r)
exUnitsf = "{ cpu = " % (accessed exUnitsSteps int) 
        <> ", mem = " % (accessed exUnitsMem int) % " }"

dataf :: Format r (Data era -> r)
dataf = accessed (const "Not Yet Implemented") text

txf :: Format r (Tx era -> r)
txf = accessed (const "Not Yet Implemented") text
