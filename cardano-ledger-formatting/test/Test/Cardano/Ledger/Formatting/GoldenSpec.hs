module Test.Cardano.Ledger.Formatting.GoldenSpec where

import Data.Text.Lazy (Text)
import Test.Hspec (describe, Spec, it, shouldBe)
import Formatting (format)
import Cardano.Ledger.Formatting (txf)
import Cardano.Ledger.Api (mkBasicTx, mkBasicTxBody, ConwayEra, StandardCrypto)

goldenSpec :: Spec
goldenSpec = describe "Tx Formatting" $ do
    it "Tx" $ do
      let tx = mkBasicTx @(ConwayEra StandardCrypto) $ mkBasicTxBody
      format txf tx `shouldBe` ("Not Yet Implemented" :: Text)
