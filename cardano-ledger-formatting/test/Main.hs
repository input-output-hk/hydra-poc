module Main where

import Test.Hspec
import Test.Cardano.Ledger.Formatting.GoldenSpec (goldenSpec)

main :: IO ()
main = hspec goldenSpec
