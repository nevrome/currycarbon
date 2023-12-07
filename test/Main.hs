module Main where

import qualified Spec
import           System.Environment   (lookupEnv)
import           Test.Hspec.Core.Util
import           Test.Hspec.Runner

main :: IO ()
main = do
    goldenFlag <- maybe False (const True) <$> lookupEnv "CURRY_RUN_GOLDEN"
    let config = if goldenFlag
                 then defaultConfig
                 else defaultConfig {configSkipPredicate = Just $ filterPredicate "Golden"}
    hspecWith config Spec.spec
