module GoldenSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec          (Spec, describe, it, shouldReturn)

spec :: Spec
spec = goldenTest

goldenTest :: Spec
goldenTest =

  describe "currycarbon cli test" $ do

    let stdout_stderr_tests = [
            "single_radiocarbon_date"
            ]

    runStdoutStderrTests stdout_stderr_tests

    --let file_output_tests = [
    --        ()
    --        ]

    --runTestScripts "basic" basic_tests

    where
        runStdoutStderrTests :: [String] -> Spec
        runStdoutStderrTests tests = do
            forM_ tests $ \test -> do
                it ("should be executed correctly: " ++ test) $ do
                    let cp = (shell ("bash " ++ test ++ ".sh")) {
                          cwd = Just $ "test/golden",
                          std_out = CreatePipe,
                          std_err = CreatePipe
                        }
                    (_, Just out, Just err, _) <- createProcess cp
                    hSetBuffering out NoBuffering
                    hSetBuffering err NoBuffering
                    outExpected <- readFile $ "test/golden/data/" ++ test ++ ".out"
                    liftA2 (++) (hGetContents err) (hGetContents out) `shouldReturn` outExpected