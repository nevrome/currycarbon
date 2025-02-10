module GoldenSpec (spec) where

import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec     (Spec, describe, it, shouldBe)

spec :: Spec
spec = goldenTest

goldenTest :: Spec
goldenTest =

  describe "currycarbon cli test" $ do

    let stdout_stderr_tests = [
              "single_radiocarbon_date"
            , "tricky_expressions"
            ]

    runStdoutStderrTests stdout_stderr_tests

    let file_output_tests = [
              "density_file"
            , "hdr_file"
            , "samples_file"
            , "cal_curve_seg_file"
            ]

    runFileOutputTests file_output_tests

runStdoutStderrTests :: [String] -> Spec
runStdoutStderrTests tests = do
    forM_ tests $ \test -> do
        it (test ++ " should yield the correct stdout stderr output") $ do
            let cp = (shell ("bash " ++ test ++ ".sh")) {
                  cwd = Just "test/golden",
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
            (_, Just out, Just err, _) <- createProcess cp
            hSetBuffering out NoBuffering
            hSetBuffering err NoBuffering
            outActually <- liftA2 (++) (hGetContents err) (hGetContents out)
            outExpected <- readFile $ "test/golden/expected_data/" ++ test ++ ".out"
            outActually `shouldBe` outExpected

runFileOutputTests :: [String] -> Spec
runFileOutputTests tests = do
    forM_ tests $ \(test) -> do
        it (test ++ " should produce the correct output file") $ do
            let cp = (shell ("bash " ++ test ++ ".sh")) {
                  cwd = Just "test/golden",
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
            (_, _, _, exitCode) <- createProcess cp
            _ <- waitForProcess exitCode
            outActually <- readFile ("test/golden/actual_data/" ++ test ++ ".tsv")
            outExpected <- readFile ("test/golden/expected_data/" ++ test ++ ".tsv")
            outActually `shouldBe` outExpected
