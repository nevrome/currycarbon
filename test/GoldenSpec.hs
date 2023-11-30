module GoldenSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec          (Spec, describe, it, shouldReturn, shouldBe)

spec :: Spec
spec = goldenTest

goldenTest :: Spec
goldenTest =

  describe "currycarbon cli test" $ do

    let stdout_stderr_tests = [
              "single_radiocarbon_date"
            ]

    runStdoutStderrTests stdout_stderr_tests

    let file_output_tests = [
              ("density_file",       "/tmp/currycarbon_test_density_file.tsv")
            , ("hdr_file",           "/tmp/currycarbon_test_hdr_file.tsv")
            , ("samples_file",       "/tmp/currycarbon_test_samples_file.tsv")
            , ("cal_curve_seg_file", "/tmp/currycarbon_test_cal_curve_seg_file.tsv")
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
            outExpected <- readFile $ "test/golden/data/" ++ test ++ ".out"
            liftA2 (++) (hGetContents err) (hGetContents out) `shouldReturn` outExpected

runFileOutputTests :: [(String, FilePath)] -> Spec
runFileOutputTests tests = do
    forM_ tests $ \(test, path) -> do
        it (test ++ " should produce the correct output file") $ do
            let cp = (shell ("bash " ++ test ++ ".sh")) {
                  cwd = Just "test/golden",
                  std_out = CreatePipe,
                  std_err = CreatePipe
                }
            _ <- createProcess cp
            outActually <- readFile path
            outExpected <- readFile ("test/golden/data/" ++ test ++ ".tsv")
            outActually `shouldBe` outExpected