import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Currycarbon/Calibration/Utils.hs"]