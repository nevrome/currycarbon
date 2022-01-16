import Test.DocTest

import Currycarbon.CalCurves.Intcal20

main :: IO ()
main = doctest [
      "-isrc"
    , "src/Currycarbon/Calibration/Bchron.hs"
    , "src/Currycarbon/Calibration/Calibration.hs"
    , "src/Currycarbon/Calibration/Utils.hs"
    ]