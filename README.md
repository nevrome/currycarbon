[![GitHub Workflow Status](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml/badge.svg)](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml)
[![Coverage Status](https://img.shields.io/codecov/c/github/nevrome/currycarbon/master.svg)](https://codecov.io/github/nevrome/currycarbon?branch=master)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/nevrome/currycarbon?include_prereleases) 
![GitHub all releases](https://img.shields.io/github/downloads/nevrome/currycarbon/total)](https://github.com/nevrome/currycarbon/releases)

# currycarbon

Radiocarbon calibration module written in and for [Haskell](https://www.haskell.org). Comes with a small CLI app to run calibration on the command line.

### Library

The Haskell library is available on Hackage [here](https://hackage.haskell.org/package/currycarbon) and on Stackage [here](https://www.stackage.org/package/currycarbon).

### CLI app

For stable release versions we automatically prepare statically built binaries that can be downloaded and run directly.

- [📥 Linux](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Linux)
- [📥 macOS (ARM64)](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-macOS-ARM64)
- [📥 macOS (X64)](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-macOS-X64)
- [📥 Windows](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Windows.exe)

So in Linux you can run the following commands to get started:

```bash
# download the current stable release binary
wget https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Linux
# make it executable
chmod +x currycarbon-Linux
# test it
./currycarbon-Linux "Sample1,4990,30"
```

```
currycarbon v0.4.0.2 (UTF-8)
Method: Bchron {distribution = StudentTDist {ndf = 100.0}}
Curve: IntCal20
Calibrating...
CalEXPR: [1] Sample1:4990±30BP
Calibrated: 3936BC >> 3794BC > 3757BC < 3662BC << 3654BC
1-sigma: 3794-3707BC, 3666-3662BC
2-sigma: 3936-3874BC, 3804-3697BC, 3684-3654BC

    BP
  5120 ┤ ┆
       │  ┆┆           ┆┆┆┆┆┆┆┆┆┆
       │    ┆┆┆┆┆┆┆   ┆          ┆┆
       │ ┄┄┄┄┄┄┄┄┄┄┆┆┆┄┄┄┄┄┄┄┄┄┄┄┄┄┆┆┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
  4990 ┤ ┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┆┆┆┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅┅
       │ ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┆┆┆┆┆┆┆┆┆┆┄┄┄┄┄┄┄┆┄┄┄
       │                                          ┆┆  ┆┆┆ ┆
       │                                            ┆┆     ┆
  4870 ┤                                                    ┆
                                      ▁▁▁    ▁▁▁▁
                                    ▁▁▒▒▒▁▁▁▁▒▒▒▒▁
                                    ▒▒▒▒▒▒▒▒▒▒▒▒▒▒       ▁
                   ▁▁              ▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁     ▁▒
                 ▁▁▒▒▁             ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒    ▁▒▒▁
            ▁▁▁▁▁▒▒▒▒▒▁          ▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁▁▁▁▒▒▒▒▁
         ▁▁▁▒▒▒▒▒▒▒▒▒▒▒▁▁▁▁▁▁▁▁▁▁▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▁
 -3950 ┄─────────┬───────────────┬────────────────┬─────────┄ -3640
    BC      >                      >     ^               < <  BC
                                   ────────────────      ─
            ───────────           ──────────────────  ──────
Done.
```

```
Usage: currycarbon [--version] [CalEXPRs] [-i|--inputFile FILE]
                   [--calCurve IntCal20 | SHCal20 | Marine20 | FILE]
                   [--method DSL] [--allowOutside] [--noInterpolation]
                   [--noTrimCalCurve] [--noTrimOutCalPDF] [-q|--quiet]
                   [--basicFile FILE] [--densityFile FILE] [--hdrFile FILE]
                   [[--seed INT] (-n|--nrSamples INT) --samplesFile FILE]
                   [--calCurveSegFile FILE] [--calCurveMatFile FILE]

  currycarbon calibrates radiocarbon dates

Available options:
  -h,--help                Show this help text
  --version                Show version
  CalEXPRs                 ---
                           A string to specify "calibration expressions", so
                           small chronological models for individual events.
                           These can include uncalibrated radiocarbon ages,
                           uniform age ranges and operations to combine the
                           resulting age probability distribution as sums or
                           products.
                           The expression language includes the following
                           functions:

                           - calExpr(id = STRING, expr = EXPR)
                           - uncalC14(id = STRING, yearBP = INT, sigma = INT)
                           - rangeBP(id = STRING, start = INT, stop = INT)
                           - rangeBCAD(id = STRING, start = INT, stop = INT)
                           - sum(a = EXPR, b = EXPR)
                           - product(a = EXPR, b = EXPR)

                           The order of arguments is fixed, but the argument
                           names '<arg> =' can be left out. The 'id' arguments
                           are optional. Some functions can be shortened with
                           syntactic sugar:

                           - calExpr(STRING, EXPR) -> id: EXPR
                           - uncalC14(STRING, INT, INT) -> STRING,INT,INT
                           - sum(EXPR, EXPR) -> EXPR + EXPR
                           - product(EXPR, EXPR) -> EXPR * EXPR

                           Parentheses '()' can be used to specify the
                           evaluation order within an expression. Multiple
                           expressions can be chained, separated by ';'.

                           Examples:
                           1. Calibrate a single radiocarbon date with a mean
                           age BP and a one sigma standard deviation:
                           "3000,30" or "uncalC14(yearBP = 3000, sigma = 30)"
                           2. Calibrate two radiocarbon dates and sum them:
                           "(3000,30) + (3100,40)" or
                           "sum(uncalC14(3000,30), uncalC14(3100,40))"
                           3. Compile a complex, named expression:
                           "Ex3: ((3000,30) + (3100,40)) * rangeBP(3200,3000)"
                           ---
  -i,--inputFile FILE      A file with a list of calibration expressions.
                           Formatted just as CalEXPRs, but with a new line for
                           each input expression. CalEXPRs and --inputFile can
                           be combined and you can provide multiple instances of
                           --inputFile. Note that syntactic sugar allows to read
                           simple radiocarbon dates from a headless .csv file
                           with one sample per row: <sample name>,<mean age
                           BP>,<one sigma standard deviation>.
  --calCurve IntCal20 | SHCal20 | Marine20 | FILE
                           Either one of the included calibration curves, or a
                           file path to an calibration curve file in '.14c'
                           format. The calibration curve will be read and used
                           for calibration. (default: IntCal20)
  --method DSL             The calibration algorithm that should be used:
                           '<Method>,<Distribution>,<NumberOfDegreesOfFreedom>'.
                           The default setting is equivalent to
                           "Bchron,StudentT,100" which copies the algorithm
                           implemented in the Bchron R package. For the Bchron
                           algorithm with a normal distribution
                           ("Bchron,Normal") the degrees of freedom argument is
                           not relevant
                           Alternatively we implemented "MatrixMult", which
                           comes without further arguments.
  --allowOutside           Allow calibrations to run outside the range of the
                           calibration curve.
  --noInterpolation        Do not interpolate the calibration curve.
  --noTrimCalCurve         Do not trim the calibration curve before the
                           calibration. If a probability distribution over the
                           entire range of the calibration curve is needed. See
                           also --noTrimOutCalPDF.
  --noTrimOutCalPDF        Do not trim the output CalPDF. If an untrimmed
                           probability distribution is needed. See also
                           --noTrimCalCurve.
  -q,--quiet               Suppress the printing of calibration results to the
                           command line.
  --basicFile FILE         Path to an output file to store basic, per-expression
                           output: The minimum start and maximum end of the high
                           probability density regions and the median age.
  --densityFile FILE       Path to an output file to store output densities per
                           CalEXPR and calender year.
  --hdrFile FILE           Path to an output file to store the high probability
                           density regions for each CalEXPR.
  --seed INT               Seed for the random number generator for age
                           sampling. The default causes currycarbon to fall back
                           to a random seed. (default: Nothing)
  -n,--nrSamples INT       Number of age samples to draw per CalEXPR.
  --samplesFile FILE       Path to an output file to store age samples for each
                           CalEXPR.
  --calCurveSegFile FILE   Path to an output file to store the relevant,
                           interpolated calibration curve segment for the first
                           (!) input date. This option as well as
                           --calCurveMatFile are meant for debugging.
  --calCurveMatFile FILE   Path to an output file which stores the relevant,
                           interpolated calibration curve segment for the first
                           (!) input date in a wide matrix format.
```

### For developers who want to edit the code

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

#### Running the golden tests

Because the golden tests can not run on stackage as they are set up now (see the discussion [here](https://github.com/nevrome/currycarbon/issues/17)) I hid them behind an environment variable. You can run them with

```bash
CURRY_RUN_GOLDEN=true stack test --pedantic
```

Just calling `stack test --pedantic` without this variable will skip any test with the pattern `"Golden"` in their descriptors.

#### Upload to Hackage

See the documentation here:
- https://hackage.haskell.org/upload
- https://docs.haskellstack.org/en/stable/commands/upload_command

`stack` allows to upload a release candidate with

```
stack upload . --test-tarball --candidate --no-save-hackage-creds
```

using my Hackage credentials. It can then be published at https://hackage.haskell.org/package/currycarbon/candidates

The building of the haddock documentation can be tested with

```
stack haddock --haddock-for-hackage
```

#### Preparing a new stable release

The Github Actions script in `.github/workflows/release.yml` registers a new draft release and automatically builds and uploads currycarbon binaries when a new Git tag with the prefix `v*` is pushed. 

```bash
# locally register a new tag (e.g. 0.3.1)
git tag -a v0.3.1 -m "see CHANGELOG.md"
# push tag
git push origin v0.3.1
```

In case of a failing build delete the tag and the release draft on Github and then delete the tag locally with

```bash
git tag -d v0.3.1
```

before rerunning the procedure above.

#### Profiling

```
stack build --profile
stack exec --profile -- currycarbon "1000,200;2000,200;3000,200;4000,200;5000,200;6000,200;7000,200;8000,200" -q --densityFile /dev/null +RTS -p
stack exec -- currycarbon "1000,200;2000,200;3000,200;4000,200;5000,200;6000,200;7000,200;8000,200" -q --densityFile /dev/null +RTS -s
```
