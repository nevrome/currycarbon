[![GitHub Workflow Status](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml/badge.svg)](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/nevrome/currycarbon?include_prereleases) 
![GitHub all releases](https://img.shields.io/github/downloads/nevrome/currycarbon/total)](https://github.com/nevrome/currycarbon/releases)

# currycarbon

An experimental radiocarbon calibration module written in and for [Haskell](https://www.haskell.org). Comes with a small CLI app to run calibration on the commandline.

### Library

The Haskell library is not on Hackage yet. Some documentation for the dev version can be found here: https://nevrome.github.io/currycarbon

### CLI app

For stable release versions we automatically prepare binaries that can be downloaded and run.

You can download them here: [ [Linux 📥](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Linux) | [macOS 📥](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-macOS) | [Windows 📥](https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Windows.exe) ]. Older release versions are available [here](https://github.com/nevrome/currycarbon/releases).

So in Linux you can run the following commands to get started:

```bash
# download the current stable release binary
wget https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Linux
# make it executable
chmod +x currycarbon-Linux
# test it
./currycarbon-Linux "Sample1,4000,30"
```

```
Usage: currycarbon [--version] [DATES] [-i|--inputFile ARG] 
                   [--calibrationCurveFile ARG] [--method ARG] [--allowOutside] 
                   [--noInterpolation] [-q|--quiet] [--densityFile ARG] 
                   [--hdrFile ARG] [--calCurveSegmentFile ARG] 
                   [--calCurveMatrixFile ARG]
  Simple intercept calibration for one or multiple radiocarbon dates

Available options:
  -h,--help                Show this help text
  --version                Show version
  DATES                    A string with one or multiple uncalibrated dates of
                           the form "<sample name>,<mean age BP>,<one sigma
                           standard deviation>;..." where <sample name> is
                           optional. So for example
                           "S1,4000,50;3000,25;S3,1000,20".
  -i,--inputFile ARG       A file with a list of uncalibrated dates. Formated
                           just as DATES, but with a new line for each input
                           date. DATES and --uncalFile can be combined and you
                           can provide multiple instances of --uncalFile
  --calibrationCurveFile ARG
                           Path to an calibration curve file in .14c format. The
                           calibration curve will be read and used for
                           calibration. If no file is provided, currycarbon will
                           use the intcal20 curve.
  --method ARG             The calibration algorithm that should be used:
                           "<Method>,<Distribution>,<NumberOfDegreesOfFreedom>".
                           The default setting is equivalent to
                           "Bchron,StudentT,100" which copies the algorithm
                           implemented in the Bchron R package. Alternatively we
                           implemented "MatrixMult", which comes without further
                           arguments. For the Bchron algorithm with a normal
                           distribution ("Bchron,Normal") the degrees of freedom
                           argument is not relevant
  --allowOutside           Allow calibrations to run outside the range of the
                           calibration curve
  --noInterpolation        Don't interpolate the calibration curve
  -q,--quiet               Suppress the printing of calibration results to the
                           command line
  --densityFile ARG        Path to an output file which stores output densities
                           per sample and calender year
  --hdrFile ARG            Path to an output file which stores the high
                           probability density regions for each sample
  --calCurveSegmentFile ARG
                           Path to an output file which stores the relevant,
                           interpolated calibration curve segment for the first
                           (!) input date in a long format. This option as well
                           as --calCurveMatrixFile are mostly meant for
                           debugging
  --calCurveMatrixFile ARG Path to an output file which stores the relevant,
                           interpolated calibration curve segment for the first
                           (!) input date in a wide matrix format
```

### For developers

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

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
