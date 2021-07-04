[![GitHub Workflow Status](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml/badge.svg)](https://github.com/nevrome/currycarbon/actions/workflows/normalCheck.yml)

# currycarbon

An experimental radiocarbon calibration module written in and for [Haskell](https://www.haskell.org/). Comes with a small executable to run calibration on the commandline.

I'm developing this for my own needs and will add features accordingly. The interface is highly fluid and prone to change for the time being.

### Install

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

## For developers

Profiling: 

```
stack build --profile
stack exec --profile -- currycarbon calibrate "5000+200;5000+200;5000+200;5000+200;5000+200;5000+200;5000+200;5000+200" -q +RTS -p
```
