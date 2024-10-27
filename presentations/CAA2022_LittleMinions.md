# Currycarbon

**A Haskell library and command line tool for radiocarbon calibration**

CAA 2022

S05 - Little minions

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products of calibrated dates
  - Reasonably fast

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products of calibrated dates
  - Reasonably fast
- A didactic exercise
  - Calibration as vector-matrix multiplication
  - Understanding by implementation

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products of calibrated dates
  - Reasonably fast
- A didactic exercise
  - Calibration as vector-matrix multiplication
  - Understanding by implementation
- A command line tool for your quick calibration needs

```bash
currycarbon "5000,30"
```

---

## Why yet another calibration library?

1. I wanted to understand intercept calibration

---

## Why yet another calibration library?

1. I wanted to understand intercept calibration

```
uncal date
┌─┐
│ │                                                    - The uncalibrated date as a vector
│.│                                                      of probability densities
│.│
│#│
│#│
│#│
│.│
│ │
└─┘
```

---

## Why yet another calibration library?

1. I wanted to understand intercept calibration

```
uncal date            calibration curve
┌─┐                   ┌───────────────────────────┐
│ │                   │.#.                        │    - The uncalibrated date as a vector
│.│                   │ .#...                     │      of probability densities
│.│                   │ ..###..        .          │    - The calibration curve as a matrix  
│#│                   │    ..##..    .###.        │      of probability densities
│#│         *         │       ..#...##...#.       │
│#│                   │          #.#.   ..#..     │
│.│                   │           #        .###.  │
│ │                   │                       ..##│
└─┘                   └───────────────────────────┘
```

---

## Why yet another calibration library?

1. I wanted to understand intercept calibration

```
uncal date            calibration curve
┌─┐                   ┌───────────────────────────┐
│ │                   │.#.                        │    - The uncalibrated date as a vector
│.│                   │ .#...                     │      of probability densities
│.│                   │ ..###..        .          │    - The calibration curve as a matrix  
│#│                   │    ..##..    .###.        │      of probability densities
│#│         *         │       ..#...##...#.       │    - Column-wise multiplication to
│#│                   │          #.#.   ..#..     │      derive the post-calibration
│.│                   │           #        .###.  │      probability distribution
│ │                   │                       ..##│
└─┘                   └───────────────────────────┘
                                    =
                      ┌───────────────────────────┐ post-calibration
                      │      ***   ***** ***      │ probability distribution
                      │    **'''***'''''*'''**    │
                      │****'''''''''''''''''''****│ 
                      └───────────────────────────┘
```

---

## Why yet another calibration library?

1. I wanted to understand intercept calibration

```
uncal date            calibration curve
┌─┐                   ┌───────────────────────────┐
│ │                   │.#.                        │    - The uncalibrated date as a vector
│.│                   │ .#...                     │      of probability densities
│.│                   │ ..###..        .          │    - The calibration curve as a matrix  
│#│                   │    ..##..    .###.        │      of probability densities
│#│         *         │       ..#...##...#.       │    - Column-wise multiplication to
│#│                   │          #.#.   ..#..     │      derive the post-calibration
│.│                   │           #        .###.  │      probability distribution
│ │                   │                       ..##│
└─┘                   └───────────────────────────┘
                                    =
                      ┌───────────────────────────┐ post-calibration
                      │      ***   ***** ***      │ probability distribution
                      │    **'''***'''''*'''**    │
                      │****'''''''''''''''''''****│ 
                      └───────────────────────────┘
```

- Martin Hinz: https://www.martinhinz.info/jekyll/update/blog/2016/06/03/simple_calibration.html
- Andrew Parnell: https://github.com/andrewcparnell/Bchron

---

## Why yet another calibration library?

2. I wanted to write a Haskell library

```
 .,,,,,,,  *******                      
   ,,,,,,,   *******                        - Haskell is a purely functional programming language
     ,,,,,,,  *******                       
      ,,,,,,,,  *******  ///////////////    - Looks and feels different from C, R, Java, ...
        ,,,,,,,   *******  /////////////    
          ,,,,,,,  *******                    - Pure: Side effects are well contained
        ,,,,,,,   **********  //////////      - Statically typed: Most errors are caught at compile time
      .,,,,,,,  **************  ////////      - Lazy: It evaluates only what it really must
     ,,,,,,,  *******   *******             
   ,,,,,,,  ,*******      *******           - Well suitable for command line applications      
 .,,,,,,,  *******          *******         - We're developing tools for aDNA in it
```

---

## Why yet another calibration library?

2. I wanted to write a Haskell library

```
 .,,,,,,,  *******                      
   ,,,,,,,   *******                        - Haskell is a purely functional programming language
     ,,,,,,,  *******                       
      ,,,,,,,,  *******  ///////////////    - Looks and feels different from C, R, Java, ...
        ,,,,,,,   *******  /////////////    
          ,,,,,,,  *******                    - Pure: Side effects are well contained
        ,,,,,,,   **********  //////////      - Statically typed: Most errors are caught at compile time
      .,,,,,,,  **************  ////////      - Lazy: It evaluates only what it really must
     ,,,,,,,  *******   *******             
   ,,,,,,,  ,*******      *******           - Well suitable for command line applications      
 .,,,,,,,  *******          *******         - We're developing tools for aDNA in it
```

I needed a calibration library in Haskell, but there was none

---

## How does it work?

Simple calibration

```Haskell
calibrateDate :: CalibrateDatesConf     -- ^ configuration options
                 -> CalCurveBP          -- ^ calibration curve
                 -> UncalC14            -- ^ uncalibrated date
                 -> Either CurrycarbonException CalPDF

data CalPDF = CalPDF {
      _calPDFid :: String               -- | Identifier, e.g. a lab number
    , _calPDFCals :: VU.Vector YearBCAD -- | years calBC/AD
    , _calPDFDens :: VU.Vector Double    -- | probability for each year
    }
```

---

## How does it work?

Simple calibration

```Haskell
calibrateDate :: CalibrateDatesConf     -- ^ configuration options
                 -> CalCurveBP          -- ^ calibration curve
                 -> UncalC14            -- ^ uncalibrated date
                 -> Either CurrycarbonException CalPDF

data CalPDF = CalPDF {
      _calPDFid :: String               -- | Identifier, e.g. a lab number
    , _calPDFCals :: VU.Vector YearBCAD -- | years calBC/AD
    , _calPDFDens :: VU.Vector Double    -- | probability for each year
    }
```

Sum- and Product calibration

```Haskell
data CalExpr =
      UnCalDate UncalC14                -- | uncalibrated date
    | CalDate CalPDF                    -- | calibrated date
    | SumCal CalExpr CalExpr            -- | sum of two CalExpressions
    | ProductCal CalExpr CalExpr        -- | product of two CalExpressions

evalCalExpr :: CalibrateDatesConf -> CalCurveBP -> CalExpr -> Either CurrycarbonException CalPDF
```

---

## How can you use it?

1. Download a statically compiled exectuable for your OS here: https://github.com/nevrome/currycarbon
2. Give it execution permission `chmod +x`
3. Have fun!

```bash
wget https://github.com/nevrome/currycarbon/releases/latest/download/currycarbon-Linux -O currycarbon-Linux
chmod +x currycarbon-Linux
./currycarbon-Linux "Sample1,5000,30"
```

---

## Features: Calibrating multiple dates

```bash
currycarbon "Sample1,5000,30;Sample2,3900,30;Sample3,2700,40" > results.txt
tail --lines 30 results.txt
```

---

## Features: Post-calibration probability distributions

```bash
currycarbon "Sample1,5000,30;Sample2,3900,30;Sample3,2700,40" -q --densityFile results.txt
head --lines 15 results.txt | column -s "," -t
```

---

## Features: High density regions

```bash
currycarbon "Sample1,5000,30;Sample2,3900,30;Sample3,2700,40" -q --hdrFile results.txt
column -s "," -t results.txt
```

---

## Features: Sum calibration

```bash
currycarbon "Sample1,5000,30+Sample2,4900,30+Sample3,4800,40"
```

---

## Features: Product calibration ("combine")

```bash
currycarbon "Sample1,5000,30*Sample2,4900,30*Sample3,4800,40"
```

---

## Conclusion

- If you want to understand an algorithm, implement it
- If you need calibration in your Haskell software, use currycarbon
- If you want to quickly calibrate dates without opening OxCal or R, download currycarbon

at https://github.com/nevrome/currycarbon
