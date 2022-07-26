# Currycarbon

**A Haskell library and command line tool for radiocarbon calibration**

CAA 2022

S05 - Little minions

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products
  - Reasonably fast

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products
  - Reasonably fast
- A didactic exercise
  - Understanding by implementation
  - Calibration as vector-matrix multiplication

---

## What is currycarbon

- A simple radiocarbon calibration software library in and for Haskell
  - Intercept calibration
  - Sums and products
  - Reasonably fast
- A didactic exercise
  - Understanding by implementation
  - Calibration as vector-matrix multiplication
- A command line tool for your quick calibration needs

```bash
currycarbon "5000,30"
```

---

## Why yet another calibration library?

<!--I wanted to understand intercept calibration -->
<!--Martin Hinz: https://www.martinhinz.info/jekyll/update/blog/2016/06/03/simple_calibration.html-->

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
│.│                   │                       .#. │
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

I wanted to write a Haskell library

```
 .,,,,,,,  *******                      
   ,,,,,,,   *******                        - Haskell is a purely functional, lazy and
     ,,,,,,,  *******                         statically typed programming language
      ,,,,,,,,  *******  ///////////////    - Looks and feels very different from C, R, Java, ...
        ,,,,,,,   *******  /////////////    - Makes you feel powerful
          ,,,,,,,  *******                    - Pure: Side effects are well contained
        ,,,,,,,   **********  //////////      - Lazy: It evaluates only what it really must
      .,,,,,,,  **************  ////////      - Types: Most errors are caught at compile time
     ,,,,,,,  *******   *******             
   ,,,,,,,  ,*******      *******           - Well suitable for command line applications      
 .,,,,,,,  *******          *******  
```

We wrote the tools trident and xerxes of our aDNA genotype data management framework Poseidon with Haskell: https://poseidon-framework.github.io

---

## How does it work?

dsdads

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
tail --lines 15 results.txt
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
currycarbon "Sample1,5000,30+Sample2,3900,30+Sample3,2700,40"
```

---

## Features: Sums and products

```bash
currycarbon "Sample1,5000,30+Sample2,3900,30*Sample3,2700,40"
```

---

## Conclusion

- If you want to understand an algorithm, implement it
- If you need calibration in your Haskell software, use currycarbon
- If you want to quickly calibrate dates without opening OxCal or R, download currycarbon

at https://github.com/nevrome/currycarbon
