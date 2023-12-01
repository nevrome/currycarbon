library(ggplot2)
library(magrittr)

#### normalization experiments ####

# normalization function
nor <- function(x) { x/sum(x) }

# test density vectors, all normalized
a <- c(0.1,0.7,0.2)
b <- c(0.3,0.4,0.3)
c <- c(0.1,0.1,0.8)

# sum -> all of these differ
nor(a+b+c)
nor(nor(a+b)+c)
nor(a+nor(b+c))

# product -> all are the same
nor(a*b*c)
nor(nor(a*b)*c)
nor(a*nor(b*c))

#### test simple sum ####

system("currycarbon \"rangeBP(3000,2800)\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFRange <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())
system("currycarbon \"3000,30\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFC14a <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFRange,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "blue"
  ) +
  geom_line(
    data = calPDFC14a,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "red"
  )

system("currycarbon \"(3000,30) + rangeBP(3000,2800)\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFSum <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFSum,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "purple"
  )

#### test two sums ####

# this must not the same as the previous test
system("currycarbon \"((3000,30) + (3000,30)) + rangeBP(3000,2800)\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFSum <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFSum,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "orange"
  )

#### test more complex arrangements ####

system("currycarbon \"2900,100\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFC14b <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFSum,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "purple"
  ) +
  geom_line(
    data = calPDFC14b,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "darkred"
  )

system("currycarbon \"(((3000,30) + (3000,30)) + rangeBP(3000,2800)) * (2900,100)\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFProd <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFProd,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "darkgreen"
  )


