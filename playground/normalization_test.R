library(ggplot2)
library(magrittr)

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

system("currycarbon \"((3000,30) + rangeBP(3000,2800)) * (2900,100)\" --densityFile /tmp/currycarbonOutput.tsv -q")
calPDFProd <- readr::read_tsv("/tmp/currycarbonOutput.tsv", col_types = readr::cols())

ggplot() +
  geom_line(
    data = calPDFProd,
    mapping = aes(x = yearBCAD, y = density), 
    linewidth = 1, alpha = 0.5, color = "darkgreen"
  )
