hu <- read.csv("~/agora/currycarbon/test.txt", header = F)

plot(
  hu$V1, 
  hu$V2, 
  xlim=c(3000,3500), 
  type = "l"
)

bchron <- Bchron::BchronCalibrate(3000, 25, calCurves = 'intcal13')

plot(
  bchron$Date1$ageGrid,
  bchron$Date1$densities,
  xlim=c(3000,3500),
  type = "l"
)
