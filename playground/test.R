library(ggplot2)

testdate <- c(5000,50)

system(paste0(
  "currycarbon calibrate \"1:",
  testdate[1], "+", testdate[2],
  "\" --outFile /tmp/currycarbon.txt --explore --exploreDir \"/tmp/currytest\""))

cal_curve_matrix <- as.matrix(read.csv("/tmp/currytest/calCurveMatrix.csv", row.names = 1, header = T))

image(cal_curve_matrix)

test <- readr::read_csv("/tmp/currycarbon.txt")

bchronRaw <- Bchron::BchronCalibrate(
  testdate[1], testdate[2],
  calCurves = 'intcal20'
)

bchron <- tibble::tibble(
  calBC = bchronRaw$Date1$ageGrid - 1950,
  density_bchron = bchronRaw$Date1$densities
) 

bchron |> dplyr::left_join(
  test, by = "calBC"
) |>
  tidyr::pivot_longer(
    tidyselect::starts_with("dens"),
    names_to = "method"
  ) |>
  ggplot() +
  geom_line(aes(x = calBC, y = value, colour = method))


test$density |> sum()
bchron$density_bchron |> sum()
