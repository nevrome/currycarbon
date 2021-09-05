library(ggplot2)

testdate <- c(10000,200)

system(paste0(
  "currycarbon \"",
  testdate[1], ",", testdate[2],
  "\" --densityFile /tmp/currycarbon.txt"))# --explore --exploreDir \"/tmp/currytest\""))

# cal_curve_matrix <- as.matrix(read.csv("/tmp/currytest/calCurveMatrix.csv", row.names = 1, header = T))
# cal_curve_segment <- readr::read_csv("/tmp/currytest/calCurveInterpolated.csv")

# image(cal_curve_matrix)
# 
# cal_curve_segment |>
#   ggplot() +
#   geom_point(aes(x = `CAL BP`, y = `14C age`, label = `14C age`))


test <- readr::read_csv("/tmp/currycarbon.txt")

bchronRaw <- Bchron::BchronCalibrate(
  testdate[1], testdate[2],
  calCurves = 'intcal20'
)

bchron <- tibble::tibble(
  calBCAD = -bchronRaw$Date1$ageGrid + 1950,
  density_bchron = bchronRaw$Date1$densities
)

bchron |> dplyr::left_join(
  test, by = "calBCAD"
) |>
  tidyr::pivot_longer(
    tidyselect::starts_with("dens"),
    names_to = "method"
  ) |>
  ggplot() +
  geom_point(aes(x = calBCAD, y = value, colour = method), 
            size = 1, alpha = 0.5)

