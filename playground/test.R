library(ggplot2)

testdate <- c(5000,190)

system(paste0(
  "currycarbon \"",
  testdate[1], ",", testdate[2],
  "\" --densityFile /tmp/currycarbon.txt"))
#--method MatrixMultiplication --noInterpolation"))

test <- readr::read_csv("/tmp/currycarbon.txt")

bchronRaw <- Bchron::BchronCalibrate(
  testdate[1], testdate[2],
  calCurves = 'intcal20'
)

bchron <- tibble::tibble(
  calBCAD = -bchronRaw$Date1$ageGrid + 1950,
  density_bchron = bchronRaw$Date1$densities
)

bchron |> dplyr::full_join(
  test, by = "calBCAD"
) |>
  tidyr::pivot_longer(
    tidyselect::starts_with("dens"),
    names_to = "method"
  ) |>
  ggplot() +
  geom_point(aes(x = calBCAD, y = value, colour = method), 
            size = 1, alpha = 0.5)

### large test (for memory leaks)

calpal <- c14bazAAR::get_calpal()
calpal |> dplyr::select(c14age, c14std) |> dplyr::slice_head(n = 5000) |> readr::write_csv("/tmp/currycarbon_large_input_test.csv", col_names = F)

#currycarbon --inputFile /tmp/currycarbon_large_input_test.csv -q --densityFile /dev/null

