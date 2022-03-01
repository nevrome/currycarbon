library(ggplot2)

testdate <- c(5000,190)

run_currycarbon <- function(additional_commands = "") {
  system(
    paste0(
      "currycarbon \"",
      testdate[1], ",", testdate[2],
      "\" ",
      additional_commands
    )
  )
}

run_currycarbon_calPDF <- function(additional_commands = "") {
  run_currycarbon(paste(
    "--densityFile /tmp/currycarbonOutput.txt",
    additional_commands
  ))
  readr::read_csv(
    "/tmp/currycarbonOutput.txt",
    col_types = readr::cols()
  )
}

#### comparison with the Bchron R package ####

curry_bchron_studentT100 <- run_currycarbon_calPDF()
curry_matrixmult_default <- run_currycarbon_calPDF("--method MatrixMultiplication")
curry_bchron_normal <- run_currycarbon_calPDF("--method \"Bchron,Normal\"")
# --noInterpolation"))


bchronRaw <- Bchron::BchronCalibrate(
  testdate[1], testdate[2],
  calCurves = 'intcal20'
)
bchron <- tibble::tibble(
  calBCAD = -bchronRaw$Date1$ageGrid + 1950,
  density_bchron = bchronRaw$Date1$densities
)

bchron |> 
  dplyr::full_join(curry_bchron_studentT100, by = "calBCAD") |>
  #dplyr::full_join(curry_bchron_normal, by = "calBCAD") |>
  #dplyr::full_join(curry_matrixmult_default, by = "calBCAD") |>
  tidyr::pivot_longer(
    tidyselect::starts_with("dens"),
    names_to = "method"
  ) |>
  ggplot() +
  geom_line(
    aes(x = calBCAD, y = value, colour = method), 
    size = 1, alpha = 0.5
  )

#### large test (for memory leaks) ####

calpal <- c14bazAAR::get_calpal()
calpal |> dplyr::select(c14age, c14std) |> dplyr::slice_head(n = 5000) |> readr::write_csv("/tmp/currycarbon_large_input_test.csv", col_names = F)

#currycarbon --inputFile /tmp/currycarbon_large_input_test.csv -q --densityFile /dev/null

#### other tests ####

run_currycarbon(paste(
  "--calCurveMatrixFile /tmp/curryMatrix.csv",
  "--calCurveSegmentFile /tmp/currySegment.csv"
))

cal_matrix <- readr::read_csv("/tmp/curryMatrix.csv") |>
  tidyr::gather(vars, count, -...1) |>
  dplyr::transmute(
    uncal = ...1,
    cal = as.numeric(vars),
    val = count
  )

cal_segment <- readr::read_csv("/tmp/currySegment.csv")

ggplot() +
  geom_raster(
    data = cal_matrix,
    mapping = aes(x = cal, y = uncal, fill = val)
  ) +
  geom_path(
    data = cal_segment,
    mapping = aes(x = calBCAD, y = uncalBCAD),
    color = "red", size = 0.5
  ) +
  scale_y_reverse() +
  coord_fixed()

  