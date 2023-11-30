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
    "--densityFile /tmp/currycarbonOutput.tsv -q",
    additional_commands
  ))
  readr::read_tsv(
    "/tmp/currycarbonOutput.tsv",
    col_types = readr::cols()
  )
}

#### comparison with the Bchron R package ####

curry_bchron_studentT100 <- run_currycarbon_calPDF() |>
  dplyr::rename(density_curry_bchron_studentT100 = density)
curry_matrixmult <- run_currycarbon_calPDF("--method MatrixMultiplication") |>
  dplyr::rename(density_curry_matrixmult = density)
curry_bchron_normal <- run_currycarbon_calPDF("--method \"Bchron,Normal\"") |>
  dplyr::rename(density_curry_bchron_normal = density)

bchronRaw <- Bchron::BchronCalibrate(
  testdate[1], testdate[2],
  calCurves = 'intcal20'
)
bchron <- tibble::tibble(
  yearBCAD = -bchronRaw$Date1$ageGrid + 1950,
  density_bchron = bchronRaw$Date1$densities
)

bchron |>
  dplyr::full_join(curry_bchron_studentT100, by = "yearBCAD") |>
  dplyr::full_join(curry_bchron_normal, by = "yearBCAD") |>
  dplyr::full_join(curry_matrixmult, by = "yearBCAD") |>
  tidyr::pivot_longer(
    tidyselect::starts_with("dens"),
    names_to = "method"
  ) |>
  ggplot() +
  geom_line(
    aes(x = yearBCAD, y = value, colour = method), 
    linewidth = 1, alpha = 0.5
  )

#### large test (for memory leaks) ####

calpal <- c14bazAAR::get_calpal()
calpal |> dplyr::select(c14age, c14std) |> dplyr::slice_head(n = 5000) |> readr::write_csv("/tmp/currycarbon_large_input_test.csv", col_names = F)

system("currycarbon --inputFile /tmp/currycarbon_large_input_test.csv -q")

#### cal curve ####

run_currycarbon(paste(
  "--calCurveMatFile /tmp/curryMatrix.tsv",
  "--calCurveSegFile /tmp/currySegment.tsv"
))

cal_matrix <- readr::read_tsv("/tmp/curryMatrix.tsv") |>
  tidyr::gather(vars, count, -...1) |>
  dplyr::transmute(
    uncal = ...1,
    cal = as.numeric(vars),
    val = count
  )

cal_segment <- readr::read_tsv("/tmp/currySegment.tsv")

ggplot() +
  geom_raster(
    data = cal_matrix,
    mapping = aes(x = cal, y = uncal, fill = val)
  ) +
  geom_path(
    data = cal_segment,
    mapping = aes(x = calYearBCAD, y = uncalYearBCAD),
    color = "red", linewidth = 0.5
  ) +
  scale_y_reverse() +
  coord_fixed()

#### sum and product cal ####

system('currycarbon "A,3000,20+B,2500,200+C,2800,70" --densityFile /tmp/currycarbonSumCalTest1.tsv')

sumCalTest1 <- readr::read_tsv(
  "/tmp/currycarbonSumCalTest1.tsv",
  col_types = readr::cols()
)

sumCalTest1 |>
  ggplot() +
  geom_line(aes(x = yearBCAD, y = density))

# Oxcal:
#
# Sum("A+B+C)")
# {
#   R_Date("A",3000,20);
#   R_Date("B",2500,200);
#   R_Date("C",2800,70);
# };

system('currycarbon "A,3000,30+B,3200,40*C,3300,30" --densityFile /tmp/currycarbonSumCalTest2.tsv')

sumCalTest2 <- readr::read_tsv(
  "/tmp/currycarbonSumCalTest2.tsv",
  col_types = readr::cols()
)

sumCalTest2 |>
  ggplot() +
  geom_line(aes(x = yearBCAD, y = density))

# Oxcal:
#
# Sum("A+(B*C)")
# {
#   R_Date("A",3000,30);
#   Combine("B*C")
#   {
#     R_Date("B",3200,40);
#     R_Date("C",3300,30);
#   };
# };
