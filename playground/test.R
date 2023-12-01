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

#### confirm the reliability of the random age sampling ####

run_currycarbon("--samplesFile /tmp/currySamples.tsv -n 10000")

age_samples <- readr::read_tsv("/tmp/currySamples.tsv")

year_count <- age_samples |>
  dplyr::mutate(yearBCAD = round(yearBCAD, -1)) |>
  dplyr::group_by(yearBCAD) |>
  dplyr::summarise(n = dplyr::n())

year_count |>
  ggplot() +
  geom_bar(aes(x = yearBCAD, y = n), stat = "identity")

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

# the following two results are equal
system('currycarbon "((A,3000,20)+(B,2900,200)+(C,2800,70))+(((D,3500,60)+(E,3400,60))+(F,3300,30))" --densityFile /tmp/currycarbonSumCalTest1.tsv')

sumCalTest1 <- readr::read_tsv(
  "/tmp/currycarbonSumCalTest2.tsv",
  col_types = readr::cols()
)

system('currycarbon "(A,3000,20)+(B,2900,200)+(C,2800,70)+(D,3500,60)+(E,3400,60)+(F,3300,30)" --densityFile /tmp/currycarbonSumCalTest2.tsv')

sumCalTest2 <- readr::read_tsv(
  "/tmp/currycarbonSumCalTest2.tsv",
  col_types = readr::cols()
)

ggplot() +
  geom_line(
    data = sumCalTest2,
    mapping = aes(x = yearBCAD, y = density),
    color = "red"
  ) +
  geom_line(
    data = sumCalTest2,
    mapping = aes(x = yearBCAD, y = density),
    color = "blue"
  )

# but the following two oxcal scripts are not - only the second one is similar to the currycarbon result
# I think that's how it should be (2023-12-01)

# Oxcal:
#
# Sum("(A+B+C)+((D*E)+F)")
# {
#   Sum("A+B+C")
#   {
#     R_Date("A",3000,20);
#     R_Date("B",2900,200);
#     R_Date("C",2800,70);
#   };
#   Sum("(D*E)+F")
#   {
#     Sum("D*E")
#     {
#       R_Date("D",3500,60);
#       R_Date("E",3400,60);
#     };
#     R_Date("F",3300,30);
#   };
# };

# Sum("(A+B+C)+((D*E)+F)")
# {
#   R_Date("A",3000,20);
#   R_Date("B",2900,200);
#   R_Date("C",2800,70);
#   R_Date("D",3500,60);
#   R_Date("E",3400,60);
#   R_Date("F",3300,30);
# };

#### another test of the random age sampling ####

system('currycarbon "A,3000,30+B,3200,40*C,3300,30" --samplesFile /tmp/currySamples.tsv -n 10000')

age_samples <- readr::read_tsv("/tmp/currySamples.tsv")

year_count <- age_samples |>
  dplyr::mutate(yearBCAD = round(yearBCAD, -1)) |>
  dplyr::group_by(yearBCAD) |>
  dplyr::summarise(n = dplyr::n())

year_count |>
  ggplot() +
  geom_bar(aes(x = yearBCAD, y = n), stat = "identity")
