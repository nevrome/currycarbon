library(ggplot2)
oxcAAR::quickSetupOxcal()

#### oxcal vs currycarbon for sums and products ####

run_oxcal <- function(code) {
  oxcalRawRes <- oxcAAR::executeOxcalScript(code) |>
    oxcAAR::readOxcalOutput() |>
    oxcAAR::parseFullOxcalOutput()
  tibble::tibble(
    density = oxcalRawRes$`ocd[1]`$likelihood$prob,
    yearBCAD = seq(
      oxcalRawRes$`ocd[1]`$likelihood$start,
      by = oxcalRawRes$`ocd[1]`$likelihood$resolution,
      length.out = length(density)
    )
  )
}

run_currycarbon <- function(code) {
  system(
    paste0(
      "currycarbon \"",
      code,
      "\" --densityFile /tmp/currycarbonOutput.tsv -q"
    )
  )
  readr::read_tsv(
    "/tmp/currycarbonOutput.tsv",
    col_types = readr::cols()
  ) %>%
    dplyr::mutate(
      density = density/max(density)
    )
}

# Test 1

oxcal_test1 <- run_oxcal(
'
  Sum("A+B+C)")
  {
    R_Date("A",3000,20);
    R_Date("B",2500,200);
    R_Date("C",2800,70);
  };
'
)

currycarbon_test1 <- run_currycarbon(
  "A,3000,20+B,2500,200+C,2800,70"
)

ggplot(mapping = aes(x = yearBCAD, y = density)) +
  geom_line(data = oxcal_test1) +
  geom_line(data = currycarbon_test1, color = "red")

# Test 2

oxcal_test2 <- run_oxcal(
'
  Sum("A+(B*C)")
  {
    R_Date("A",3000,30);
    Combine("B*C")
    {
      R_Date("B",3200,40);
      R_Date("C",3300,30);
    };
  };
'
)

currycarbon_test2 <- run_currycarbon(
  "A,3000,30+(B,3200,40*C,3300,30)"
)

ggplot(mapping = aes(x = yearBCAD, y = density)) +
  geom_line(data = oxcal_test2) +
  geom_line(data = currycarbon_test2, color = "red")

# Test 3

oxcal_test3a <- run_oxcal(
'
  Sum("(A+B+C)+((D*E)+F)")
  {
    R_Date("A",3000,20);
    R_Date("B",2900,200);
    R_Date("C",2800,70);
    R_Date("D",3500,60);
    R_Date("E",3400,60);
    R_Date("F",3300,30);
  };
'
)

oxcal_test3b <- run_oxcal(
'
  Sum("(A+B+C)+((D*E)+F)")
  {
    Sum("A+B+C")
    {
      R_Date("A",3000,20);
      R_Date("B",2900,200);
      R_Date("C",2800,70);
    };
    Sum("(D*E)+F")
    {
      Sum("D*E")
      {
        R_Date("D",3500,60);
        R_Date("E",3400,60);
      };
      R_Date("F",3300,30);
    };
  };
'
)

currycarbon_test3a <- run_currycarbon(
  "(A,3000,20)+(B,2900,200)+(C,2800,70)+(D,3500,60)+(E,3400,60)+(F,3300,30)"
)

currycarbon_test3b <- run_currycarbon(
  "((A,3000,20)+(B,2900,200)+(C,2800,70))+(((D,3500,60)+(E,3400,60))+(F,3300,30))"
)

# currycarbon behaves consistently like a simple sum model
ggplot(mapping = aes(x = yearBCAD, y = density)) +
  geom_line(
    data = oxcal_test3a,
  ) +
  geom_line(
    data = currycarbon_test3a,
    color = "red"
  ) +
  geom_point(
    data = currycarbon_test3b,
    color = "green",
    size = 0.1
  )

# oxcal behaves differently with complex models, maybe because of intermediate normalization
ggplot(mapping = aes(x = yearBCAD, y = density)) +
  geom_line(
    data = oxcal_test3b,
  ) +
  geom_point(
    data = currycarbon_test3b,
    color = "green",
    size = 0.1
  )
