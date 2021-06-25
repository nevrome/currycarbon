library(ggplot2)

test <- readr::read_csv("~/agora/currycarbon/test.txt")

bchronRaw <- Bchron::BchronCalibrate(
  4000, 25, 
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
  geom_line(aes(x = calBC, y = value, colour = method)) +
  coord_cartesian(xlim = c(2700,2300))


test$density |> sum()
bchron$density_bchron |> sum()
