library(tidyverse)

ss_data_cpi <- read_csv("input_data/ss_data_cpi.csv")

# Create a data frame with the change in `Total Revenue - Per Pupil` for each state between 2002 and 2020
rev_change_03 <- ss_data_cpi |>
  select(year, state, total_revenue_pp) |>
  rename(
    Year = year,
    State = state,
    `Total Revenue - Per Pupil` = total_revenue_pp
  ) |>
  filter(Year == 2003 | Year == 2019) |>
  pivot_wider(
    names_from = Year,
    values_from = `Total Revenue - Per Pupil`
  ) |>
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `2019` / `2003` - 1
  ) |>
  rename(
    `Total Revenue - Per Pupil 2019` = `2019`
  ) |>
  select(State, `Total Revenue - Per Pupil Percent Change`, `Total Revenue - Per Pupil 2019`)

names(rev_change_03)[1:2] <- c("State", "Percent Change Since 2003")


rev_change_02 <- ss_data_cpi |>
  select(year, state, total_revenue_pp) |>
  rename(
    Year = year,
    State = state,
    `Total Revenue - Per Pupil` = total_revenue_pp
  ) |>
  filter(Year == 2002 | Year == 2020) |>
  pivot_wider(
    names_from = Year,
    values_from = `Total Revenue - Per Pupil`
  ) |>
  mutate(
    `Total Revenue - Per Pupil Percent Change` = `2020` / `2002` - 1
  ) |>
  rename(
    `Total Revenue - Per Pupil 2020` = `2020`
  ) |>
  select(State, `Total Revenue - Per Pupil Percent Change`, `Total Revenue - Per Pupil 2020`)

names(rev_change_02)[1:2] <- c("State", "Percent Change Since 2002")


rev_change <- left_join(rev_change_02, rev_change_03) |>
  select(State, `Percent Change Since 2003`, `Total Revenue - Per Pupil 2019`)


write_csv(rev_change, "output_data/revenue_change_2003_2019.csv")
