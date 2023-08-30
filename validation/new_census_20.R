library(tidyverse)
library(readxl)
options(scipen = 999)
# Read in the data
old_2020 <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "old_2020") |> as.data.frame()

revenue <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "revenue")
curr_spen_sal_ben <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "curr spen sal ben")
supp_serv <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "supp serv")
cap <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "cap")
long_short_debt <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "long short debt")
enrollment <- read_excel("input_data/ss_data_2020_ii.xlsx", sheet = "enrollment")


cpi <- old_2020 |>
  select(state, cpi)

new_2020 <- revenue |>
  left_join(curr_spen_sal_ben) |>
  left_join(supp_serv) |>
  left_join(cap) |>
  left_join(long_short_debt) |>
  left_join(enrollment) |>
  left_join(cpi) |>
  as.data.frame()

names(old_2020)
names(new_2020)

# For all columns except for state and cpi, multiply the values by 1000
new_2020 <- new_2020 |>
  mutate(year = 2020) |>
  select(year, everything()) |>
#  make all values except for state numeric
    mutate(across(-c(year, state, cpi), as.numeric)) |>
  mutate(across(-c(year, state, enrollment, cpi), ~ .x * 1000))



# Comparison

# Identify common columns between the two dataframes
common_cols <- intersect(names(old_2020), names(new_2020))

# Join the two data frames by 'state'
comparison_df <- left_join(old_2020, new_2020, by = "state", suffix = c("_old", "_new"))

# Calculate the difference for each common column
for (col in common_cols) {
  if (col != "state") { # Exclude the 'state' column from the difference calculation
    comparison_df <- comparison_df %>%
      mutate(!!paste0("diff_", col) := !!sym(paste0(col, "_new")) - !!sym(paste0(col, "_old")))
  }
}

# Filter rows where there are differences
updated_rows <- comparison_df %>%
  rowwise() %>%
  filter(any(c_across(starts_with("diff_")) != 0))

# Put the diff values in per pupil terms
updated_rows <- updated_rows |>
  mutate(across(starts_with("diff_"), ~ .x / enrollment_new)) |>
#   mutate(across(starts_with("diff_"), ~ .x * 1000)) |>
  mutate(across(starts_with("diff_"), round, 2))

# View the rows with updates
print(updated_rows)

write_csv(updated_rows, "ss_changes_updated_rows_pp.csv")

