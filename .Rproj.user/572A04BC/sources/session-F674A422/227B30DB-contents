library(tidyverse)
library(readxl)
options(scipen = 999)
# Read in the data
old_2020 <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "old_2020") |> as.data.frame()

revenue <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "revenue")
curr_spen_sal_ben <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "curr spen sal ben")
supp_serv <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "supp serv")
cap <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "cap")
long_short_debt <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "long short debt")
enrollment <- read_excel("validation/ss_data_2020_ii.xlsx", sheet = "enrollment")


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

# For all columns except for state and cpi, multiply the values by 1000
new_2020 <- new_2020 |>
  mutate(year = 2020) |>
  select(year, everything()) |>
#  make all values except for state numeric
    mutate(across(-c(year, state, cpi), as.numeric)) |>
  mutate(across(-c(year, state, enrollment, cpi), ~ .x * 1000))

# For every variable except, year, state, enrollment, and cpi create a new per pupil(_pp) variable using enrollment
new_2020_pp <- new_2020 |>
    mutate(across(-c(year, state, enrollment, cpi), ~ .x / enrollment)) |>
    rename_with(~ str_c(., "_pp"), -c(year, state, enrollment, cpi))

# Join new_2020 and new_2020_pp
new_2020 <- new_2020 |>
    left_join(new_2020_pp, by = c("state", "year", "cpi", "enrollment"), suffix = c("", "_pp")) |>
    as.data.frame()

# Reorganize the columns so that the per pupil variables are next to their non-per pupil counterparts
new_2020 <- new_2020 |>
    select(
        year,
        state,
        total_revenue,
        total_revenue_pp,
        federal_revenue,
        federal_revenue_pp,
        state_revenue,
        state_revenue_pp,
        local_revenue,
        local_revenue_pp,
        current_spending,
        current_spending_pp,
        total_salary,
        total_salary_pp,
        total_benefits,
        total_benefits_pp,
        all_instruction,
        all_instruction_pp,
        instructional_staff_salary,
        instructional_staff_salary_pp,
        instructional_staff_benefits,
        instructional_staff_benefits_pp,
        all_support_services,
        all_support_services_pp,
        support_services_staff_salary,
        support_services_staff_salary_pp,
        support_services_staff_benefits,
        support_services_staff_benefits_pp,
        ss_pupil_support_services,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services,
        ss_instructional_staff_support_services_pp,
        ss_general_administration,
        ss_general_administration_pp,
        ss_school_administration,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services,
        ss_other_nonspecified_support_services_pp,
        total_capital_outlay,
        total_capital_outlay_pp,
        short_term_debt,
        short_term_debt_pp,
        long_term_debt,
        long_term_debt_pp,
        enrollment,
        cpi
    )

# Read in complete old data
old_2020_complete <- read_csv("input_data/ss_data.csv") |> as.data.frame()

names(old_2020_complete)
names(new_2020)

# I'd like to put the names of old_2020_complete and new_2020 in a dataframe to make sure they match
names_comparison <- data.frame(
    old_names = names(old_2020_complete), 
    new_names = names(new_2020)
)


# Replace names in old_2020_complete with names in new_2020
names(old_2020_complete) <- names(new_2020)

# Replace 2020 data in old_2020_complete with 2020 data in new_2020
old_2020_complete[old_2020_complete$year == 2020, ] <- new_2020

# Write data
write_csv(old_2020_complete, "input_data/ss_data_updated.csv")