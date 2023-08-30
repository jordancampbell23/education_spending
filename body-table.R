# Load libararies
library(tidyverse)
options(scipen = 999)

# Load data
ss_data <- read_csv("input_data/ss_data_updated.csv") |> as.data.frame()

ss_data_cpi <- ss_data |>
    filter(state != "District of Columbia") |>
    arrange(state, year) |>
    group_by(state) |>
    # adjust for inflation for all variables except year, state, enrollment, and cpi
     mutate(
        total_revenue = total_revenue * (last(cpi) / cpi),
        total_revenue_pp = total_revenue_pp * (last(cpi) / cpi),
        federal_revenue = federal_revenue * (last(cpi) / cpi),
        federal_revenue_pp = federal_revenue_pp * (last(cpi) / cpi),
        state_revenue = state_revenue * (last(cpi) / cpi),
        state_revenue_pp = state_revenue_pp * (last(cpi) / cpi),
        local_revenue = local_revenue * (last(cpi) / cpi),
        local_revenue_pp = local_revenue_pp * (last(cpi) / cpi),
        current_spending = current_spending * (last(cpi) / cpi),
        current_spending_pp = current_spending_pp * (last(cpi) / cpi),
        total_salary = total_salary * (last(cpi) / cpi),
        total_salary_pp = total_salary_pp * (last(cpi) / cpi),
        total_benefits = total_benefits * (last(cpi) / cpi),
        total_benefits_pp = total_benefits_pp * (last(cpi) / cpi),
        all_instruction = all_instruction * (last(cpi) / cpi),
        all_instruction_pp = all_instruction_pp * (last(cpi) / cpi),
        instructional_staff_salary = instructional_staff_salary * (last(cpi) / cpi),
        instructional_staff_salary_pp = instructional_staff_salary_pp * (last(cpi) / cpi),
        instructional_staff_benefits = instructional_staff_benefits * (last(cpi) / cpi),
        instructional_staff_benefits_pp = instructional_staff_benefits_pp * (last(cpi) / cpi),
        all_support_services = all_support_services * (last(cpi) / cpi),
        all_support_services_pp = all_support_services_pp * (last(cpi) / cpi),
        support_services_staff_salary = support_services_staff_salary * (last(cpi) / cpi),
        support_services_staff_salary_pp = support_services_staff_salary_pp * (last(cpi) / cpi),
        support_services_staff_benefits = support_services_staff_benefits * (last(cpi) / cpi),
        support_services_staff_benefits_pp = support_services_staff_benefits_pp * (last(cpi) / cpi),
        ss_pupil_support_services = ss_pupil_support_services * (last(cpi) / cpi),
        ss_pupil_support_services_pp = ss_pupil_support_services_pp * (last(cpi) / cpi),
        ss_instructional_staff_support_services = ss_instructional_staff_support_services * (last(cpi) / cpi),
        ss_instructional_staff_support_services_pp = ss_instructional_staff_support_services_pp * (last(cpi) / cpi),
        ss_general_administration = ss_general_administration * (last(cpi) / cpi),
        ss_general_administration_pp = ss_general_administration_pp * (last(cpi) / cpi),
        ss_school_administration = ss_school_administration * (last(cpi) / cpi),
        ss_school_administration_pp = ss_school_administration_pp * (last(cpi) / cpi),
        ss_operation_and_maintenance_of_plant = ss_operation_and_maintenance_of_plant * (last(cpi) / cpi),
        ss_operation_and_maintenance_of_plant_pp = ss_operation_and_maintenance_of_plant_pp * (last(cpi) / cpi),
        ss_pupil_transportation = ss_pupil_transportation * (last(cpi) / cpi),
        ss_pupil_transportation_pp = ss_pupil_transportation_pp * (last(cpi) / cpi),
        ss_other_nonspecified_support_services = ss_other_nonspecified_support_services * (last(cpi) / cpi),
        ss_other_nonspecified_support_services_pp = ss_other_nonspecified_support_services_pp * (last(cpi) / cpi),
        total_capital_outlay = total_capital_outlay * (last(cpi) / cpi),
        total_capital_outlay_pp = total_capital_outlay_pp * (last(cpi) / cpi),
        short_term_debt = short_term_debt * (last(cpi) / cpi),
        short_term_debt_pp = short_term_debt_pp * (last(cpi) / cpi),
        long_term_debt = long_term_debt * (last(cpi) / cpi),
        long_term_debt_pp = long_term_debt_pp * (last(cpi) / cpi),
        total_debt = short_term_debt + long_term_debt,
        total_debt_pp = short_term_debt_pp + long_term_debt_pp
    )

# Write CSV
write_csv(ss_data_cpi, "input_data/ss_data_cpi.csv")



# Table 1: Key U.S Public School Spending, Staffing, and Enrollment Trends (2002-2020)
staffing <- read_csv("input_data/teachers_non_teachers.csv")
salary <- read_csv("input_data/salary_data.csv")

salary_pct <- salary |>
    filter(year != 2021) |>
     filter(state != "District of Columbia") |>
    group_by(state) |>
    mutate(salary_adj = salary * last(cpi) / cpi)

intro_table_1 <- ss_data_cpi |>
    filter(year == 2002 | year == 2020) |>
    filter(state == "United States") |>
    select(year, total_revenue_pp, total_benefits_pp, enrollment) |>
    left_join(
        staffing |>
            filter(year == 2002 | year == 2020) |>
            filter(state == "United States") |>
            select(year, teachers, all_staff) |>
            mutate(non_teachers = all_staff - teachers) |>
            mutate(total_staff = teachers + non_teachers),
            by = "year"
    ) |>
    left_join(
        salary_pct |>
            filter(year == 2002 | year == 2020) |>
            filter(state == "United States") |>
            select(year, salary_adj),
            by = "year"
    ) |>
    select(-state.y, -all_staff) |>
    rename(
        `Total Revenue` = total_revenue_pp,
        `Total Benefits` = total_benefits_pp,
    )

# Pivot intro_table_1 to long format, with the following columns: Category, 2002, 2020
names(intro_table_1) <- c("State.x", "Year", "Total Revenue", "Total Benefits", "Enrollment", "Teachers", "Non-Teachers", "Total Staff", "Salary Adj")

intro_table_1 <- intro_table_1 |>
    pivot_longer(
         cols = c(
            `Total Revenue`,
            `Total Benefits`,
            Enrollment,
            `Total Staff`,
            `Non-Teachers`,
            `Teachers`,
            `Salary Adj`
    ),
        names_to = "Category",
        values_to = "Value"
    ) |>
      #   Pivot intro_table_1 Year wider
    pivot_wider(
        names_from = Year,
        values_from = Value
    ) |>
    select(-State.x) |>
    mutate(
        Growth_pct = (`2020` / `2002`) - 1,
        Growth_diff = `2020` - `2002`
    )


# Write intro_table_1 to CSV
write_csv(intro_table_1, "output_data/body/introduction/intro_table_1_summary.csv")




# Table 2: U.S. NAEP Score Growth by Subject
naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_ts.csv")
naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_ts.csv")
naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_ts.csv")
naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_ts.csv")


# Create a table for 4th Grade NAEP Reading Score Growth (2003-2019)
us_naep_reading_4 <- naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State == "United States") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Reading 4 Score Pct`, `Reading 4 Score Diff`) |>
  rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`)

# Create a table for 8th Grade NAEP Reading Score Growth (2003-2019)
us_naep_reading_8 <- naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State == "United States") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Reading 8 Score Pct`, `Reading 8 Score Diff`) |>
  rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`)

# Create a table for 4th Grade NAEP Math Score Growth (2003-2019)
us_naep_math_4 <- naep_math_4 |>
  filter(Year == 2019) |>
  filter(State == "United States") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Math 4 Score Pct`, `Math 4 Score Diff`) |>
  rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`)

# Create a table for 8th Grade NAEP Math Score Growth (2003-2019)
us_naep_math_8 <- naep_math_8 |>
  filter(Year == 2019) |>
  filter(State == "United States") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Math 8 Score Pct`, `Math 8 Score Diff`) |>
  rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`)


# Take us_naep_reading_4, us_naep_reading_8, us_naep_math_4, us_naep_math_8 and combine into one table
# that has the following columns: `4th Grade Reading`, `4th Grade Math`, `8th Grade Reading`, and `8th Grade Math`

us_naep <- us_naep_reading_4 |>
  left_join(us_naep_reading_8, by = "State") |>
  left_join(us_naep_math_4, by = "State") |>
  left_join(us_naep_math_8, by = "State") |>
  mutate(`Student Group` = "All") |>
  select(
    `Student Group`,
    `Reading 4 Score Pct`,
    `Reading 4 Score Diff`,
    `Reading 8 Score Pct`,
    `Reading 8 Score Diff`,
    `Math 4 Score Pct`,
    `Math 4 Score Diff`,
    `Math 8 Score Pct`,
    `Math 8 Score Diff`
  )


# Do the same for Low Income NAEP

low_income_naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_low_income_ts.csv")
low_income_naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_low_income_ts.csv")
low_income_naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_low_income_ts.csv")
low_income_naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_low_income_ts.csv")

# Create a table for 4th Grade NAEP Reading Score Growth (2003-2019) in the United States
low_income_naep_reading_4 <- low_income_naep_reading_4 |>
  filter(State == "United States") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_4_table <- low_income_naep_reading_4 |>
  filter(Year == 2019) |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading 4 Score Pct` = `Eligible.x` / `Eligible.y` - 1) |>
  mutate(`Reading 4 Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Reading 4 Score Pct`, `Reading 4 Score Diff`) |>
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`)

# Create a table for 8th Grade NAEP Reading Score Growth (2003-2019) in the United States
low_income_naep_reading_8 <- low_income_naep_reading_8 |>
  filter(State == "United States") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_8_table <- low_income_naep_reading_8 |>
  filter(Year == 2019) |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading 8 Score Pct` = `Eligible.x` / `Eligible.y` - 1) |>
  mutate(`Reading 8 Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Reading 8 Score Pct`, `Reading 8 Score Diff`) |>
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`)

# Create a table for 4th Grade NAEP Math Score Growth (2003-2019) in the United States
low_income_naep_math_4 <- low_income_naep_math_4 |>
  filter(State == "United States") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_4_table <- low_income_naep_math_4 |>
  filter(Year == 2019) |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math 4 Score Pct` = `Eligible.x` / `Eligible.y` - 1) |>
  mutate(`Math 4 Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Math 4 Score Pct`, `Math 4 Score Diff`) |>
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`)

# Create a table for 8th Grade NAEP Math Score Growth (2003-2019) in the United States
low_income_naep_math_8 <- low_income_naep_math_8 |>
  filter(State == "United States") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_8_table <- low_income_naep_math_8 |>
  filter(Year == 2019) |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math 8 Score Pct` = `Eligible.x` / `Eligible.y` - 1) |>
  mutate(`Math 8 Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Math 8 Score Pct`, `Math 8 Score Diff`) |>
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`)

# Take low_income_naep_reading_4_table, low_income_naep_reading_8_table, low_income_naep_math_4_table,
# and low_income_naep_math_8_table and combine them into one table

low_income_us_naep <- low_income_naep_reading_4_table |>
  left_join(
    low_income_naep_reading_8_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_4_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_8_table,
    by = "State"
  ) |>
  mutate(`Student Group` = "FR Eligible Only") |>
  select(
    `Student Group`,
    `Reading 4 Score Pct`,
    `Reading 4 Score Diff`,
    `Reading 8 Score Pct`,
    `Reading 8 Score Diff`,
    `Math 4 Score Pct`,
    `Math 4 Score Diff`,
    `Math 8 Score Pct`,
    `Math 8 Score Diff`
  )


# Bind together us_naep and  low_income_us_naep
us_naep <- rbind(us_naep, low_income_us_naep)


# Using us_naep create two tables one with Pct and one with Diff
us_naep_pct <- us_naep |>
  select(
    `Student Group`,
    `Reading 4 Score Pct`,
    `Reading 8 Score Pct`,
    `Math 4 Score Pct`,
    `Math 8 Score Pct`
  ) |>
  #  round to 1 decimal place
  mutate_at(vars(`Reading 4 Score Pct`, `Reading 8 Score Pct`, `Math 4 Score Pct`, `Math 8 Score Pct`), round, 3)

us_naep_diff <- us_naep |>
  select(
    `Student Group`,
    `Reading 4 Score Diff`,
    `Reading 8 Score Diff`,
    `Math 4 Score Diff`,
    `Math 8 Score Diff`
  ) |>
  #  round to 0 decimal place
  mutate_at(vars(`Reading 4 Score Diff`, `Reading 8 Score Diff`, `Math 4 Score Diff`, `Math 8 Score Diff`), round, 0)

# 12th Grade NAEP

naep_math_12 <- read_csv("input_data/raw_naep/naep_math_12.csv")
naep_reading_12 <- read_csv("input_data/raw_naep/naep_reading_12.csv")
low_income_math_12 <- read_csv("input_data/raw_naep/low_income_naep_math_12.csv")
low_income_reading_12 <- read_csv("input_data/raw_naep/low_income_naep_reading_12.csv")

options(scipen = 999)

# Create a table for 12th Grade NAEP Reading Score Growth (2003-2019) in the United States
naep_reading_12_table <- naep_reading_12 |>
  filter(Year == 2019) |>
  select(Jurisdiction, `Average scale score`) |>
  left_join(
    naep_reading_12 |>
      filter(Year == 2002) |>
      select(Jurisdiction, `Average scale score`),
    by = "Jurisdiction"
  ) |>
  mutate(`Reading 12 Score Pct` = `Average scale score.x` / `Average scale score.y` - 1) |>
  mutate(`Reading 12 Score Diff` = `Average scale score.x` - `Average scale score.y`) |>
  select(Jurisdiction, `Reading 12 Score Pct`, `Reading 12 Score Diff`)

# Create a table for 12th Grade NAEP Math Score Growth (2005-2019) in the United States
naep_math_12_table <- naep_math_12 |>
  filter(Year == 2019) |>
  select(Jurisdiction, `Average scale score`) |>
  left_join(
    naep_math_12 |>
      filter(Year == 2005) |>
      select(Jurisdiction, `Average scale score`),
    by = "Jurisdiction"
  ) |>
  mutate(`Math 12 Score Pct` = `Average scale score.x` / `Average scale score.y` - 1) |>
  mutate(`Math 12 Score Diff` = `Average scale score.x` - `Average scale score.y`) |>
  select(Jurisdiction, `Math 12 Score Pct`, `Math 12 Score Diff`)

# Create a table for Low Income 12th Grade NAEP Reading Score Growth (2003-2019) in the United States
low_income_naep_reading_12_table <- low_income_reading_12 |>
  filter(Year == 2019) |>
  filter(`National School Lunch Program eligibility, 3 categories` == "Eligible") |>
  select(Jurisdiction, `Average scale score`) |>
  left_join(
    low_income_reading_12 |>
      filter(Year == 2002) |>
      filter(`National School Lunch Program eligibility, 3 categories` == "Eligible") |>
      select(Jurisdiction, `Average scale score`),
    by = "Jurisdiction"
  ) |>
  mutate(`Reading 12 Score Pct` = `Average scale score.x` / `Average scale score.y` - 1) |>
  mutate(`Reading 12 Score Diff` = `Average scale score.x` - `Average scale score.y`) |>
  select(Jurisdiction, `Reading 12 Score Pct`, `Reading 12 Score Diff`)

# Create a table for Low Income 12th Grade NAEP Math Score Growth (2005-2019) in the United States
low_income_naep_math_12_table <- low_income_math_12 |>
  filter(Year == 2019) |>
  filter(`National School Lunch Program eligibility, 3 categories` == "Eligible") |>
  select(Jurisdiction, `Average scale score`) |>
  left_join(
    low_income_math_12 |>
      filter(Year == 2005) |>
      filter(`National School Lunch Program eligibility, 3 categories` == "Eligible") |>
      select(Jurisdiction, `Average scale score`),
    by = "Jurisdiction"
  ) |>
  mutate(`Math 12 Score Pct` = `Average scale score.x` / `Average scale score.y` - 1) |>
  mutate(`Math 12 Score Diff` = `Average scale score.x` - `Average scale score.y`) |>
  select(Jurisdiction, `Math 12 Score Pct`, `Math 12 Score Diff`)


# Take naep_reading_12_table, naep_math_12_table, low_income_naep_reading_12_table, low_income_naep_math_12_table
# and create a table with the following columns for each subject

us_naep_12 <- naep_reading_12_table |>
  left_join(naep_math_12_table, by = "Jurisdiction") |>
  mutate(`Student Group` = "All") |>
  select(
    `Student Group`,
    `Reading 12 Score Pct`,
    `Reading 12 Score Diff`,
    `Math 12 Score Pct`,
    `Math 12 Score Diff`
  )

low_income_us_naep_12 <- low_income_naep_reading_12_table |>
  left_join(low_income_naep_math_12_table, by = "Jurisdiction") |>
  mutate(`Student Group` = "FR Eligible Only") |>
  select(
    `Student Group`,
    `Reading 12 Score Pct`,
    `Reading 12 Score Diff`,
    `Math 12 Score Pct`,
    `Math 12 Score Diff`
  )

# Bind 12th grade NAEP and Low Income NAEP
us_naep_12 <- bind_rows(us_naep_12, low_income_us_naep_12)


# Using us_naep_12 create two tables one with Pct and one with Diff
us_naep_12_pct <- us_naep_12 |>
  select(
    `Student Group`,
    `Reading 12 Score Pct`,
    `Math 12 Score Pct`
  ) |>
  #  round to 1 decimal place
  mutate_at(vars(`Reading 12 Score Pct`, `Math 12 Score Pct`), round, 3)

us_naep_12_diff <- us_naep_12 |>
  select(
    `Student Group`,
    `Reading 12 Score Diff`,
    `Math 12 Score Diff`
  ) |>
  #  round to 0 decimal place
  mutate_at(vars(`Reading 12 Score Diff`, `Math 12 Score Diff`), round, 0)



# Join us_naep_pct and us_naep_12_pct
us_naep_pct <- us_naep_pct |>
  left_join(us_naep_12_pct, by = c("Student Group"))

# Join us_naep_diff and us_naep_12_diff
us_naep_diff <- us_naep_diff |>
  left_join(us_naep_12_diff, by = c("Student Group"))



# Write us_naep_pct and us_naep_diff to csv
write_csv(us_naep_pct, "output_data/body/introduction/intro_table_2_us_naep_pct.csv")
write_csv(us_naep_diff, "output_data/body/introduction/intro_table_2_us_naep_diff.csv")



# Table 3: Average Teacher Salary Growth for the Top 10 States in `Total Revenue - Per Pupil` growth between 2002 and 2020

# Create a table comparing the total revenue per student in 2020 to the total revenue per student in 2002
# w/ a column for percent change

rev_table <- ss_data_cpi |>
    filter(year == 2020) |>
    select(state, total_revenue_pp) |>
    left_join(
        ss_data_cpi |>
            filter(year == 2002) |>
            select(state, total_revenue_pp),
        by = "state"
    ) |>
    mutate(`Total Revenue - Per Pupil 2020` = total_revenue_pp.x) |>
    mutate(`Total Revenue - Per Pupil 2002` = total_revenue_pp.y) |>
    mutate(
        `Percent Change` = (`Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002`) - 1,
    ) |>
    select(state, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
    arrange(desc(`Percent Change`)) 

salary_pct <- salary |>
    filter(year != 2021) |>
    filter(state != "District of Columbia") |>
    group_by(state) |>
    mutate(salary_adj = salary * last(cpi) / cpi)

salary_table <- salary_pct |>
    filter(year == 2020) |>
    select(state, salary_adj) |>
    left_join(
        salary_pct |>
            filter(year == 2002) |>
            select(state, salary_adj),
        by = "state"
    ) |>
    mutate(`Salary Adj 2020` = salary_adj.x) |>
    mutate(`Salary Adj 2002` = salary_adj.y) |>
    mutate(
        `Percent Change` = (`Salary Adj 2020` / `Salary Adj 2002`) - 1,
    ) |>
    select(state, `Salary Adj 2020`, `Salary Adj 2002`, `Percent Change`)

# Join rev_table and salary_table
rev_salary_table <- rev_table |>
    left_join(salary_table, by = "state") |>
    arrange(desc(`Percent Change.x`)) |>
    select(state, `Total Revenue - Per Pupil 2002`, `Total Revenue - Per Pupil 2020`, `Percent Change.x`, `Salary Adj 2002`, `Salary Adj 2020`, `Percent Change.y`) |>
    head(10) |>
    mutate_at(vars(`Percent Change.x`, `Percent Change.y`), round, 3) |>
    mutate_at(vars(`Total Revenue - Per Pupil 2002`, `Total Revenue - Per Pupil 2020`, `Salary Adj 2002`, `Salary Adj 2020`), round, 0) |>
    rename(
        `Revenue Growth` = `Percent Change.x`,
        `Salary Growth` = `Percent Change.y`
    )


# Write rev_salary_table to csv
write_csv(rev_salary_table, "output_data/body/introduction/intro_table_3_rev_salary_table.csv")




# Table 4: Staffing Trends for the Top 10 Enrollment Decline States

# Create a table comparing student enrollment growth and staffing growth between 2002 and 2020
enrollment_trends <- ss_data_cpi |>
  filter(year == 2020) |>
  filter(state != "District of Columbia") |>
  select(state, enrollment) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      filter(state != "District of Columbia") |>
      select(state, enrollment),
    by = "state"
  ) |>
  rename(`Enrollment 2020` = enrollment.x) |>
  rename(`Enrollment 2002` = enrollment.y) |>
  mutate(
    `Enrollment Growth` = `Enrollment 2020` / `Enrollment 2002` - 1
  )


staffing_trends <- staffing |>
  filter(year == 2020) |>
  filter(state != "District of Columbia") |>
  select(state, teachers, all_staff) |>
  mutate(non_teachers = all_staff - teachers) |>
  mutate(total_staff = teachers + non_teachers) |>
  left_join(
    staffing |>
      filter(year == 2002) |>
      filter(state != "District of Columbia") |>
      select(state, teachers, all_staff) |>
      mutate(non_teachers = all_staff - teachers) |>
      mutate(total_staff = teachers + non_teachers),
    by = "state"
  ) |>
  rename(`Teachers 2020` = `teachers.x`) |>
  rename(`Teachers 2002` = `teachers.y`) |>
  rename(`Non-Teachers 2020` = `non_teachers.x`) |>
  rename(`Non-Teachers 2002` = `non_teachers.y`) |>
  rename(`Total Staff 2020` = `total_staff.x`) |>
  rename(`Total Staff 2002` = `total_staff.y`) |>
  mutate(
    `Teachers Growth` = `Teachers 2020` / `Teachers 2002` - 1
  ) |>
  mutate(
    `Non-Teachers Growth` = `Non-Teachers 2020` / `Non-Teachers 2002` - 1
  ) |>
  mutate(
    `Total Staff Growth` = `Total Staff 2020` / `Total Staff 2002` - 1
  )


# Join enrollment_trends and staffing_trends

enrollment_staffing_trends <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  head(10) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`)


# Write enrollment_staffing_trends to csv
write_csv(enrollment_staffing_trends, "output_data/body/introduction/intro_table_4_enrollment_staffing_trends.csv")



# Table 5: Benefit Spending Per Student Growth vs. Total Revenue Per Student Growth

# Create a table comparing benefit spending per student growth and total revenue per student growth between 2002 and 2020

rev_benefit_trends <- ss_data_cpi |>
  filter(year == 2020) |>
  filter(state != "District of Columbia") |>
  select(state, total_revenue_pp, total_benefits_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      filter(state != "District of Columbia") |>
      select(state, total_revenue_pp, total_benefits_pp),
    by = "state"
  ) |>
  rename(`Total Revenue - Per Pupil 2020` = `total_revenue_pp.x`) |>
  rename(`Total Revenue - Per Pupil 2002` = `total_revenue_pp.y`) |>
  rename(`Total Benefits - Per Pupil 2020` = `total_benefits_pp.x`) |>
  rename(`Total Benefits - Per Pupil 2002` = `total_benefits_pp.y`) |>
  mutate(
    `Total Revenue Raw Growth` = `Total Revenue - Per Pupil 2020` - `Total Revenue - Per Pupil 2002`
  ) |>
  mutate(
    `Total Benefits Raw Growth` = `Total Benefits - Per Pupil 2020` - `Total Benefits - Per Pupil 2002`
  ) |>
  mutate(
    `Total Benefits Growth` = `Total Benefits - Per Pupil 2020` / `Total Benefits - Per Pupil 2002` - 1
  ) |>
  mutate(
    `Benefit Spending Growth / Revenue Growth` = `Total Benefits Raw Growth` / `Total Revenue Raw Growth`
  ) |>
  arrange(desc(`Total Benefits Growth`)) |>
  select(state, `Total Benefits - Per Pupil 2002`, `Total Benefits - Per Pupil 2020`, `Total Benefits Growth`, `Total Benefits Raw Growth`, `Total Revenue Raw Growth`, `Benefit Spending Growth / Revenue Growth`) |>
  mutate_at(vars(`Total Benefits Raw Growth`, `Total Revenue Raw Growth`, `Total Benefits - Per Pupil 2002`, `Total Benefits - Per Pupil 2020`), round, 0) |>
  mutate_at(vars(`Total Benefits Growth`), round, 3) |>
  mutate_at(vars(`Benefit Spending Growth / Revenue Growth`), round, 3) |>
  head(10)


# Write rev_benefit_trends to csv
write_csv(rev_benefit_trends, "output_data/body/introduction/intro_table_5_rev_benefit_trends.csv")




# Table 6: NAEP Score Growth for the Five Highest Spending States

naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_ts.csv")
naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_ts.csv")
naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_ts.csv")
naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_ts.csv")


naep_reading_4_table <- naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Reading 4 Score Diff`) |>
  # mutate(`Reading 4 Score Pct` = round(`Reading 4 Score Pct`, 3)) |>
  mutate(`Reading 4 Score Diff` = round(`Reading 4 Score Diff`, 0))


naep_reading_8_table <- naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Reading 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Reading 8 Score Diff`) |>
  # mutate(`Reading 8 Score Pct` = round(`Reading 8 Score Pct`, 3)) |>
  mutate(`Reading 8 Score Diff` = round(`Reading 8 Score Diff`, 0))


naep_math_4_table <- naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 4 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 4 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Math 4 Score Diff`) |>
  # mutate(`Math 4 Score Pct` = round(`Math 4 Score Pct`, 3)) |>
  mutate(`Math 4 Score Diff` = round(`Math 4 Score Diff`, 0))


naep_math_8_table <- naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math 8 Score Pct` = `NAEP.x` / `NAEP.y` - 1) |>
  mutate(`Math 8 Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `Math 8 Score Diff`) |>
  # mutate(`Math 8 Score Pct` = round(`Math 8 Score Pct`, 3)) |>
  mutate(`Math 8 Score Diff` = round(`Math 8 Score Diff`, 0))





# Low income NAEP

low_income_naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_low_income_ts.csv")
low_income_naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_low_income_ts.csv")
low_income_naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_low_income_ts.csv")
low_income_naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_low_income_ts.csv")


low_income_naep_reading_4 <- low_income_naep_reading_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_4_table <- low_income_naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Reading Score Diff`) |>
  mutate(`Reading Score Diff` = round(`Reading Score Diff`, 0)) |>
  rename(`Low Income Reading 4 Score` = `Reading Score Diff`)


low_income_naep_reading_8 <- low_income_naep_reading_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_8_table <- low_income_naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Reading Score Diff`) |>
  mutate(`Reading Score Diff` = round(`Reading Score Diff`, 0)) |>
  rename(`Low Income Reading 8 Score` = `Reading Score Diff`)


low_income_naep_math_4 <- low_income_naep_math_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_4_table <- low_income_naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Math Score Diff`) |>
  mutate(`Math Score Diff` = round(`Math Score Diff`, 0)) |>
  rename(`Low Income Math 4 Score` = `Math Score Diff`)


low_income_naep_math_8 <- low_income_naep_math_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_8_table <- low_income_naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Math Score Diff`) |>
  mutate(`Math Score Diff` = round(`Math Score Diff`, 0)) |>
  rename(`Low Income Math 8 Score` = `Math Score Diff`)


# Create a table of the top 5 and bottom 5 states for `Total Revenue Per Pupil`
intro_table_6 <- ss_data_cpi |>
  filter(year == 2019) |>
  filter(state != "District of Columbia") |>
  select(state, total_revenue_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2003) |>
      select(state, total_revenue_pp),
    by = "state"
  ) |>
  mutate(`Total Revenue Per Pupil Diff` = `total_revenue_pp.x` / `total_revenue_pp.y` - 1) |>
  arrange(desc(`Total Revenue Per Pupil Diff`)) |>
  mutate(`Total Revenue Per Pupil Diff` = round(`Total Revenue Per Pupil Diff`, 3))

intro_table_6_top <- intro_table_6 |>
  head(3)

intro_table_6_bottom <- intro_table_6 |>
  tail(3)

# Row bind the top and bottom tables
intro_table_6 <- rbind(intro_table_6_top, intro_table_6_bottom)

names(intro_table_6)[1] <- "State"

# Left join NAEP tables to intro table

intro_table_6_join <- intro_table_6 |>
  left_join(
    naep_math_4_table,
    by = "State"
  ) |>
  left_join(
    naep_math_8_table,
    by = "State"
  ) |>
  left_join(
    naep_reading_4_table,
    by = "State"
  ) |>
  left_join(
    naep_reading_8_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_4_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_math_8_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_reading_4_table,
    by = "State"
  ) |>
  left_join(
    low_income_naep_reading_8_table,
    by = "State"
  )

intro_table_6 <- intro_table_6_join |>
  mutate(`Revenue 2019` = round(`total_revenue_pp.x`, 0)) |>
  mutate(`Revenue 2003` = round(`total_revenue_pp.y`, 0)) |>
  select(State, `Revenue 2003`, `Revenue 2019`, `Total Revenue Per Pupil Diff`, `Reading 4 Score Diff`, `Math 4 Score Diff`, `Reading 8 Score Diff`, `Math 8 Score Diff`, `Low Income Reading 4 Score`, `Low Income Math 4 Score`, `Low Income Reading 8 Score`, `Low Income Math 8 Score`)


write_csv(intro_table_6, "output_data/body/introduction/intro_table_6.csv")




# Table 7: Total Revenue Per Student Growth (2002-2020)

rev_table <- ss_data_cpi |>
  filter(year == 2020) |>
  select(state, total_revenue_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, total_revenue_pp),
    by = "state"
  ) |>
  mutate(`Total Revenue - Per Pupil 2020` = `total_revenue_pp.x`) |>
  mutate(`Total Revenue - Per Pupil 2002` = `total_revenue_pp.y`) |>
  mutate(
    `Percent Change` = `Total Revenue - Per Pupil 2020` / `Total Revenue - Per Pupil 2002` - 1
  ) |>
  select(state, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))

# Remove the United States row, add a rank column, then add back the United States row to the top of the table

rev_table_states <- rev_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Total Revenue - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Total Revenue - Per Pupil 2020`,
    `2002` = `Total Revenue - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


rev_table_us <- rev_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Revenue - Per Pupil 2020`,
    `2002` = `Total Revenue - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


rev_table <- rbind(rev_table_us, rev_table_states)


rev_table <- rev_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(rev_table, "output_data/body/revenue_expenditure_trends/rev_table.csv")



# Table 8: Support Services Spending Per Student Growth (2002-2020)

support_services_table <- ss_data_cpi |>
  filter(year == 2020) |>
  select(state, all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, all_support_services_pp),
    by = "state"
  ) |>
  mutate(`All Support Services - Per Pupil 2020` = `all_support_services_pp.x`) |>
  mutate(`All Support Services - Per Pupil 2002` = `all_support_services_pp.y`) |>
  mutate(
    `Percent Change` = `All Support Services - Per Pupil 2020` / `All Support Services - Per Pupil 2002` - 1
  ) |>
  select(state, `All Support Services - Per Pupil 2020`, `All Support Services - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))

# Remove the United States row, add a rank column, then add back the United States row to the top of the table

support_services_table_states <- support_services_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`All Support Services - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `All Support Services - Per Pupil 2020`,
    `2002` = `All Support Services - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


support_services_table_us <- support_services_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `All Support Services - Per Pupil 2020`,
    `2002` = `All Support Services - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


support_services_table <- rbind(support_services_table_us, support_services_table_states)


support_services_table <- support_services_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(support_services_table, "output_data/body/revenue_expenditure_trends/support_services_table.csv")




# Table 9: Instruction Spending Per Student Growth (2002-2020)

ss_data_instruction <- read_csv("input_data/ss_data_instruction.csv") |> as.data.frame()

ss_data_instruction <- ss_data_instruction |>
    mutate(all_instruction_pp = all_instruction_pp * (last(cpi) / cpi))

instruction_table <- ss_data_instruction |>
  filter(year == 2020) |>
  select(state, all_instruction_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, all_instruction_pp),
    by = "state"
  ) |>
  mutate(`Instruction - Per Pupil 2020` = `all_instruction_pp.x`) |>
  mutate(`Instruction - Per Pupil 2002` = `all_instruction_pp.y`) |>
  mutate(
    `Percent Change` = `Instruction - Per Pupil 2020` / `Instruction - Per Pupil 2002` - 1
  ) |>
  select(state, `Instruction - Per Pupil 2020`, `Instruction - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))

# Remove the United States row, add a rank column, then add back the United States row to the top of the table
instruction_table_states <- instruction_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Instruction - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Instruction - Per Pupil 2020`,
    `2002` = `Instruction - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


instruction_table_us <- instruction_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Instruction - Per Pupil 2020`,
    `2002` = `Instruction - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


instruction_table <- rbind(instruction_table_us, instruction_table_states)


instruction_table <- instruction_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(instruction_table, "output_data/body/revenue_expenditure_trends/instruction_table.csv")




# Table 10: Employee Benefit Spending Per Student Growth (2002-2020)

employee_benefits_table <- ss_data_cpi |>
  filter(year == 2020) |>
  select(state, total_benefits_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, total_benefits_pp),
    by = "state"
  ) |>
  mutate(`Total Benefits - Per Pupil 2020` = `total_benefits_pp.x`) |>
  mutate(`Total Benefits - Per Pupil 2002` = `total_benefits_pp.y`) |>
  mutate(
    `Percent Change` = `Total Benefits - Per Pupil 2020` / `Total Benefits - Per Pupil 2002` - 1
  ) |>
  select(state, `Total Benefits - Per Pupil 2020`, `Total Benefits - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))

# Remove the United States row, add a rank column, then add back the United States row to the top of the table

employee_benefits_table_states <- employee_benefits_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Total Benefits - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Total Benefits - Per Pupil 2020`,
    `2002` = `Total Benefits - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


employee_benefits_table_us <- employee_benefits_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Benefits - Per Pupil 2020`,
    `2002` = `Total Benefits - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


employee_benefits_table <- rbind(employee_benefits_table_us, employee_benefits_table_states)


employee_benefits_table <- employee_benefits_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(employee_benefits_table, "output_data/body/revenue_expenditure_trends/employee_benefits_table.csv")






# Table 11: Capital Outlay Spending Per Student Growth (2002-2020)

capital_outlay_table <- ss_data_cpi |>
  filter(year == 2020) |>
  select(state, total_capital_outlay_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, total_capital_outlay_pp),
    by = "state"
  ) |>
  mutate(`Total Capital Outlays - Per Pupil 2020` = `total_capital_outlay_pp.x`) |>
  mutate(`Total Capital Outlays - Per Pupil 2002` = `total_capital_outlay_pp.y`) |>
  mutate(
    `Percent Change` = `Total Capital Outlays - Per Pupil 2020` / `Total Capital Outlays - Per Pupil 2002` - 1
  ) |>
  select(state, `Total Capital Outlays - Per Pupil 2020`, `Total Capital Outlays - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))

# Remove the United States row, add a rank column, then add back the United States row to the top of the table

capital_outlay_table_states <- capital_outlay_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Total Capital Outlays - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Total Capital Outlays - Per Pupil 2020`,
    `2002` = `Total Capital Outlays - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


capital_outlay_table_us <- capital_outlay_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Capital Outlays - Per Pupil 2020`,
    `2002` = `Total Capital Outlays - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


capital_outlay_table <- rbind(capital_outlay_table_us, capital_outlay_table_states)


capital_outlay_table <- capital_outlay_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(capital_outlay_table, "output_data/body/revenue_expenditure_trends/capital_outlay_table.csv")






# Table 12: Total Debt Per Student Growth (2002-2020)

total_debt_table <- ss_data_cpi |>
  filter(year == 2020) |>
  select(state, total_debt_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(state, total_debt_pp),
    by = "state"
  ) |>
  mutate(`Total Debt - Per Pupil 2020` = `total_debt_pp.x`) |>
  mutate(`Total Debt - Per Pupil 2002` = `total_debt_pp.y`) |>
  mutate(
    `Percent Change` = `Total Debt - Per Pupil 2020` / `Total Debt - Per Pupil 2002` - 1
  ) |>
  select(state, `Total Debt - Per Pupil 2020`, `Total Debt - Per Pupil 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

total_debt_table_states <- total_debt_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Total Debt - Per Pupil 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Total Debt - Per Pupil 2020`,
    `2002` = `Total Debt - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


total_debt_table_us <- total_debt_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Debt - Per Pupil 2020`,
    `2002` = `Total Debt - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


total_debt_table <- rbind(total_debt_table_us, total_debt_table_states)


total_debt_table <- total_debt_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(total_debt_table, "output_data/body/revenue_expenditure_trends/total_debt_table.csv")





# Table 13: Public School Enrollment Growth (2002-2020)

enrollment_table <- ss_data_cpi |>
  filter(year == 2020) |>
  rename(`Total Enrollment` = enrollment) |>
  select(state, `Total Enrollment`) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      rename(`Total Enrollment` = enrollment) |>
      select(state, `Total Enrollment`),
    by = "state"
  ) |>
  mutate(`Total Enrollment 2020` = `Total Enrollment.x`) |>
  mutate(`Total Enrollment 2002` = `Total Enrollment.y`) |>
  mutate(
    `Percent Change` = `Total Enrollment 2020` / `Total Enrollment 2002` - 1
  ) |>
  select(state, `Total Enrollment 2020`, `Total Enrollment 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

enrollment_table_states <- enrollment_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Total Enrollment 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Total Enrollment 2020`,
    `2002` = `Total Enrollment 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


enrollment_table_us <- enrollment_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Enrollment 2020`,
    `2002` = `Total Enrollment 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


enrollment_table <- rbind(enrollment_table_us, enrollment_table_states)


enrollment_table <- enrollment_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(enrollment_table, "output_data/body/enrollment_staffing_trends/enrollment_table.csv")





# Table 14: Non-Teaching Staff Growth (2002-2020)

staffing <- read_csv("input_data/teachers_non_teachers.csv")

staffing_table <- staffing |>
  filter(year == 2020) |>
  mutate(non_teaching_staff = all_staff - teachers) |>
  select(state, non_teaching_staff) |>
  left_join(
    staffing |>
      filter(year == 2002) |>
      mutate(non_teaching_staff = all_staff - teachers) |>
      select(state, non_teaching_staff),
    by = "state"
  ) |>
  mutate(`Non-Teaching Staff 2020` = `non_teaching_staff.x`) |>
  mutate(`Non-Teaching Staff 2002` = `non_teaching_staff.y`) |>
  mutate(
    `Percent Change` = `Non-Teaching Staff 2020` / `Non-Teaching Staff 2002` - 1
  ) |>
  select(state, `Non-Teaching Staff 2020`, `Non-Teaching Staff 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

staffing_table_states <- staffing_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  filter(state != "District of Columbia") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Non-Teaching Staff 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Non-Teaching Staff 2020`,
    `2002` = `Non-Teaching Staff 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


staffing_table_us <- staffing_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Non-Teaching Staff 2020`,
    `2002` = `Non-Teaching Staff 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`)


staffing_table <- rbind(staffing_table_us, staffing_table_states)


staffing_table <- staffing_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(staffing_table, "output_data/body/enrollment_staffing_trends/staffing_table.csv")





# Table 15: Teaching Staff Growth (2002-2020)

teachers <- read_csv("input_data/teachers_non_teachers.csv")

teachers_table <- teachers |>
  filter(year == 2020) |>
  select(state, teachers) |>
  left_join(
    teachers |>
      filter(year == 2002) |>
      select(state, teachers),
    by = "state"
  ) |>
  mutate(`Teachers 2020` = `teachers.x`) |>
  mutate(`Teachers 2002` = `teachers.y`) |>
  mutate(
    `Percent Change` = `Teachers 2020` / `Teachers 2002` - 1
  ) |>
  select(state, `Teachers 2020`, `Teachers 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

teachers_table_states <- teachers_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  filter(state != "District of Columbia") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Teachers 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Teachers 2020`,
    `2002` = `Teachers 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


teachers_table_us <- teachers_table |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Teachers 2020`,
    `2002` = `Teachers 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


teachers_table <- rbind(teachers_table_us, teachers_table_states)


teachers_table <- teachers_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(teachers_table, "output_data/body/enrollment_staffing_trends/teachers_table.csv")





# Table 16:  Average Teacher Salary Growth (2002-2020)

salary <- read_csv("input_data/salary_data.csv")

salary_pct <- salary |>
  filter(year != 2021) |>
  filter(state != "District of Columbia") |>
  group_by(state) |>
  mutate(`Salary Adj` = salary * last(cpi) / cpi) |>
  mutate(`Average Salary` = `Salary Adj`)

salary_pct_t <- salary_pct |>
  filter(year == 2020) |>
  select(state, `Average Salary`) |>
  left_join(
    salary_pct |>
      filter(year == 2002) |>
      select(state, `Average Salary`),
    by = "state"
  ) |>
  mutate(`Average Salary 2020` = `Average Salary.x`) |>
  mutate(`Average Salary 2002` = `Average Salary.y`) |>
  mutate(
    `Percent Change` = `Average Salary 2020` / `Average Salary 2002` - 1
  ) |>
  select(state, `Average Salary 2020`, `Average Salary 2002`, `Percent Change`) |>
  arrange(desc(`Percent Change`))


# Remove the United States row, add a rank column, then add back the United States row to the top of the table

salary_pct_states <- salary_pct_t |>
  as.data.frame() |>
  filter(state != "United States") |>
  arrange(desc(`Percent Change`)) |>
  mutate(`Growth Rank` = row_number()) |>
  arrange(desc(`Average Salary 2020`)) |>
  mutate(`2020 Rank` = row_number()) |>
  rename(
    `2020` = `Average Salary 2020`,
    `2002` = `Average Salary 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  arrange(desc(`Growth`))


salary_pct_us <- salary_pct_t |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Average Salary 2020`,
    `2002` = `Average Salary 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2002`, `2020`, `Growth`)


salary_pct_table <- rbind(salary_pct_us, salary_pct_states)


salary_pct_table <- salary_pct_table |>
  # Round values
  mutate(`2020` = round(`2020`, 0)) |>
  mutate(`2002` = round(`2002`, 0)) |>
  mutate(`Growth` = round(`Growth`, 3))


# Write csv
write_csv(salary_pct_table, "output_data/body/enrollment_staffing_trends/salary_pct.csv")






# NAEP Scores

naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_ts.csv")
naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_ts.csv")
naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_ts.csv")
naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_ts.csv")

str(naep_math_4)

# Table 17: 4th Grade NAEP Reading Score Growth (2003-2019)

naep_reading_4_table <- naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Reading Score Diff`) |>
  rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`) |>
  arrange(desc(`Reading Score Diff`))

# Remove United States, calculate 2019 rank, "Reading Score Pct" rank, then add back the United States at the top
naep_reading_4_table_states <- naep_reading_4_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Reading Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Reading Score 2019`)) |>
  mutate(`Reading Score 2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`Reading Score 2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `2019` = `Reading Score 2019`,
    `2003` = `Reading Score 2003`,
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_reading_4_table_us <- naep_reading_4_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_reading_4_table <- rbind(naep_reading_4_table_us, naep_reading_4_table_states)


naep_reading_4_table <- naep_reading_4_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(naep_reading_4_table, "output_data/body/student_outcomes/naep_reading_4_table.csv")





# Table 18: 4th Grade NAEP Math Score Growth (2003-2019)

naep_math_4_table <- naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_4 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Math Score Diff`) |>
  rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`) |>
  arrange(desc(`Math Score Diff`))

# Remove United States, calculate 2019 rank, "Math Score Pct" rank, then add back the United States at the top

naep_math_4_table_states <- naep_math_4_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Math Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Math Score 2019`)) |>
  mutate(`Math Score 2019` = round(`Math Score 2019`, 0)) |>
  mutate(`Math Score 2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `2019` = `Math Score 2019`,
    `2003` = `Math Score 2003`,
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_math_4_table_us <- naep_math_4_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Math Score 2019`, 0)) |>
  mutate(`2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


naep_math_4_table <- rbind(naep_math_4_table_us, naep_math_4_table_states)


naep_math_4_table <- naep_math_4_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(naep_math_4_table, "output_data/body/student_outcomes/naep_math_4_table.csv")






# Table 19: 8th Grade NAEP Reading Score Growth (2003-2019)

naep_reading_8_table <- naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Reading Score Diff`) |>
  rename(`Reading Score 2019` = `NAEP.x`, `Reading Score 2003` = `NAEP.y`) |>
  arrange(desc(`Reading Score Diff`))

# Remove United States, calculate 2019 rank, "Reading Score Pct" rank, then add back the United States at the top

naep_reading_8_table_states <- naep_reading_8_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Reading Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Reading Score 2019`)) |>
  mutate(`Reading Score 2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`Reading Score 2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `2019` = `Reading Score 2019`,
    `2003` = `Reading Score 2003`,
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_reading_8_table_us <- naep_reading_8_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_reading_8_table <- rbind(naep_reading_8_table_us, naep_reading_8_table_states)


naep_reading_8_table <- naep_reading_8_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(naep_reading_8_table, "output_data/body/student_outcomes/naep_reading_8_table.csv")



# Table 20: 8th Grade NAEP Math Score Growth (2003-2019)

naep_math_8_table <- naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "DoDEA") |>
  filter(State != "Puerto Rico") |>
  select(State, `NAEP`) |>
  left_join(
    naep_math_8 |>
      filter(Year == 2003) |>
      select(State, NAEP),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `NAEP.x` - `NAEP.y`) |>
  select(State, `NAEP.x`, `NAEP.y`, `Math Score Diff`) |>
  rename(`Math Score 2019` = `NAEP.x`, `Math Score 2003` = `NAEP.y`) |>
  arrange(desc(`Math Score Diff`))


# Remove United States, calculate 2019 rank, "Math Score Pct" rank, then add back the United States at the top

naep_math_8_table_states <- naep_math_8_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Math Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Math Score 2019`)) |>
  mutate(`Math Score 2019` = round(`Math Score 2019`, 0)) |>
  mutate(`Math Score 2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `2019` = `Math Score 2019`,
    `2003` = `Math Score 2003`,
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

naep_math_8_table_us <- naep_math_8_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Math Score 2019`, 0)) |>
  mutate(`2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


naep_math_8_table <- rbind(naep_math_8_table_us, naep_math_8_table_states)


naep_math_8_table <- naep_math_8_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(naep_math_8_table, "output_data/body/student_outcomes/naep_math_8_table.csv")




# Table 21: 4th Grade FRL NAEP Reading Score Growth (2003-2019)

# Load data
low_income_naep_math_4 <- read_csv("input_data/raw_naep/naep_4_math_low_income_ts.csv")
low_income_naep_reading_4 <- read_csv("input_data/raw_naep/naep_4_reading_low_income_ts.csv")
low_income_naep_math_8 <- read_csv("input_data/raw_naep/naep_8_math_low_income_ts.csv")
low_income_naep_reading_8 <- read_csv("input_data/raw_naep/naep_8_reading_low_income_ts.csv")

# Create a table for 4th Grade NAEP Reading Score Growth (2003-2019)
low_income_naep_reading_4 <- low_income_naep_reading_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_4_table <- low_income_naep_reading_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Reading Score Diff`) |>
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`) |>
  arrange(desc(`Reading Score Diff`))


# Remove United States, calculate 2019 rank, "Reading Score Pct" rank, then add back the United States at the top
low_income_naep_reading_4_table_states <- low_income_naep_reading_4_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Reading Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Reading Score 2019`)) |>
  mutate(`Reading Score 2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`Reading Score 2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `2019` = `Reading Score 2019`,
    `2003` = `Reading Score 2003`,
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )

low_income_naep_reading_4_table_us <- low_income_naep_reading_4_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_reading_4_table <- rbind(low_income_naep_reading_4_table_us, low_income_naep_reading_4_table_states)


low_income_naep_reading_4_table <- low_income_naep_reading_4_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(low_income_naep_reading_4_table, "output_data/body/student_outcomes/low_income_naep_reading_4_table.csv")


# Table 22: 4th Grade FRL NAEP Math Score Growth (2003-2019)

low_income_naep_math_4 <- low_income_naep_math_4 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_4_table <- low_income_naep_math_4 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_4 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Math Score Diff`) |>
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`) |>
  arrange(desc(`Math Score Diff`))


# Remove United States, calculate 2019 rank, "Reading Score Pct" rank, then add back the United States at the top

low_income_naep_math_4_table_states <- low_income_naep_math_4_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Math Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Math Score 2019`)) |>
  mutate(`Math Score 2019` = round(`Math Score 2019`, 0)) |>
  mutate(`Math Score 2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `2019` = `Math Score 2019`,
    `2003` = `Math Score 2003`,
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_math_4_table_us <- low_income_naep_math_4_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Math Score 2019`, 0)) |>
  mutate(`2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_math_4_table <- rbind(low_income_naep_math_4_table_us, low_income_naep_math_4_table_states)


low_income_naep_math_4_table <- low_income_naep_math_4_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(low_income_naep_math_4_table, "output_data/body/student_outcomes/low_income_naep_math_4_table.csv")



# Table 23: 8th Grade FRL NAEP Reading Score Growth (2003-2019)

low_income_naep_reading_8 <- low_income_naep_reading_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_reading_8_table <- low_income_naep_reading_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_reading_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Reading Score Diff`) |>
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`) |>
  arrange(desc(`Reading Score Diff`))


# Remove United States, calculate 2019 rank, "Reading Score Pct" rank, then add back the United States at the top

low_income_naep_reading_8_table_states <- low_income_naep_reading_8_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Reading Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Reading Score 2019`)) |>
  mutate(`Reading Score 2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`Reading Score 2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `2019` = `Reading Score 2019`,
    `2003` = `Reading Score 2003`,
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_reading_8_table_us <- low_income_naep_reading_8_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Reading Score 2019`, 0)) |>
  mutate(`2003` = round(`Reading Score 2003`, 0)) |>
  rename(
    `Growth` = `Reading Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_reading_8_table <- rbind(low_income_naep_reading_8_table_us, low_income_naep_reading_8_table_states)


low_income_naep_reading_8_table <- low_income_naep_reading_8_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(low_income_naep_reading_8_table, "output_data/body/student_outcomes/low_income_naep_reading_8_table.csv")


# Table 24: 8th Grade FRL NAEP Math Score Growth (2003-2019)

low_income_naep_math_8 <- low_income_naep_math_8 |>
  filter(State != "DoDEA") |>
  mutate(Eligible = as.numeric(NAEP))

low_income_naep_math_8_table <- low_income_naep_math_8 |>
  filter(Year == 2019) |>
  filter(State != "District of Columbia") |>
  filter(State != "Puerto Rico") |>
  select(State, `Eligible`) |>
  left_join(
    low_income_naep_math_8 |>
      filter(Year == 2003) |>
      select(State, Eligible),
    by = "State"
  ) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  select(State, `Eligible.x`, `Eligible.y`, `Math Score Diff`) |>
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`) |>
  arrange(desc(`Math Score Diff`))


# Remove United States, calculate 2019 rank, "Math Score Pct" rank, then add back the United States at the top

low_income_naep_math_8_table_states <- low_income_naep_math_8_table |>
  filter(State != "United States") |>
  mutate(`Growth Rank` = rank(-`Math Score Diff`)) |>
  mutate(`2019 Rank` = rank(-`Math Score 2019`)) |>
  mutate(`Math Score 2019` = round(`Math Score 2019`, 0)) |>
  mutate(`Math Score 2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `2019` = `Math Score 2019`,
    `2003` = `Math Score 2003`,
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_math_8_table_us <- low_income_naep_math_8_table |>
  filter(State == "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2019 Rank` = " "
  ) |>
  mutate(`2019` = round(`Math Score 2019`, 0)) |>
  mutate(`2003` = round(`Math Score 2003`, 0)) |>
  rename(
    `Growth` = `Math Score Diff`
  ) |>
  select(
    `Growth Rank`,
    `2019 Rank`,
    State,
    `2003`,
    `2019`,
    `Growth`
  )


low_income_naep_math_8_table <- rbind(low_income_naep_math_8_table_us, low_income_naep_math_8_table_states)


low_income_naep_math_8_table <- low_income_naep_math_8_table |>
  mutate(Growth = round(Growth, 0))


# Write csv
write_csv(low_income_naep_math_8_table, "output_data/body/student_outcomes/low_income_naep_math_8_table.csv")


