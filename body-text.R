################################################################################
############################ EXECUTIVE SUMMARY #################################
################################################################################

############################### Page 1 #########################################

# Pull "United States" and get the percentage change from 2020
# `Total Revenue - Per Pupil` and 2002 `Total Revenue - Per Pupil`
us_2020 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_revenue_pp)

us_2002 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_revenue_pp)

# Difference
us_2020
us_2002
us_2020[1, 2] / us_2002[1, 2] - 1

# Pull "United States" and get the 2020 `Total Benefits - Per Pupil`and 2002
# `Total Benefits - Per Pupil`
us_2020_benefits <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_benefits_pp)

us_2002_benefits <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_benefits_pp)

# Calculate the difference between us_2020_benefits and us_2002_benefits
us_2020_benefits[1, 2] - us_2002_benefits[1, 2]
us_2020_benefits[1, 2] / us_2002_benefits[1, 2] - 1


# Enrollment

# Pull "United States" and get the 2020 `Enrollment` and 2002 `Enrollment`
us_2020_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(enrollment)

us_2002_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(enrollment)

# Calculate the percent change between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] / us_2002_enrollment[1, 2] - 1

# Get `All staff` for 2020 and 2002 from teachers_non_teachers
teachers_non_teachers_2020 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(all_staff)

teachers_non_teachers_2002 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(all_staff)

# Calculate the percent change between teachers_non_teachers_2020 and teachers_non_teachers_2002
teachers_non_teachers_2020[1, 1] / teachers_non_teachers_2002[1, 1] - 1

# Get Non-Teachers from `All staff` - `Teachers` for 2020 and 2002 from staffing
non_teachers_2020 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(all_staff, teachers) |>
  mutate(`Non-Teachers` = all_staff - teachers)

non_teachers_2002 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(all_staff, teachers) |>
  mutate(non_teachers = all_staff - teachers)

# Calculate the percent change between non_teachers_2020 and non_teachers_2002
non_teachers_2020[1, 1] / non_teachers_2002[1, 1] - 1


#  Get average salary for teachers in the "United States" for 2020 and 2002
teachers_salary_2020 <- salary_pct |>
  filter(state == "United States") |>
  filter(year == 2020 | year == 2002)

teachers_salary_2020



############################## Key Trend #1 ####################################

# Count the number of rows in rev_table where Growth is greater than 0
rev_table |>
  filter(state != "United States") |>
  filter(Growth > 0) |>
  count()

# Look and count the number of rows in rev_table where Growth is greater than 0.5
rev_table |>
  filter(state != "United States") |>
  filter(Growth > 0.5) |>
  count()

rev_table |>
  filter(state != "United States") |>
  filter(Growth > 0.5)

# Look at the number of rows in rev_table where revenue exceeds $20,000
rev_table |>
  filter(state != "United States") |>
  filter(`2020` > 20e3)



############################## Key Trend #2 ####################################

rev_table_key_trend_2 <- rev_table |>
  filter(state != "United States") |>
  arrange(-Growth) |>
  filter(as.numeric(`Growth Rank`) < 11)


# Left join rev_table_key_trend_2 and salary_pct by "State"
salary_pct_key_trend_2 <- salary_pct_table
names(salary_pct_key_trend_2) <- c("Salary Growth Rank", "2020 Salary Rank", "state", "2002 Salary", "2020 Salary", "Salary Growth")

rev_table_key_trend_2 <- left_join(
  rev_table_key_trend_2,
  salary_pct_key_trend_2,
  by = "state"
) |>
  mutate(`Salary Drop Flag` = ifelse(
    `2020 Salary` < `2002 Salary`,
    1,
    0
  )) |>
  mutate(
    `Revenue Change` = `2020` - `2002`,
    `Salary Change` = `2020 Salary` - `2002 Salary`
  )

rev_table_key_trend_2



############################## Key Trend #3 ####################################

# Staffing / Enrollment Changes

staff_greater_enrollment <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  filter(state != "United States") |>
  mutate(`Staff_Greater_Enrollment` = ifelse(`Total Staff Growth` > `Enrollment Growth`, 1, 0)) |>
  # Get the sum of `Staff_Greater_Enrollment`
  as.data.frame() |>
  summarise(`Staff_Greater_Enrollment` = sum(`Staff_Greater_Enrollment`))

staff_greater_enrollment


non_teachers_growth <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  filter(state == "United States")

non_teachers_growth

# Calulate the enrollment loss and staff growth in Pennsylvania and Connecticut
pa_ca_enrollment_staffing <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  filter(state == "Pennsylvania" | state == "Connecticut")

pa_ca_enrollment_staffing



############################## Key Trend #4 ####################################

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


benefits_revenue_states <- ss_data_cpi |>
  filter(year == 2020) |>
  filter(state != "United States") |>
  select(state, total_revenue_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      filter(state != "United States") |>
      select(state, total_revenue_pp),
    by = "state"
  ) |>
  mutate(`Total Revenue - Per Pupil 2020` = `total_revenue_pp.x`) |>
  mutate(`Total Revenue - Per Pupil 2002` = `total_revenue_pp.y`) |>
  mutate(
    `Revenue Difference` = `Total Revenue - Per Pupil 2020` - `Total Revenue - Per Pupil 2002`
  ) |>
  left_join(
    employee_benefits_table,
    by = "state"
  ) |>
  mutate(
    `Benefits Difference` = `Total Benefits - Per Pupil 2020` - `Total Benefits - Per Pupil 2002`,
    `Benefits Difference as a Percent of Revenue Difference` = `Benefits Difference` / `Revenue Difference`
  ) |>
  select(state, `Total Revenue - Per Pupil 2020`, `Total Revenue - Per Pupil 2002`, `Revenue Difference`, `Total Benefits - Per Pupil 2020`, `Total Benefits - Per Pupil 2002`, `Benefits Difference`, `Benefits Difference as a Percent of Revenue Difference`)

benefits_revenue_states |>
  filter(state == "Hawaii")


# Key Trend #5

# 4th Grade Reading NAEP Scores
naep_reading_4_states <- naep_reading_4 |>
  filter(Year == 2019) |>
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

# 4th Grade Math NAEP Scores
naep_math_4_states <- naep_math_4 |>
  filter(Year == 2019) |>
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


# 8th Grade Reading NAEP Scores
naep_reading_8_states <- naep_reading_8 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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

# 8th Grade Math NAEP Scores
naep_math_8_states <- naep_math_8 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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



# 4th Grade Low Income Reading NAEP Scores
li_naep_reading_4_states <- low_income_naep_reading_4 |>
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



# 4th Grade Low Inocme Math NAEP Scores
li_naep_math_4_states <- low_income_naep_math_4 |>
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



# 8th Grade Low Income Reading NAEP Scores
li_naep_reading_8_states <- low_income_naep_reading_8 |>
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


# 8th Grade Low Income Math NAEP Scores
li_naep_math_8_states <- low_income_naep_math_8 |>
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






################################################################################
############################# INTRODUCTION #####################################
################################################################################

enrollment_table

################################################################################
################## ANALYSIS: NATIONAL SNAPSHOT AND KEY TRENDS ##################
################################################################################


# Pull "United States" and get the 2020 `Total Revenue - Per Pupil` and 2002 `Total Revenue - Per Pupil`
us_2020 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_revenue_pp)

us_2002 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_revenue_pp)

us_2020
us_2002



# Pull "United States" and get the 2020 `Enrollment` and 2002 `Enrollment`
us_2020_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(enrollment)

us_2002_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(enrollment)

# Pull "United States" and get the 2020 `Total Benefits - Per Pupil`and 2002
# `Total Benefits - Per Pupil`
us_2020_benefits <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_benefits_pp)

us_2002_benefits <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_benefits_pp)

# Calculate the difference between us_2020_benefits and us_2002_benefits
us_2020_benefits[1, 2] - us_2002_benefits[1, 2]
us_2020_benefits[1, 2] / us_2002_benefits[1, 2] - 1

# Calculate the difference between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] - us_2002_enrollment[1, 2]

# Calculate the percent change between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] / us_2002_enrollment[1, 2] - 1

# Get `All staff` for 2020 and 2002 from teachers_non_teachers
teachers_non_teachers_2020 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(all_staff)

teachers_non_teachers_2002 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(all_staff)


# Calculate the difference between teachers_non_teachers_2020 and teachers_non_teachers_2002
teachers_non_teachers_2020[1, 1] - teachers_non_teachers_2002[1, 1]

# Calculate the percent change between teachers_non_teachers_2020 and teachers_non_teachers_2002
teachers_non_teachers_2020[1, 1] / teachers_non_teachers_2002[1, 1] - 1


# Get Non-Teachers from `All staff` - `Teachers` for 2020 and 2002 from staffing
non_teachers_2020 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(all_staff, teachers) |>
  mutate(non_teachers = all_staff - teachers)

non_teachers_2002 <- staffing |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(all_staff, teachers) |>
  mutate(non_teachers = all_staff - teachers)

# Calculate the difference between non_teachers_2020 and non_teachers_2002
non_teachers_2020[1, 3] - non_teachers_2002[1, 3]

(non_teachers_2020[1, 3] - non_teachers_2002[1, 3]) / (teachers_non_teachers_2020[1, 1] - teachers_non_teachers_2002[1, 1])

non_teachers_2020[1, 3] / non_teachers_2020[1, 1]

# Get `Salary Adj` for 2020 and 2002 for the "United States" from salary_pct

us_2020_salary <- salary_pct |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(`Salary Adj`)

us_2002_salary <- salary_pct |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(`Salary Adj`)

us_2020_salary
us_2002_salary




########################### Key Trends #1 ######################################

# Count the number of rows in rev_table where Growth is greater than 0
rev_table |>
  filter(state != "United States") |>
  filter(Growth > 0) |>
  count()

ss_data_cpi |>
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
  mutate(greater_than_50pct = ifelse(`Percent Change` > 0.5, 1, 0)) |>
  filter(greater_than_50pct == 1)


# Get the `Federal Revenue - Per Pupil`, `State Revenue - Per Pupil`, `Local Revenue - Per Pupil`, `Total Revenue - Per Pupil` for 2020 and 2002 for the "United States"
us_2020_rev <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(federal_revenue_pp, state_revenue_pp, local_revenue_pp, total_revenue_pp)

us_2002_rev <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(federal_revenue_pp, state_revenue_pp, local_revenue_pp, total_revenue_pp)

# Calculate the difference between us_2020_rev$`Federal Revenue - Per Pupil` and us_2002_rev$`Federal Revenue - Per Pupil`
us_2020_rev$federal_revenue_pp - us_2002_rev$federal_revenue_pp

# Calculate the percent change between us_2020_rev$`Federal Revenue - Per Pupil` and us_2002_rev$`Federal Revenue - Per Pupil`
us_2020_rev$federal_revenue_pp / us_2002_rev$federal_revenue_pp - 1

# Calculate the difference between us_2020_rev$`State Revenue - Per Pupil` and us_2002_rev$`State Revenue - Per Pupil`
us_2020_rev$state_revenue_pp - us_2002_rev$state_revenue_pp

# Calculate the percent change between us_2020_rev$`State Revenue - Per Pupil` and us_2002_rev$`State Revenue - Per Pupil`
us_2020_rev$state_revenue_pp / us_2002_rev$state_revenue_pp - 1

# Calculate the difference between us_2020_rev$`Local Revenue - Per Pupil` and us_2002_rev$`Local Revenue - Per Pupil`
us_2020_rev$local_revenue_pp - us_2002_rev$local_revenue_pp

# Calculate the percent change between us_2020_rev$`Local Revenue - Per Pupil` and us_2002_rev$`Local Revenue - Per Pupil`
us_2020_rev$local_revenue_pp / us_2002_rev$local_revenue_pp - 1

ss_data_cpi |>
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
  filter(`Total Revenue - Per Pupil 2020` > 20e3) |>
  arrange(desc(`Total Revenue - Per Pupil 2020`))




############################## Key Trend #2 ####################################

# Pull "United States" and get the 2020 `Total Revenue - Per Pupil` and 2002 `Total Revenue - Per Pupil`
us_2020 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_revenue_pp)

us_2002 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_revenue_pp)

us_2020[1,2] / us_2002[1,2]-1


# United States
us_rev_table_key_trend_2 <- rev_table |>
  filter(state == "United States") |>
  arrange(-Growth)

rev_table_key_trend_2 <- rev_table |>
  filter(state != "United States") |>
  arrange(-Growth) |>
  filter(as.numeric(`Growth Rank`) < 11)

# Left join rev_table_key_trend_2 and salary_pct by "State"
salary_pct_key_trend_2 <- salary_pct_table
names(salary_pct_key_trend_2) <- c("Salary Growth Rank", "2020 Salary Rank", "state", "2002 Salary", "2020 Salary", "Salary Growth")



us_rev_table_key_trend_2 <- left_join(
  us_rev_table_key_trend_2,
  salary_pct_key_trend_2,
  by = "state"
) |>
  mutate(`Salary Drop Flag` = ifelse(
    `2020 Salary` < `2002 Salary`,
    1,
    0
  )) |>
  mutate(
    `Revenue Change` = `2020` - `2002`,
    `Salary Change` = `2020 Salary` - `2002 Salary`
  )

us_rev_table_key_trend_2

rev_table_key_trend_2 <- left_join(
  rev_table_key_trend_2,
  salary_pct_key_trend_2,
  by = "state"
) |>
  mutate(`Salary Drop Flag` = ifelse(
    `2020 Salary` < `2002 Salary`,
    1,
    0
  )) |>
  mutate(
    `Revenue Change` = `2020` - `2002`,
    `Salary Change` = `2020 Salary` - `2002 Salary`
  )

rev_table_key_trend_2



############################## Key Trend #3 ####################################

# Pull "United States" and get the 2020 `Enrollment` and 2002 `Enrollment`
us_2020_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(enrollment)

us_2002_enrollment <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(enrollment)


# Calculate the percent change between us_2020_enrollment and us_2002_enrollment
us_2020_enrollment[1, 2] / us_2002_enrollment[1, 2] - 1


# Staffing / Enrollment Changes
staff_greater_enrollment <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  filter(state != "United States") |>
  mutate(`Staff_Greater_Enrollment` = ifelse(`Total Staff Growth` > `Enrollment Growth`, 1, 0)) |>
  # Get the sum of `Staff_Greater_Enrollment`
  as.data.frame() |>
  summarise(`Staff_Greater_Enrollment` = sum(`Staff_Greater_Enrollment`))

staff_greater_enrollment


non_teachers_growth <- enrollment_trends |>
  left_join(staffing_trends, by = "state") |>
  arrange(`Enrollment Growth`) |>
  select(state, `Enrollment 2020`, `Enrollment 2002`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  mutate_at(vars(`Enrollment Growth`, `Teachers Growth`, `Non-Teachers Growth`, `Total Staff Growth`), round, 3) |>
  select(state, `Enrollment 2002`, `Enrollment 2020`, `Enrollment Growth`, `Total Staff Growth`, `Non-Teachers Growth`, `Teachers Growth`) |>
  filter(state == "United States")

non_teachers_growth


# Load Complete US Staffing Data

complete_staffing <- read_csv("input_data/complete_staffing.csv")

# Calculate `Other support services staff ` as a percent of non-teachers
complete_staffing_total <- complete_staffing |>
  as.data.frame() |>
  filter(Category != "Teachers") |>
  summarise(
    `2002` = sum(`2002`),
    `2020` = sum(`2020`)
  )


complete_staffing_total[1, 2] / complete_staffing_total[1, 1] - 1

(complete_staffing[11, 3] + complete_staffing[7, 3])/ complete_staffing_total[1, 2]

(complete_staffing[1, 3] + complete_staffing[2, 3] + complete_staffing[3, 3])/ complete_staffing_total[1, 2]




############################## Key Trend #4 ####################################

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

employee_benefits_table |>
  filter(state == "United States") |>
  mutate(Difference = `Total Benefits - Per Pupil 2020` - `Total Benefits - Per Pupil 2002`)


rev_benefit_trends

############################## Key Trend #5 ####################################

# Reference values are generated by Table 6 in in body-table.R


################################################################################
######################### REVENUE AND EXPENDITURE TRENDS #######################
################################################################################

us_2020 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2020) |>
  select(total_revenue_pp)

us_2002 <- ss_data_cpi |>
  filter(state == "United States") |>
  filter(year == 2002) |>
  select(total_revenue_pp)

# Difference
us_2020[1, 2] / us_2002[1, 2] - 1
us_2020[1, 2] - us_2002[1, 2]


ss_data_cpi |>
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
  mutate(greater_than_10pct = ifelse(`Percent Change` >= 0.1, 1, 0)) |>
  as.data.frame() |>
  summarise(
    `Number of States` = sum(greater_than_10pct)
  )


ss_data_cpi |>
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
  filter(`Percent Change` > 0.5) |>
  arrange(desc(`Total Revenue - Per Pupil 2020`))

ss_data_cpi |>
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
  filter(`Total Revenue - Per Pupil 2020` > 20e3) |>
  arrange(desc(`Total Revenue - Per Pupil 2020`))


ss_data_cpi |>
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
  arrange(`Total Revenue - Per Pupil 2020`)



support_services_us <- ss_data_cpi |>
  filter(year == 2020) |>
  filter(state == "United States") |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      filter(state == "United States") |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  )


support_services_us_pct <- support_services_us |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  mutate(`Pupil Support Services Growth` = `ss_pupil_support_services_pp.x` / `ss_pupil_support_services_pp.y` - 1) |>
  mutate(`Instructional Staff Growth` = `ss_instructional_staff_support_services_pp.x` / `ss_instructional_staff_support_services_pp.y` - 1) |>
  mutate(`General Administration Growth` = `ss_general_administration_pp.x` / `ss_general_administration_pp.y` - 1) |>
  mutate(`School Administration Growth` = `ss_school_administration_pp.x` / `ss_school_administration_pp.y` - 1) |>
  mutate(`Operation & Maintenance Growth` = `ss_operation_and_maintenance_of_plant_pp.x` / `ss_operation_and_maintenance_of_plant_pp.y` - 1) |>
  mutate(`Pupil Transportation Growth` = `ss_pupil_transportation_pp.x` / `ss_pupil_transportation_pp.y` - 1) |>
  mutate(`Other Growth` = `ss_other_nonspecified_support_services_pp.x` / `ss_other_nonspecified_support_services_pp.y` - 1) |>
  select(
    state,
    `Total Growth`,
    `Pupil Support Services Growth`,
    `Instructional Staff Growth`,
    `General Administration Growth`,
    `School Administration Growth`,
    `Operation & Maintenance Growth`,
    `Pupil Transportation Growth`,
    `Other Growth`
  )


support_serivces_us_diff <- support_services_us |>
  mutate(`Total Diff` = `Total.x` - `Total.y`) |>
  mutate(`Pupil Support Services Diff` = `ss_pupil_support_services_pp.x` - `ss_pupil_support_services_pp.y`) |>
  mutate(`Instructional Staff Diff` = `ss_instructional_staff_support_services_pp.x` - `ss_instructional_staff_support_services_pp.y`) |>
  mutate(`General Administration Diff` = `ss_general_administration_pp.x` - `ss_general_administration_pp.y`) |>
  mutate(`School Administration Diff` = `ss_school_administration_pp.x` - `ss_school_administration_pp.y`) |>
  mutate(`Operation & Maintenance Diff` = `ss_operation_and_maintenance_of_plant_pp.x` - `ss_operation_and_maintenance_of_plant_pp.y`) |>
  mutate(`Pupil Transportation Diff` = `ss_pupil_transportation_pp.x` - `ss_pupil_transportation_pp.y`) |>
  mutate(`Other Diff` = `ss_other_nonspecified_support_services_pp.x` - `ss_other_nonspecified_support_services_pp.y`) |>
  select(
    state,
    `Total Diff`,
    `Pupil Support Services Diff`,
    `Instructional Staff Diff`,
    `General Administration Diff`,
    `School Administration Diff`,
    `Operation & Maintenance Diff`,
    `Pupil Transportation Diff`,
    `Other Diff`
  )


support_services_us_pct |>
  t()

ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  ) |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  filter(`Total Growth` >= 0.5)


ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  ) |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  filter(`Total Growth` >= 0.6)


ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  ) |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  filter(`Total Growth` <= 0.1)


ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  ) |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  filter(`Total Growth` <= 0)


support_services_states <- ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    ss_pupil_support_services_pp,
    ss_instructional_staff_support_services_pp,
    ss_general_administration_pp,
    ss_school_administration_pp,
    ss_operation_and_maintenance_of_plant_pp,
    ss_pupil_transportation_pp,
    ss_other_nonspecified_support_services_pp,
    all_support_services_pp
  ) |>
  mutate(`Total` = all_support_services_pp) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        ss_pupil_support_services_pp,
        ss_instructional_staff_support_services_pp,
        ss_general_administration_pp,
        ss_school_administration_pp,
        ss_operation_and_maintenance_of_plant_pp,
        ss_pupil_transportation_pp,
        ss_other_nonspecified_support_services_pp,
        all_support_services_pp
      ) |>
      mutate(`Total` = all_support_services_pp),
    by = "state"
  ) |>
  mutate(`Total Growth` = `Total.x` / `Total.y` - 1) |>
  mutate(`Pupil Support Services Growth` = `ss_pupil_support_services_pp.x` / `ss_pupil_support_services_pp.y` - 1) |>
  mutate(`Instructional Staff Growth` = `ss_instructional_staff_support_services_pp.x` / `ss_instructional_staff_support_services_pp.y` - 1) |>
  mutate(`General Administration Growth` = `ss_general_administration_pp.x` / `ss_general_administration_pp.y` - 1) |>
  mutate(`School Administration Growth` = `ss_school_administration_pp.x` / `ss_school_administration_pp.y` - 1) |>
  mutate(`Operation & Maintenance Growth` = `ss_operation_and_maintenance_of_plant_pp.x` / `ss_operation_and_maintenance_of_plant_pp.y` - 1) |>
  mutate(`Pupil Transportation Growth` = `ss_pupil_transportation_pp.x` / `ss_pupil_transportation_pp.y` - 1) |>
  mutate(`Other Growth` = `ss_other_nonspecified_support_services_pp.x` / `ss_other_nonspecified_support_services_pp.y` - 1) |>
  mutate(`Total Diff` = `Total.x` - `Total.y`) |>
  mutate(`Pupil Support Services Diff` = `ss_pupil_support_services_pp.x` - `ss_pupil_support_services_pp.y`) |>
  mutate(`Instructional Staff Diff` = `ss_instructional_staff_support_services_pp.x` - `ss_instructional_staff_support_services_pp.y`) |>
  mutate(`General Administration Diff` = `ss_general_administration_pp.x` - `ss_general_administration_pp.y`) |>
  mutate(`School Administration Diff` = `ss_school_administration_pp.x` - `ss_school_administration_pp.y`) |>
  mutate(`Operation & Maintenance Diff` = `ss_operation_and_maintenance_of_plant_pp.x` - `ss_operation_and_maintenance_of_plant_pp.y`) |>
  mutate(`Pupil Transportation Diff` = `ss_pupil_transportation_pp.x` - `ss_pupil_transportation_pp.y`) |>
  mutate(`Other Diff` = `ss_other_nonspecified_support_services_pp.x` - `ss_other_nonspecified_support_services_pp.y`) |>
  # 2020 values
  mutate(`Total 2020` = `Total.x`) |>
  mutate(`Pupil Support Services 2020` = `ss_pupil_support_services_pp.x`) |>
  mutate(`Instructional Staff 2020` = `ss_instructional_staff_support_services_pp.x`) |>
  mutate(`General Administration 2020` = `ss_general_administration_pp.x`) |>
  mutate(`School Administration 2020` = `ss_school_administration_pp.x`) |>
  mutate(`Operation & Maintenance 2020` = `ss_operation_and_maintenance_of_plant_pp.x`) |>
  mutate(`Pupil Transportation 2020` = `ss_pupil_transportation_pp.x`) |>
  mutate(`Other 2020` = `ss_other_nonspecified_support_services_pp.x`) |>
  select(
    state,
    `Total 2020`,
    `Pupil Support Services 2020`,
    `Instructional Staff 2020`,
    `General Administration 2020`,
    `School Administration 2020`,
    `Operation & Maintenance 2020`,
    `Pupil Transportation 2020`,
    `Other 2020`,
    `Total Growth`,
    `Pupil Support Services Growth`,
    `Instructional Staff Growth`,
    `General Administration Growth`,
    `School Administration Growth`,
    `Operation & Maintenance Growth`,
    `Pupil Transportation Growth`,
    `Other Growth`,
    `Total Diff`,
    `Pupil Support Services Diff`,
    `Instructional Staff Diff`,
    `General Administration Diff`,
    `School Administration Diff`,
    `Operation & Maintenance Diff`,
    `Pupil Transportation Diff`,
    `Other Diff`
  )


support_services_pupil_support_services <- support_services_states |>
  arrange(desc(`Total 2020`))


support_services_pupil_support_services




# Get `All Instruction - Per Pupil` data from ss_data_cpi for 2002 and 2020
instructional_states <- ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    all_instruction_pp
  ) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        all_instruction_pp
      ),
    by = "state"
  ) |>
  rename(`Instructional 2020` = `all_instruction_pp.x`) |>
  rename(`Instructional 2002` = `all_instruction_pp.y`) |>
  mutate(`Instructional Growth` = `Instructional 2020` / `Instructional 2002` - 1)


instructional_states_us <- instructional_states |>
  filter(state == "United States") |>
  mutate(`Instructional Difference` = `Instructional 2020` - `Instructional 2002`)


ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    all_instruction_pp
  ) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        all_instruction_pp      ),
    by = "state"
  ) |>
  rename(`Instructional 2020` = `all_instruction_pp.x`) |>
  rename(`Instructional 2002` = `all_instruction_pp.y`) |>
  mutate(`Instructional Growth` = `Instructional 2020` / `Instructional 2002` - 1) |>
  filter(`Instructional Growth` >= 0.5)

ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    all_instruction_pp
  ) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        all_instruction_pp
      ),
    by = "state"
  ) |>
  rename(`Instructional 2020` = `all_instruction_pp.x`) |>
  rename(`Instructional 2002` = `all_instruction_pp.y`) |>
  mutate(`Instructional Growth` = `Instructional 2020` / `Instructional 2002` - 1) |>
  filter(`Instructional Growth` < 0)

ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    all_instruction_pp
  ) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        all_instruction_pp
      ),
    by = "state"
  ) |>
  rename(`Instructional 2020` = `all_instruction_pp.x`) |>
  rename(`Instructional 2002` = `all_instruction_pp.y`) |>
  filter(`Instructional 2020` > 10e3)

ss_data_cpi |>
  filter(year == 2020) |>
  select(
    state,
    all_instruction_pp
  ) |>
  left_join(
    ss_data_cpi |>
      filter(year == 2002) |>
      select(
        state,
        all_instruction_pp
      ),
    by = "state"
  ) |>
  rename(`Instructional 2020` = `all_instruction_pp.x`) |>
  rename(`Instructional 2002` = `all_instruction_pp.y`) |>
  arrange(`Instructional 2020`)



# Remove the United States row, add a rank column, then add back the United States row to the top of the table

employee_benefits_table |>
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


employee_benefits_table |>
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
    select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
    mutate(Difference = `2020` - `2002`)
    


employee_benefits_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Benefits - Per Pupil 2020`,
    `2002` = `Total Benefits - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  filter(Growth > 1)


employee_benefits_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Benefits - Per Pupil 2020`,
    `2002` = `Total Benefits - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  filter(Growth > 1.7)

employee_benefits_table |>
  as.data.frame() |>
  filter(state != "United States") |>
  mutate(
    `Growth Rank` = " ",
    `2020 Rank` = " "
  ) |>
  rename(
    `2020` = `Total Benefits - Per Pupil 2020`,
    `2002` = `Total Benefits - Per Pupil 2002`,
    `Growth` = `Percent Change`
  ) |>
  select(`Growth Rank`, `2020 Rank`, state, `2020`, `2002`, `Growth`) |>
  filter(Growth < 0.2)


# Capital Outlay

capital_outlay_table_us <- capital_outlay_table |>
  filter(state == "United States")

capital_outlay_table_us$`2020` - capital_outlay_table_us$`2002`

capital_outlay_table_us$Growth


# Total Debt

total_debt_table_us <- total_debt_table |>
  filter(state == "United States")

total_debt_table_us$`Total Debt - Per Pupil 2020` - total_debt_table_us$`Total Debt - Per Pupil 2002`

total_debt_table_us$`Percent Change`

total_debt_table |>
  filter(`Percent Change` > 2)

total_debt_table |>
  filter(`Percent Change` < 0)





################################################################################
########################## ENROLLMENT & STAFFING TRENDS ########################
################################################################################

# Enrollment
enrollment_table |>
  filter(state == "United States")

enrollment_table |>
  filter(state != "United States") |>
  filter(Growth < 0)

enrollment_table |>
  filter(state != "United States") |>
  filter(Growth < -0.15)

enrollment_table |>
  filter(state != "United States") |>
  filter(Growth > 0.2)

# Sum 2020 enrollment for California, Florida, and Texas
ca_fl_tx <- enrollment_table |>
  filter(state == "California" | state == "Florida" | state == "Texas") |>
  summarise(`2020 Enrollment` = sum(`2020`))

us_total_enrollment <- enrollment_table |>
  filter(state != "United States") |>
  summarise(`2020 Enrollment` = sum(`2020`))

ca_fl_tx[1,1] / us_total_enrollment[1,1]

# Complete Staffing
complete_staffing_list <- complete_staffing |>
  as.data.frame() |>
  # pivot wider
  mutate(
    `Growth` = round(`2020` / `2002` - 1, 3)
  ) |>
  arrange(desc(`2020`))


# Staffing
staffing_trends_states <- staffing_trends |>
  select(state, `Non-Teachers Growth`)

staffing_trends_states |>
  filter(state == "United States")

staffing_trends_states |>
  filter(state != "United States") |>
  filter(`Non-Teachers Growth`  > 0)

staffing_trends_states |>
  filter(state != "United States") |>
  filter(`Non-Teachers Growth`  > 0.2)

staffing_trends_states |>
  filter(state != "United States") |>
  filter(`Non-Teachers Growth`  > 0.25) |>
  arrange(desc(`Non-Teachers Growth`))


# Non-Teachers
(complete_staffing[1, 3] + complete_staffing[2, 3] + complete_staffing[3, 3] + complete_staffing[4, 3] + complete_staffing[5, 3] + complete_staffing[7, 3] + complete_staffing[8, 3] + complete_staffing[9, 3] + complete_staffing[10, 3] + complete_staffing[11, 3])
# District Level Employees
(complete_staffing[1, 3] + complete_staffing[2, 3] + complete_staffing[3, 3])/ complete_staffing_total[1, 2]
# Teacher Growth
complete_staffing[6, 3] / complete_staffing[6, 2] - 1

staffing_trends |>
  as.data.frame() |>
  filter(state == "United States")

staffing_trends |>
  filter(state != "United States") |>
  filter(`Teachers Growth` > 0) 

staffing_trends |>
  filter(state != "United States") |>
  filter(`Teachers Growth` < 0) 

staffing_trends |>
  filter(state != "United States") |>
  filter(`Teachers Growth` < 0) |>
  filter(`Non-Teachers Growth` < 0)

complete_staffing[6, 3]  / (complete_staffing[1, 3] + complete_staffing[2, 3] + complete_staffing[3, 3] + complete_staffing[4, 3] + complete_staffing[5, 3] + complete_staffing[6, 3] + complete_staffing[7, 3] + complete_staffing[8, 3] + complete_staffing[9, 3] + complete_staffing[10, 3] + complete_staffing[11, 3])

staffing_trends |>
  as.data.frame() |>
  filter(state == "United States") |>
  mutate(non_teacher_pct = `Non-Teachers 2020`/(`Teachers 2020` + `Non-Teachers 2020`))


# Average Teacher Salary Growth
salary_table |>
  filter(state == "United States")


salary_delcine_states <- salary_table |>
  filter(state != "United States") |>
  filter(`Percent Change` < 0) |>
  arrange(`Percent Change`)

salary_table |>
  arrange(desc(`Percent Change`))

salary_table |>
  arrange(`Percent Change`)

salary_table |>
  filter(state == "United States")

salary_table |>
  arrange(desc(`Salary Adj 2020`))




################################################################################
################################## NAEP SCORES #################################
################################################################################


# 4th Grade Reading NAEP Scores
naep_reading_4_states <- naep_reading_4 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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


# 4th Grade Math NAEP Scores
naep_math_4_states <- naep_math_4 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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


# 8th Grade Reading NAEP Scores
naep_reading_8_states <- naep_reading_8 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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

# 8th Grade Math NAEP Scores
naep_math_8_states <- naep_math_8 |>
  filter(Year == 2019) |>
  # filter(State != "United States") |>
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


# 4th Grade Low Income Reading NAEP Scores
li_naep_reading_4_states <- low_income_naep_reading_4 |>
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
  mutate(`Reading 4 Score Pct` = `Eligible.x`/`Eligible.y` - 1) |>
  mutate(`Reading Score Diff` = `Eligible.x` - `Eligible.y`) |>
  # select(State, `Eligible.x`, `Eligible.y`, `Reading Score Diff`) |>
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`)



# 4th Grade Low Inocme Math NAEP Scores
li_naep_math_4_states <- low_income_naep_math_4 |>
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
  mutate(`Math 4 Score Pct` = `Eligible.x` / `Eligible.y` - 1) |>
  mutate(`Math Score Diff` = `Eligible.x` - `Eligible.y`) |>
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`)


# 8th Grade Low Income Reading NAEP Scores
li_naep_reading_8_states <- low_income_naep_reading_8 |>
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
  rename(`Reading Score 2019` = `Eligible.x`, `Reading Score 2003` = `Eligible.y`)


# 8th Grade Low Income Math NAEP Scores
li_naep_math_8_states <- low_income_naep_math_8 |>
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
  rename(`Math Score 2019` = `Eligible.x`, `Math Score 2003` = `Eligible.y`)



