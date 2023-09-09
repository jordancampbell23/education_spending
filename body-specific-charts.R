library(tidyverse)

# NAEP High Spending / Low Spending States
naep_high_low_states <- read_csv("output_data/body/introduction/intro_table_6.csv")


# Support Staff 
support_services_us_pct <- read_csv("output_data/support_services_us_pct.csv")




# Staffing Growth
complete_staffing <- read_csv("input_data/complete_staffing.csv") |>
  mutate(pct_change = `2020`/`2002` - 1)
