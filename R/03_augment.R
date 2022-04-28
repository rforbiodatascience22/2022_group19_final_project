# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
project_data_clean <- read_csv(file = "data/02_project_data_clean.csv")

# Wrangle data ------------------------------------------------------------
project_data_clean_aug <- project_data_clean %>%
  mutate(id = paste0(Peptide, " (", Origin, ")"))

# Write data --------------------------------------------------------------
write_csv(x = project_data_clean_aug,
          file = "data/03_project_data_clean_aug.csv")
