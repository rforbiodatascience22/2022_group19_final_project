# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------
project_data_clean <- read_csv(file = "data/02_project_data_clean.csv")

# Wrangle data ------------------------------------------------------------
augmenting_data <- function(data_clean){
  project_data_clean_aug <- project_data_clean %>%
    mutate(id = paste0(Peptide, " (", Origin, ")"))
}

project_data_clean_augment <- augmenting_data(project_data_clean)

# Write data --------------------------------------------------------------
write_csv(x = project_data_clean_augment,
          file = "data/03_project_data_clean_aug.csv")
