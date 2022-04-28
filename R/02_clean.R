# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------

project_data <- read_csv(file = "data/01_project_data.csv")

# Wrangle data ------------------------------------------------------------
project_data <- project_data %>%
  filter(p < 0.01 & log_fold_change > 0 & input.1 > 50 & input.2 > 50 & input.3 > 50) %>%
  mutate(id = paste0(Peptide, " (", Origin, ")")) %>%
  select("barcode", 
         "sample", 
         "log_fold_change", 
         "p", 
         "HLA", 
         "Origin", 
         "Peptide", 
         "Sequence",
         "id")


# Write data --------------------------------------------------------------
write_csv(x = project_data,
          file = "data/02_project_data_clean.csv")
