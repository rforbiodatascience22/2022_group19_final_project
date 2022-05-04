# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load data ---------------------------------------------------------------

# remove some of the uninteresting columns in the spreadsheet to clean it up.
project_data <- read_csv(file = "data/01_project_data.csv")

# Wrangle data ------------------------------------------------------------
cleaning_data <- function(project_data){
  project_data_clean <- project_data %>%
    filter(log_fold_change > 0 
           & input.1 > 50 
           & input.2 > 50 
           & input.3 > 50) %>%
    select("barcode", 
           "sample", 
           "log_fold_change", 
           "p", 
           "HLA", 
           "Origin", 
           "Peptide", 
           "Sequence")
}

project_data_cleaning <- cleaning_data(project_data)

# Write data --------------------------------------------------------------
write_csv(x = project_data_cleaning,
          file = "data/02_project_data_clean.csv")
