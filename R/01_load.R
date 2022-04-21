# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
project_data_raw <- read_csv2(file = "data/_raw/project_data_raw.csv")


# Wrangle data ------------------------------------------------------------
project_data <- project_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = project_data,
          file = "data/01_my_data.tsv")