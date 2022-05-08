# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")

# Load data ---------------------------------------------------------------

# Accessing all excel sheets 
sheet <- excel_sheets("data/_raw/project_data_raw.xlsx")

# Creating a list of individual data frames for each sheet
data_frame <- lapply(setNames(sheet, sheet), 
                       function(x) read_excel("data/_raw/project_data_raw.xlsx", 
                                              sheet = x))

# Attaching individual data frames together
data_frame <- bind_rows(data_frame, 
                        .id = "Sheet")

# Write data --------------------------------------------------------------
write_csv(x = data_frame,
          file = "data/01_project_data.csv")
