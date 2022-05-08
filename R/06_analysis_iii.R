# Load libraries ----------------------------------------------------------
library("tidyverse")
library(stringr)
library(usethis)
library(dplyr)
# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

#my_path_donor_response_database <- readline(prompt="insert path for donor response database excel sheet: ")

donor_response_database <- read_csv2(file = "data/_raw/Copy of Buffycoat Virus-screen Overview r course.csv")

#my_path_new_screen <- readline(prompt="insert path for new donor screen: ")

project_data_screen <- read_csv(file = "data/03_project_data_clean_aug.csv")


# Wrangle data ------------------------------------------------------------


#Adds appropriate names to the database sheet 
donor_response_database <- donor_response_database %>% 
  rename(
    HLA = ...2,
    origen = ...3,
    sequence = ...4)

#all peptides mentioned in the database 
string_peptides <- pull(donor_response_database, sequence)


#Removes samples under log fold change 2
project_data_log_fold <- filter(project_data_screen, log_fold_change>2)

#all peptide responses in the screened data with log fold change above 2
donor_response_petides <- pull(project_data_log_fold, Sequence)

#matching the 2 list of peptides giving position of the matches on the database 
sequence_matches <- match(donor_response_petides,string_peptides)

#adds database position of the given peptide in the database 
project_data_screen_matches <-mutate(project_data_log_fold, sequence_matches)

#removes peptides not in the database
all_responses <- filter(project_data_screen_matches, sequence_matches != "NA")

#picking out relevant coumns 
peptide_donor_to_db <- select(all_responses, sample, Sequence, sequence_matches, log_fold_change)

database_position_donor  <-pivot_wider(peptide_donor_to_db, names_from = sample, values_from = log_fold_change)

#new dataframe to add values at right positions with index 
new_df <- data.frame(matrix(ncol= 1, nrow = nrow(donor_response_database))) %>%
tibble::rownames_to_column() %>%
  rename(sequence_matches=rowname)

#merging the 2 based on row position
Positions_added <- merge(database_position_donor, new_df, by="sequence_matches", all=T) %>%
  arrange(as.integer(sequence_matches))

#removing the columns not to add in the final database and appending on the database sheet
remove_coulumns <- select(Positions_added, -sequence_matches, -Sequence, -last_col())

#append on database
final_file <- bind_cols(donor_response_database, remove_coulumns )

# Write data --------------------------------------------------------------
write_csv(x = final_file,  "results/06_append_database.csv")