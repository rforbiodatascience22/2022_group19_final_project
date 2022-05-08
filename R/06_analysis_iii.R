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
<<<<<<< HEAD
    sequence = ...4)

#all peptides mentioned in the database 
string_peptides <- pull(donor_response_database, sequence)


#Removes samples under log fold change 2
project_data_log_fold <- filter(project_data_screen, log_fold_change>2)
=======
    sequence = ...4
  )

#Creates vector with unique donors
sample_names <- pull(project_data_screen, 
                     sample)
unique_sample <- unique(sample_names)
donors <- str_subset(unique_sample,
                     "NT", 
                     negate=TRUE)

#Creates new empty dataframe based on amount of donor samples and variables in response database 
new_dataframe <- data.frame(matrix(ncol= 1, 
                                   nrow = nrow(donor_response_database)))
colnames(new_dataframe) <- donors

project_data_screen <- filter(project_data_screen, 
                              log_fold_change>2)

string_peptides <- pull(donor_response_database, 
                        sequence)
donor_responses <- pull(project_data_screen,
                        Sequence)

str_match(string_peptides,
          donor_responses[5])

sequences <- pull(project_data_screen, 
                  Sequence)
>>>>>>> 590560d6d7f708fe69789fe8adf2efaf9a97504a

#all peptide responses in the screened data with log fold change above 2
donor_response_petides <- pull(project_data_log_fold, Sequence)

#matching the 2 list of peptides giving position of the matches on the database 
sequence_matches <- match(donor_response_petides,string_peptides)

#adds database position of the given peptide in the database 
project_data_screen_matches <-mutate(project_data_log_fold, sequence_matches)

<<<<<<< HEAD
#removes peptides not in the database
all_responses <- filter(project_data_screen_matches, sequence_matches != "NA")

#picking out relevant coumns 
peptide_donor_to_db <- select(all_responses, sample, Sequence, sequence_matches, log_fold_change)
=======
all_responses <- filter(project_data_screen_matches, 
                        sequence_matches != "NA")

test <- pivot_wider(all_responses, names_from = sample,
                    values_from = sequence_matches)

test2 <-select(all_responses, 
               sample, 
               Sequence, 
               sequence_matches, 
               log_fold_change)

test3 <-pivot_wider(test2, 
                    names_from = sample, 
                    values_from = log_fold_change)

>>>>>>> 590560d6d7f708fe69789fe8adf2efaf9a97504a

database_position_donor  <-pivot_wider(peptide_donor_to_db, names_from = sample, values_from = log_fold_change)

#new dataframe to add values at right positions with index 
new_df <- data.frame(matrix(ncol= 1, nrow = nrow(donor_response_database))) %>%
tibble::rownames_to_column() %>%
  rename(sequence_matches=rowname)

#merging the 2 based on row position
Positions_added <- merge(database_position_donor, new_df, by="sequence_matches", all=T) %>%
  arrange(as.integer(sequence_matches))

<<<<<<< HEAD
#removing the columns not to add in the final database and appending on the database sheet
remove_coulumns <- select(Positions_added, -sequence_matches, -Sequence, -last_col())
=======
test4 <- rename(test4, 
                sequence_matches=rowname)




test5 <- merge(test3, 
               test4, 
               by="sequence_matches", 
               all=T)

test5 <- arrange(test5, 
                 as.integer(sequence_matches))

test5 <- select(test5, 
                -sequence_matches, 
                -Sequence, 
                -last_col()) 


final_file <- bind_cols(donor_response_database,
                        test5)


#sequncematches i new dataframe  merche( data, data, by = sequnence_matches, all = true)
>>>>>>> 590560d6d7f708fe69789fe8adf2efaf9a97504a

#append on database
final_file <- bind_cols(donor_response_database, remove_coulumns )

# Write data --------------------------------------------------------------
<<<<<<< HEAD
write_csv(x = final_file,  "results/06_append_database.csv")
=======


write_csv(x = final_file,  
          "results/06_append_database.csv")
>>>>>>> 590560d6d7f708fe69789fe8adf2efaf9a97504a
