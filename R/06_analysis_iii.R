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

my_name <- readline(prompt="Enter initials of person responsible for experiment: ")
# print(my.name)


#Adds appropriate names to the database sheet 
donor_response_database <- donor_response_database %>% 
  rename(
    HLA = ...2,
    origen = ...3,
    sequence = ...4
  )

#Creates vector with unique donors
sample_names <- pull(project_data_screen , sample)
unique_sample <- unique(sample_names)
donors <- str_subset(unique_sample,"NT", negate=TRUE)

#Creates new empty dataframe based on amount of donor samples and variables in response database 
new_dataframe <- data.frame(matrix(ncol= 1, nrow = nrow(donor_response_database)))
colnames(new_dataframe) <- donors

project_data_screen <- filter(project_data_screen, log_fold_change>2)

string_peptides <- pull(donor_response_database, sequence)
donor_responses <- pull(project_data_screen, Sequence)

str_match(string_peptides,donor_responses[5])

sequences <- pull(project_data_screen, Sequence)


sequence_matches <- match(sequences,string_peptides)


project_data_screen_matches <-mutate(project_data_screen, sequence_matches)

test <- pivot_wider(project_data_screen, names_from = sample,
                    values_from = sequence_matches)


all_responses <- filter(project_data_screen_matches, sequence_matches != "NA")

test <- pivot_wider(all_responses, names_from = sample,
                    values_from = sequence_matches)

test2 <-select(all_responses, sample, Sequence, sequence_matches, log_fold_change)

test3 <-pivot_wider(test2, names_from = sample, values_from = log_fold_change)




test4 <- new_dataframe %>%
  tibble::rownames_to_column()

test4 <- rename(test4, sequence_matches=rowname)




test5 <- merge(test3, test4, by="sequence_matches", all=T)
test5 <- arrange(test5, as.integer(sequence_matches))

test5 <- select(test5, -sequence_matches, -Sequence, -last_col()) 


final_file <- bind_cols(donor_response_database,test5)


#sequncematches i new dataframe  merche( data, data, by = sequnence_matches, all = true)


# Write data --------------------------------------------------------------


write_csv(x = final_file,  "results/06_append_database.csv")