# Load libraries ----------------------------------------------------------
library("tidyverse")
library(stringr)
library(usethis)
library(dplyr)
# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------


my_path_donor_response_database <- readline(prompt="insert path for donor response database excel sheet: ")

donor_response_database <- read_csv2(file = my_path_donor_response_database)

#data/_raw/Copy of Buffycoat Virus-screen Overview r course.csv



my_path_new_screen <- readline(prompt="insert path for new donor screen: ")

project_data_screen <- read_csv(file = my_path_new_screen)

data/03_project_data_clean_aug.csv



# Wrangle data ------------------------------------------------------------

n <- nrow(donor_response_database)

donor_response_database <- donors[1]

donor_response_database[20,4]

sequenses <- project_data_raw$Sequence


unique_sequence <- unique(sequenses)


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
sample_names <- pull(project_data_screen , sample) %>% 
unique_sample <- unique(sample_names) %>% 
donors <- str_subset(unique_sample,"NT", negate=TRUE)

#Creates new empty dataframe based on amount of donor samples and variables in response database 
new_dataframe <- data.frame(matrix(ncol = length(donors),
                        nrow = nrow(donor_response_database)))
colnames(new_dataframe) <- donors


str_match(unique_sequence,toString(donor_response_database[20,4]))


string_peptides <- pull(donor_response_database, sequence)

string_peptides
new_vector <- str_match(string_peptides,toString(unique_sample[2]))


df[,1] <- new_vector


count <- 0

for (i in length(project_data_raw)){
  if (project_data_raw$log_fold_change > 2){
  print(paste("The year is", i))}
}

for (i in project_data_raw$log_fold_change){
  count <- count+1
  
  if (i > 2){
    count <- count+1
    new_data_set[,count] <-c(project_data_raw$sample[i],project_data_raw$sequnce[i],i)
    print(project_data_raw$sample[i])
    print(i)
    print(count)
  }
}


new_dataset <-matrix(ncol=3)
new_dataset[,1] <- c(project_data_raw$sample[2],project_data_raw$Sequnce[2],2)

for (i in project_data_raw$log_fold_change){
  count <- count+1
  
  if (i > 2){
    count <- count+1
    new_dataset[,count] <- c(project_data_raw$sample[i],project_data_raw$Sequnce[i],i)
    
  }
}



project_data_raw$sample[i],
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
