# Load libraries ----------------------------------------------------------
library("tidyverse")
library(stringr)
library(usethis)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
project_data_raw <- read_csv2(file = "data/_raw/project_data_raw.csv")
donor_response_database <- read_csv2(file = "data/_raw/Copy of Buffycoat Virus-screen Overview r course.csv")

# Wrangle data ------------------------------------------------------------
my.name <- readline(prompt="Enter initials of person responsible for experiment: ")
# print(my.name)


unique_samples <- unique(project_data_raw$sample)
donors <- str_subset(unique_samples,"NT", negate=TRUE)


n <- nrow(donor_response_database)

donor_response_database <- donors[1]

donor_response_database <- donor_response_database %>% 
  rename(
    HLA = ...2,
    origen = ...3,
    sequence = ...4
  )








df <- data.frame(matrix(ncol = length(donors), 
                        nrow = nrow(donor_response_database)))

colnames(df) <- donors

donor_response_database[20,4]

sequenses <- project_data_raw$Sequence


unique_sequence <- unique(sequenses)





str_match(unique_sequence,toString(donor_response_database[20,4]))


string_peptides <- donor_response_database$sequence


new_vector <- str_match(string_peptides,
                        toString(unique_sequence[2]))

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



usethis

# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
