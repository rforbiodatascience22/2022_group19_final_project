# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug %>% ...

#finding maximum to determine the range of the axis on the plot
maximum_y = project_data_raw %>% 
  pull(log_fold_change) %>% 
  max() %>% 
  round() + 0.5

#filtering the interesting plots
pointsabovelogfold2 = project_data_raw %>% 
  filter(log_fold_change > 2)

#filtering all the uninteresting plots
pointsbelowlogfold2 = project_data_raw %>% 
  filter(log_fold_change < 2)

# Model data
my_data_clean_aug %>% ...

# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...

#plotting a log-fold-change graph
project_data_raw %>% 
  mutate(Sequence = fct_reorder(Sequence, HLA)) %>%  
  ggplot(aes(x = Sequence, y = log_fold_change)) +
  geom_point(data = pointsabovelogfold2, color = "red", aes_string(size = 'p', alpha = 0.75)) +
  geom_point(data = pointsbelowlogfold2, color = "gray", size = 0.1) +
  geom_hline(yintercept = 2, 
             linetype = "dashed") +
  scale_y_continuous(limits = c(0, 
                                maximum_y),
                     breaks = seq(0, 
                                  maximum_y, 
                                  2)) +
  labs(title = "Log-fold change vs sequence", 
       x = "Sequence", 
       y = "Log-fold change") +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 13, 
                            face = "bold"),
        axis.text.x = element_text(size = 4,
                                   angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 1))

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)