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

#Determining the numbers of sequences per virus strain (Origin)
project_data_raw %>% 
  select(Origin) %>% 
  count(Origin)

#pooling all groups of vira with less than 100 hits into HHV or Others
project_data_raw_aug = project_data_raw %>% 
  mutate(newID = case_when(Origin == "CMV" ~ "CMV",
                           Origin == "Covid-19" ~ "Covid-19",
                           Origin == "hCoV" ~ "hCoV",
                           Origin == "EBV" ~ "EBV",
                           Origin == "FLU-A" ~ "FLU-A",
                           Origin == "HHV-1" ~ "HHV",
                           Origin == "HHV-2" ~ "HHV",
                           Origin == "B19" ~ "Others",
                           Origin == "HAdV-C" ~ "Others",
                           Origin == "NWV" ~ "Others",
                           Origin == "HIV-1" ~ "Others",
                           Origin == "VACV" ~ "Others",
                           Origin == "HMPV" ~ "Others",
                           Origin == "BKPyV" ~ "Others",
                           Origin == "JCPyV" ~ "Others",
                           Origin == "HPV" ~ "Others",
                           Origin == "unknown" ~ "Others",
                           Origin == "VZV" ~ "Others",
                           Origin == "HHV-6B" ~ "Others")) %>% 
  mutate(value = case_when(log_fold_change <= 2 ~ 0,
                           0.001 <= p & log_fold_change >= 2 ~ 1,
                           0.001 >= p & log_fold_change >= 2 ~ 2,
                           0.0001 >= p & log_fold_change >= 2 ~ 3))

project_data_raw_aug %>% 
  select(value) %>% 
  count(value)
#plotting a log-fold-change graph
# integrate different sizes of dots dependent on log fold change 2
project_data_raw_aug %>% 
  ggplot(aes(x = Peptide, y = log_fold_change)) +
  facet_grid(.~newID,
             scales = "free_x", 
             space = "free") +
  geom_point(aes_string(size ="value", alpha = 0.75)) +
  geom_hline(yintercept = 2, 
             linetype = "dashed") +
  scale_y_continuous(limits = c(0, 
                                maximum_y),
                     breaks = seq(0, 
                                  maximum_y, 
                                  2)) +
  theme(legend.position="none",
        plot.title = element_text(size = 12, 
                                  hjust = 0.5),
        axis.text.x = element_text(size = 4,
                                   angle = 90, 
                                   vjust = 0.5, 
                                   hjust = 1),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Sequence", 
       y = "Log-fold change",
       title = "Log-fold change vs sequence") +
  scale_fill_gradient2(low = "red", 
                       mid = "white", 
                       high = "darkgreen")

# Write data --------------------------------------------------------------
ggsave(filename = "/cloud/project/results/log-fold change.png", 
       plot = plot,
       device = "png")
