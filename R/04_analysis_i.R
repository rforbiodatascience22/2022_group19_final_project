# Load libraries ----------------------------------------------------------
library("tidyverse")
library("forcats")
library("ggplot2")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_csv(file = "data/03_project_data_clean_aug.csv")

# Wrangle data ------------------------------------------------------------

#Finding maximum to determine the range of the axis on the plot
maximum_y <- my_data_clean_aug %>% 
  pull(log_fold_change) %>% 
  max() %>% 
  round() + 0.5

#Determining the numbers of sequences per virus strain (Origin) and setting a threshold.
threshold <- my_data_clean_aug %>% 
  count(Origin) %>% 
  filter(n > 50) %>% 
  count() %>% 
  pull()

#Pooling all groups of vira with less than 50 hits into HHV or Others
my_data_clean_aug_pooling <- my_data_clean_aug %>% 
  mutate(Origin = as.factor(Origin)) %>% 
  mutate(newID = fct_lump(Origin, threshold)) %>% 
  mutate(size_value = case_when(log_fold_change <= 2 ~ 0,
                                0.001 >= p & log_fold_change < 2 ~ 1,
                                0.001 >= p & log_fold_change >= 2 ~ 1))



### Plot v2
my_data_clean_aug_pooling %>% 
  mutate(Significance = case_when(0.001 >= p & log_fold_change >= 2 ~ "Of_interest",
                                  0.001 >= p & log_fold_change < 2 ~ "p_sig",
                                  TRUE ~ "Others")) %>% 
  ggplot(aes(x = Peptide, 
             y = log_fold_change)) +
  geom_point(aes(size = size_value, 
                 colour = Significance)) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  scale_color_manual(name = "Significance",
                     values = c(Of_interest = "red", 
                                p_sig = "blue",
                                Others = "black")) +
  scale_size(range = c(0.1,
                       1), 
             guide = "none") +
  scale_y_continuous(limits = c(0, 
                                maximum_y),
    breaks = seq(0, 
                 maximum_y, 
                 2)) +
  labs(x = "ID", 
       y = "Log-fold change",
       title = "Log-fold change vs sequence") +
  facet_grid(.~newID, 
             scales = "free_x", space = "free") +
  theme(plot.title = element_text(size = 10, 
                                  hjust = 0.5,
                                  face = "bold"),
        axis.text.x = element_text(size = 5,
                                   angle = 90, 
                                   vjust = 0.5,
                                   hjust = 1),
        strip.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", 
                                    fill = NA),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.background = element_rect(fill = "transparent", 
                                       color = NA))

# Write data --------------------------------------------------------------
ggsave(filename = "/cloud/project/results/04_dotplot.png",
       width = 10, 
       height = 7,
       device = "png")
