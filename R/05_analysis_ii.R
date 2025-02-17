# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")

# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
project_data_clean_aug <- read_csv(file = "data/03_project_data_clean_aug.csv")

# Wrangle data ------------------------------------------------------------ 
heatmap_data <- project_data_clean_aug %>% 
  filter(p < 0.01)

# Visualize data ----------------------------------------------------------  
heatmap <- heatmap_data %>% 
  ggplot(aes(x = sample, 
             y = id, 
             fill = log_fold_change)) +
  geom_tile() +
  facet_grid(HLA~., 
             scales = "free_y", 
             space = "free") +
  scale_fill_gradient2(low = "red", 
                       mid = "white", 
                       high = "darkgreen") +
  labs(x = "Sample", 
       y = "Peptide", 
       title = "Overview of viral responses", 
       tag = "HLA type", 
       fill = "Log-fold \nchange") +
  theme(plot.tag = element_text(angle = -90),
        plot.tag.position = c(1.05, 
                              0.5),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", 
                                    fill = NA),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        text = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12, 
                                  hjust = 0.5),
        legend.title = element_text(size = 10),
        plot.margin = unit (c (0.2, 2, 0.2, 0.2), 
                            'cm'),
        legend.position = c (1.3, 0.11)) +
  geom_vline(mapping = NULL, 
             xintercept = seq(1.5, 
                              length(unique(pull(heatmap_data,
                                                 sample))), 
                              by = 1),
             colour='white') +
  geom_hline(mapping = NULL, 
             yintercept = seq(1.5, 
                              length(unique(pull(heatmap_data,
                                                 id))), 
                              by = 1), 
             colour='white')

# Write data --------------------------------------------------------------
ggsave(filename = "/cloud/project/results/05_heatmap.png",
       plot = heatmap, 
       width = 5, 
       height = 8,
       device = "png")

