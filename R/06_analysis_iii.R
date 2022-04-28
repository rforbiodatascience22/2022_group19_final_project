# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggplot2")

# Load data ---------------------------------------------------------------
project_data_raw <- read_csv2(file = "data/_raw/project_data_raw.csv")


# Wrangle data ------------------------------------------------------------
project_data <- project_data_raw %>%
  #filter(!sample %in% c("NTC (H2O)","NTC (BCB)")) %>%
  mutate(id = paste0(Peptide, " (", Origin, ")")) %>%
  filter(p < 0.01 & log_fold_change > 0)
  

# Visualize data ----------------------------------------------------------  
plot <- project_data %>% 
  ggplot(aes(x = sample, 
             y = id, 
             fill = log_fold_change)) +
  geom_tile() +
  facet_grid(HLA~., 
             scales = "free_y", 
             space = "free") +
  scale_fill_gradient2(low = "red", 
                       mid = "white", 
                       high = "red") +
  labs(x = "Sample", 
       y = "Peptide", 
       title = "Awesome title", 
       tag = "HLA type", 
       fill = "Log2 \nFold \nChange") +
  theme(plot.tag = element_text(angle = -90),
        plot.tag.position = c(1.05, 0.5),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 10),
        plot.margin = unit (c (.2, 2, .2, .2), 'cm'),
        legend.position = c (1.3, .1)) +
  geom_vline(mapping = NULL, 
             xintercept = seq(1.5, length(unique(project_data$sample)), by = 1),
             colour='white') +
  geom_hline(mapping = NULL, 
             yintercept = seq(1.5, length(unique(project_data$id)), by = 1), 
             colour='white')
plot


# Write data --------------------------------------------------------------
ggsave(filename = "/cloud/project/results/heatmap.png", 
       plot = plot, 
       width = 5, 
       height = 8,
       device = "png")
