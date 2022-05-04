# Define project functions ------------------------------------------------
log_fold_change_plotting <- function(my_data){
  maximum_y <- my_data %>% 
    pull(log_fold_change) %>% 
    max() %>% 
    round() + 0.5
  
  my_data_pooling <- my_data %>% 
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
                             0.001 < p & log_fold_change >= 2 ~ 0,
                             0.001 >= p & log_fold_change >= 2 ~ 1))
  
  pointsofinterest <- my_data_pooling %>% 
    filter(0.001 >= p & log_fold_change >= 2)
  
  my_data_pooling %>% 
    ggplot(aes(x = Peptide, 
               y = log_fold_change)) +
    facet_grid(.~newID,
               scales = "free_x",
               space = "free") +
    geom_point(aes_string(size = "value")) +
    geom_point(data = pointsofinterest, 
               color = "red") +
    geom_hline(yintercept = 2, 
               linetype = "dashed") +
    scale_y_continuous(limits = c(0, 
                                  maximum_y),
                       breaks = seq(0, 
                                    maximum_y, 
                                    2)) +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, 
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
          axis.title.y = element_text(size = 8)) +
    labs(x = "ID", 
         y = "Log-fold change",
         title = "Log-fold change vs sequence") +
    scale_size(range = c(0.1,1))
  
}

log_fold_change_plotting(my_data_clean_aug)
...