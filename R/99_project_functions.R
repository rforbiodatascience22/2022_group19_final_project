# Define project functions ------------------------------------------------
log_fold_change_plotting <- function(my_data){
  maximum_y <- my_data %>% 
    pull(log_fold_change) %>% 
    max() %>% 
    round() + 0.5
  
  threshold <- my_data_clean_aug %>% 
    select(Origin) %>% 
    count(Origin) %>% 
    filter(n > 50) %>% 
    count() %>% 
    pull()
  
  my_data_pooling <- my_data %>% 
    mutate(Origin = as.factor(Origin)) %>% 
    mutate(newID = fct_lump(Origin, threshold)) %>% 
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