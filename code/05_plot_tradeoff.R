#
# Libraries and data import ----
#
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  source('./code/01_plot_aesthetics.R')
  
  model <- readRDS('./data/04_models.rds') %>%
    {.$linear$spine}
  
  data_assay <- read.csv('./data/03_assayData.csv') %>%
    filter(!spineBroken) %>%
    mutate(
      metsch_sem = case_when(
        metsch ~ 'present',
        T ~ 'absent'
      ),
      
      kairomone_sem = case_when(
        kairomone ~ 'present',
        T ~ 'absent'
      )
    ) 
  
  
#
# Plot
#
  plot <- ggplot(
    data_assay,
    aes(
      x = metsch_sem,
      y = spine,
      color = kairomone_sem,
      group = interaction(metsch_sem, kairomone_sem)
    )
  ) +
    
    # Geometry
    #
    geom_boxplot() +
    facet_grid(rows=vars(assay), labeller=labeller(assay = ~ paste("Assay ", .x))) +
    
    # Manual scales
    #
    scale_color_manual(values = colors, limits = force) +
    
    # Theme
    #
    theme_bw() +
    
    theme(
      # panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    
    labs(color = "Kairomone") +
    ylab("Spine size (mm)") +
    xlab("Metschnikowia")
  
#
# Write ----
#
  ggsave(
    './plots/plot_tradeoff.svg',
    plot=plot,
    width = 7,
    height = 5
  )
  
  saveRDS(
    plot,
    './plots/plot_tradeoff.rds'
  )