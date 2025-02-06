#
# Libraries and data import ----
#

  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  source('./code/01_plot_aesthetics.R')
  
  model <- readRDS('./data/04_models.rds') %>%
    {.$linear$feedingRate}
  
  data_assay <- read.csv('./data/03_assayData.csv') %>%
    mutate(
      label = interaction(metsch, clone),
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
# Plot ----
#
  plot <- ggplot(
    data_assay %>% 
      filter(day!=9),
    aes(
      x = metsch_sem,
      y = feedingRate,
      color = kairomone_sem,
      group = kairomone_sem
    )
  ) +
    geom_point(alpha = 0.4, size = 2, stroke=0) +
    geom_smooth(method = "lm", alpha=0.15) +
    geom_hline(yintercept = 0, color = "darkgray") +
  
  # Manual scales
  #
    scale_color_manual(values = colors, limits = force) +
  
  # Theme
  #
    theme_bw() +
    
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    
    labs(color = "Kairomone") +
    
    ylab("Feeding rate (mL/h)") +
    xlab("Metschnikowia") +
    facet_grid(~assay, labeller=labeller(assay = ~ paste("Assay ", .x))) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size=12),
      panel.border = element_rect(fill=NA, color='black', linewidth=1),
      axis.line = element_blank()  # overlaps with panel.border; ticks are still present so the border serves as axis just fine
    )

  
#
# Write ----
#
  ggsave(
    './plots/plot_feedingrate.svg',
    plot=plot,
    width = 7,
    height = 5
  )
  
  saveRDS(
    plot,
    './plots/plot_feedingrate.rds'
  )
  