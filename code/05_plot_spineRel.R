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
    mutate(
      label = interaction(metsch, clone)
    )
  
  
#
# Plot
#
  plot <- ggplot(
    data_assay,
    aes(
      x = day,
      y = spine,
      color = clone,
      linetype = clone,
      shape = clone
    )
  ) +
    
    # Geometry
    #
    geom_point(alpha = 0.4, size = 2) +
    geom_line(stat = "smooth", method = "lm") +
    
    # Manual scales
    #
    scale_color_manual(values = colors, limits = force) +
    scale_linetype_manual(
      values = c(
        "c1" = "solid",
        "m37" = "dashed",
        "w2" = "dotdash"
      )
    ) +
    
    # Theme
    #
    theme_bw() +
    
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    
    ylab("Spine size (mm)") +
    xlab("Day")
  
#
# Write ----
#
  ggsave(
    './plots/plot_spineRel.svg',
    width = 7,
    height = 5
  )
  