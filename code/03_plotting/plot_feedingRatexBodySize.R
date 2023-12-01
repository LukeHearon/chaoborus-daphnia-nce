library(here)
library(dplyr)
library(ggplot2)

#
# Data import ----
#

  data <- read.csv(here("data", "03_repDF.csv"))
  
plot.XY <- function(var_x, var_y, clone_in){
  ggplot(
    data = data %>%
      filter(clone == clone_in),
    aes(
      x = {{var_x}},
      y = {{var_y}},
      shape = metsch,
      color = kairomone
    )
  ) +
    
    
    # geoms
    #
      
      geom_point(
        alpha = 0.4
      ) +
      geom_smooth(
        method = "lm",
        aes(linetype = metsch),
        se = F
      ) +
  
    # manual scales
    #
  
      scale_color_manual(
        values = c(
          "TRUE" = "red",
          "FALSE" = "blue"
        )
      ) +
    
      scale_linetype_manual(
        values = c(
          "TRUE" = "solid",
          "FALSE" = "dashed"
        )
      ) +
    
    # Aesthetics
    #
    
    theme(
      axis.line = element_line(
        colour = 'gray',
      ),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
    ) 
}

plot.XY(body_assay1, feedingRate_assay1, "c1")



#
# Pick and choose plots
#
  
  # assay 1
  plot_assay1_significantFactorsOnly <- ggplot(
    data = data,
    aes(
      x = body_assay1,
      y = feedingRate_assay1,
      color = kairomone
    )
  ) +
    
    
    # geoms
    #
    
    geom_point(
      alpha = 0.4
    ) +
    geom_smooth(
      method = "lm",
      se = F
    ) +
    
    # manual scales
    #
    
    scale_color_manual(
      values = c(
        "TRUE" = "red",
        "FALSE" = "blue"
      )
    ) +
    
    # Aesthetics
    #
    
    theme(
      axis.line = element_line(
        colour = 'gray',
      ),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
    ) 
  
  # assay 2
  plot_assay2_significantFactorsOnly <- ggplot(
    data = data,
    aes(
      x = body_assay2,
      y = feedingRate_assay2,
      color = kairomone
    )
  ) +
    
    
    # geoms
    #
    
    geom_point(
      alpha = 0.4
    ) +
    geom_smooth(
      method = "lm",
      se = F
    ) +
    
    # manual scales
    #
    
    scale_color_manual(
      values = c(
        "TRUE" = "red",
        "FALSE" = "blue"
      )
    ) +
    
    # Aesthetics
    #
    
    theme(
      axis.line = element_line(
        colour = 'gray',
      ),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
    ) 
  
  
ggsave(
  plot_assay1_significantFactorsOnly,
  filename = here("plots", "feedingRateXbody_assay1.svg")
)

ggsave(
  plot_assay2_significantFactorsOnly,
  filename = here("plots", "feedingRateXbody_assay2.svg")
)

ggsave(
  plot_assay1_significantFactorsOnly,
  filename = here("plots", "pngs", "feedingRateXbody_assay1.png")
)

ggsave(
  plot_assay2_significantFactorsOnly,
  filename = here("plots", "pngs", "feedingRateXbody_assay2.png")
)
