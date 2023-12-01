library(here)
library(dplyr)
library(ggplot2)

#
# Data import ----
#
  
  data <- read.csv(here("data", "03_repDF.csv"))
  
#
# Sourcing plot code ----
#

  source(here("code", "plotting", "plot_rnorm_numeric.R"))
  source(here("code", "plotting", "plot_rnorm_binomial.R"))
  source(here("code", "plotting", "plot_scatter.R"))
  source(here("code", "plotting", "plot_aesthetics.R"))
  
#
# Plotting ----
#
 
plots <- list()
   
  # infection
    plots$infection <- plot.rnorm_binomial(
      infectionFate,
      data %>%
        filter(metsch == T)
    )
    
    plots$infection_noClone <- plot.rnorm_binomial(
      infectionFate,
      data %>%
        filter(metsch == T) %>%
        mutate(clone = "c1")
    )
    
  # spine length
    plots$spine_assay1 <- plot.rnorm_numeric(
      spine_assay1,
      data
    )
    
    plots$spineRel_assay1 <- plot.rnorm_numeric(
      spineRel_assay1,
      data %>%
        mutate(spineRel_assay1 = spine_assay1/body_assay1)
    )
    
    plots$spine_assay2 <- plot.rnorm_numeric(
      spine_assay2,
      data
    )
    
    plots$spineRel_assay2 <- plot.rnorm_numeric(
      spineRel_assay2,
      data %>%
        mutate(spineRel_assay2 = spine_assay2/body_assay2)
    )
    
  # reproduction
    plots$r1 <- plot.rnorm_numeric(
      r1,
      data %>%
        filter(fate != "lost")
    )
    
    plots$r1_reproductives <- plot.rnorm_numeric(
      r1,
      data %>%
        filter(fate != "lost", r1 > 0)
    )
    
    plots$r1_reproduced <- plot.rnorm_binomial(
      reproduced,
      data %>%
        filter(fate != "lost") %>%
        mutate(reproduced = r1 > 0)
    )
  
  
  # feeding rate
    plots$feedingRate_assay1 <- plot.rnorm_numeric(
      feedingRate_assay1,
      data
    )
    
    plots$feedingRate_relative_assay1 <- plot.rnorm_numeric(
      feedingRate_relative_assay1,
      data %>%
        mutate(
          feedingRate_relative_assay1 = feedingRate_assay1/body_assay1
        )
    )
    
    plots$feedingRate_assay2 <- plot.rnorm_numeric(
      feedingRate_assay2,
      data
    )
    
    plots$feedingRate_relative_assay2 <- plot.rnorm_numeric(
      feedingRate_relative_assay2,
      data %>%
        mutate(
          feedingRate_relative_assay2 = feedingRate_assay2/body_assay2
        )
    )
    
  # clone mean feeding rate against infection rate (just for practice, this will be adjusted to spine against infection rate once i have the data)
    plots$tmp <- plot.scatter(
      variable_x = feedingRate_assay1,
      variable_y = infectionRate,
      variable_fill = kairomone,
      
      dataset_in = data %>%
        filter(is.na(feedingRate_assay1) == F, metsch == T) %>%
        group_by(clone, kairomone) %>%
        dplyr::summarize(
          infectionRate = mean(infectionFate),
          feedingRate_assay1 = mean(feedingRate_assay1)
        )
    )

    
#
# Saving plots
#
  
  for(p in 1:length(plots)){
    ggsave(
      filename = here("plots", paste0(names(plots)[p], ".svg")),
      plot = plots[[p]],
      device = "svg",
      width = 2300,
      height = 1500,
      units = "px"
    )
  }
  
    for(p in 1:length(plots)){
      ggsave(
        filename = here("plots", "pngs", paste0(names(plots)[p], ".png")),
        plot = plots[[p]],
        device = "png",
        width = 2300,
        height = 1500,
        units = "px"
      )
    }
    