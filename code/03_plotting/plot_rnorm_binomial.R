library(Hmisc)
library(ggplot2)
library(ggrepel)
library(dplyr)

plot.rnorm_binomial <- function(variable_in, dataset_in){
  # my hacky way to get the variable name back
  variable_name <- dataset_in %>%
    select({{variable_in}}) %>%
    names()

  summary <- dataset_in %>%
    filter(is.na({{variable_in}}) == F) %>%
    group_by(clone, kairomone, metsch) %>%
    dplyr::summarize(
      n = n(),
      eventCount = sum({{variable_in}}),
      proportion = mean({{variable_in}})
    ) %>%
    bind_cols(
      data.frame(binconf(.$eventCount, .$n, 0.05))
    )
  
  ggplot(summary, aes(x = kairomone, y = proportion, color = clone)) +
    
    # geoms
    #
      geom_path(
        aes(
          group = interaction(clone, metsch),
          linetype = metsch
        ),
        
        alpha = alpha_global,
         size = 1.5,
        show.legend = T
      ) +
  
      geom_errorbar(
        aes(
          ymin = Lower,
          ymax = Upper,
          color = clone,
          linetype = metsch
        ),
        
        size = 1.5,
        width = 0.05,
        alpha = alpha_global
      ) +
      
      
      # fill
      geom_point(
        aes(
          group = clone,
          shape = metsch
        ),
        
        alpha = alpha_global - 0.3,
        size = size_point * 0.8,
        stroke = 0
      ) +
      
      # stroke
      geom_point(
        aes(color = clone),
        shape = 1,
        size = size_point,
        stroke = 1.8,
        alpha = alpha_global,
      ) +
      
      geom_text_repel(
        aes(label = eventCount,
            fontface = "bold",
            color = clone
        ),
        # size = size_textRepel,
        alpha = alpha_global - 0.35,
        nudge_x = 0.04,
        nudge_y = 0.03,
        
        show.legend = F,
        segment.color = NA
      ) +
    
    # Manual scales
    #
    
      scale_linetype_manual(
        values = c(
          "TRUE" = "solid",
          "FALSE" = "dashed"
        ),
        
        name = "Metschnikowia exposure"
      ) +
      
      scale_color_manual(
        "legend",
        values = c(
          "c1" = color_c1,
          "m37" = color_m37,
          "w2" = color_w2
        )
      ) +
      
      scale_shape_manual(
        values = c(
          "TRUE" = 19,
          "FALSE" = 1
        ),
        name = "Metschnikowia exposure"
      ) +
    
    # Aesthetics
    #
    
    theme(
      axis.line = element_line(
        colour = 'gray',
        # size = thickness_axisLine
      ),
      # legend.position = "none",
      text=element_text(family = fontChoice),
      # panel.grid.major.y = element_line(color = col_grid),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      
      # sizing up nonsense
      # axis.text = element_text(size = size_axis),
      # plot.title = element_text(size = size_title),
      # axis.title = element_text(size = size_axisTitle),
    ) +
    
    ylim(0, max(summary$Upper) * 1.2) +
    xlab(paste0("kairomone exposure")) +
    ylab(variable_name) #
}
