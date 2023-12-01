library(ggplot2)
library(dplyr)

plot.scatter <- function(variable_x, variable_y, variable_fill, dataset_in){
  name_varX <- dataset_in %>%
    select({{variable_x}}) %>%
    names()
  
  name_varY <- dataset_in %>%
    select({{variable_y}}) %>%
    names()
  
  name_varFill <- dataset_in %>%
    select({{variable_fill}}) %>%
    names()
  
  ggplot(
    dataset_in,
    aes(
      x = {{variable_x}},
      y = {{variable_y}},
      color = clone
    )
  ) +
    
    # fill
    geom_point(
      aes(
        group = clone,
        shape = {{variable_fill}}
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
    
    geom_smooth(
      aes(linetype = {{variable_fill}}),
      color = "black",
      method = lm,
      se = F,
      size = size_path,
      alpha = alpha_global,
      fullrange = T,
      show.legend = F
    ) +
    
    # Manual scales
    #
      
      scale_linetype_manual(
        values = c(
          "TRUE" = "solid",
          "FALSE" = "dashed"
        ),
        
        name = name_varFill
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
        name = name_varFill
      ) +
    # aesthetics
    #
      theme(
        axis.line = element_line(colour = 'gray', size = thickness_axisLine),
        text=element_text(family = fontChoice),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        # sizing up nonsense
        axis.text = element_text(size = size_axisText),
        axis.title = element_text(size = size_axisTitle)
    )
}