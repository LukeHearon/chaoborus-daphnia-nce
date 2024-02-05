library(dplyr)
library(here)
library(ggplot2)

source('./code/01_plot_aesthetics.R')

#
# Data import ----
#

data_lifehistory <- read.csv('./data/03_lifehistoryData.csv') %>%
  mutate(
    block = factor(block),
    clone = factor(clone),
    kairomone = factor(kairomone),
    metsch = factor(metsch),
    ref = factor(ref)
  )

model_rescue <- readRDS('./data/04_models.rds') %>% 
  {.$logistic$susceptibility_rescue_r1}

summary <-  data_lifehistory %>% 
  filter(metsch == T) %>% 
  group_by(kairomone) %>% 
  summarize(
    r1_min = min(r1, na.rm = T),
    r1_max = max(r1, na.rm = T),
    infections = sum(infectionFate, na.rm = T),
    count = n(),
    infectionRate = infections/count
  )

data_prediction <-  expand.grid(
  clone = c("c1", "m37", "w2"),
  kairomone = c(T, F),
  r1 = c(0.05, 0.19)
) %>% 
  mutate(
    infection_logodds = predict(
      model_rescue,
      re.form = ~(1|clone),
      newdata = .
    ),
    
    infectionRate = exp(infection_logodds)/(1 + exp(infection_logodds))
  )

plot <- ggplot(
  data_prediction,
  aes(
    x = r1,
    y = infectionRate,
    color = kairomone,
    linetype = kairomone,
    group = interaction(clone, kairomone)
  )
) +
  geom_path() +
  
  scale_color_manual(values = colors, limits = force) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = "none") +
  ylab("Predicted infection rate") +
  xlab("Reproductive fitness (r)") +
  labs(color = "Kairomone presence", linetype = "Kairomone presence") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) 


#
# Write ----
#

#
# Write ----
#
  ggsave(
    './plots/plot_infection.svg',
    plot,
    width = 7,
    height = 5
  )
