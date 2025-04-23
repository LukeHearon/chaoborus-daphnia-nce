library(dplyr)

#
# Data import ----
#
  data_lifehistory <- read.csv('./data/03_lifehistoryData.csv')
  data_assay <- read.csv('./data/03_assayData.csv')
  intervals <- readRDS('./data/04_intervals.rds')
  
  # get baseline infection rate (averaging across other variables)
  infection_percent_baseline <- data_lifehistory %>% 
    filter(metsch==T) %>% 
    {sum(.$infectionFate)/nrow(.)}
  
  infections_odds_baseline <- infection_percent_baseline/(1-infection_percent_baseline)
  
  
  # get a baseline feeding rate
  feedingrate_baseline <- data_assay %>%
    # filter(metsch==T) %>% 
    {mean(.$feedingRate, na.rm=T)}
  
  ci <- intervals$logistic$feedingRate_susceptibility['feedingRate',]

  
#
# Prediction function ----
#
  predict_infection <- function(prop_feedingrate_increase, scale='relative'){
  # predict change in infection given a proportional increase in feeding rate
  # e.g., how does infection change given a 30% increase in feeding rate implies prop_feedingrate_increase == 0.3

  
  # the base confidence interval needs two adjustments:
  # first, since the feeding rate values are in units of mL/h,
  # the CI represents the effect of changing feeding rate by 1mL/h
  # that's insanely large for Daphnia! So we have to downscale
  # to a realistic value. Here I'm using the average feeding rate
  # of the metsch group (across all assays).
  
  # Second, we want to scale to some increase of interest (prop_feedingrate_increase)
  # We aren't interested in the effect of doubling feeding rate (probably).
  # The value here is a bit subjective, or anyways depends on the question.
  # At the moment, I'm using 30%, which I'll use in the manuscript
  ci_scaled <- ci*(feedingrate_baseline*prop_feedingrate_increase)

  predict_percent <- function(effect_logodds){
    predict_logodds <- log(infections_odds_baseline) + effect_logodds
    predict_odds <- exp(predict_logodds)
    predict_percent <- predict_odds/(1+predict_odds)
    
    return(predict_percent)
  }
  
  predictions <- sapply(ci_scaled, predict_percent)
  
  if(scale=='absolute'){return(predictions)}
  if(scale=='relative'){return((predictions - infection_percent_baseline)/infection_percent_baseline)}
  }
  
  predict_infection(0.3)
  