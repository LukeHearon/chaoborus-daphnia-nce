library(dplyr)
library(here)
library(tidyr)
library(lmerTest)


#
# Data import ----
#
  
  data_assay <- here("data", "03_assayData.csv") %>%
    read.csv() %>%
    mutate(
      block = factor(block),
      clone = factor(clone),
      kairomone = factor(kairomone),
      metsch = factor(metsch),
      ref = factor(ref)
    )
  
  data_lifehistory <- here("data", "03_lifehistoryData.csv") %>%
    read.csv() %>%
    mutate(
      block = factor(block),
      clone = factor(clone),
      kairomone = factor(kairomone),
      metsch = factor(metsch),
      ref = factor(ref)
    )
  
  data_wide <- data_assay  %>%
    left_join(
      data_lifehistory
    )

#
# MODELS ----
#
  models <- list()
  
  
  # Feeding rate
  #
    models$linear$feedingRate <- lmer(
      feedingRate ~ clone + kairomone + metsch + day + (1|block) + (1|ref) + # main effects
        (clone + kairomone + metsch)^2 + # two-way interactions, experimental factors
        (metsch:day) + (kairomone:day) + # two-way interactions, effects of time
        (clone:kairomone:metsch),
      data = data_assay
    )
    
    models$linear$feedingRate_bodyFactor <- lmer(
      feedingRate ~ clone + kairomone + metsch + day + (1|block) + (1|ref) + # main effects
        (clone + kairomone + metsch)^2 + # two-way interactions, experimental factors
        (metsch:day) + (kairomone:day) + # two-way interactions, effects of time
        (clone:kairomone:metsch) +
        body, # add in body
      data = data_assay
    )
    
    models$logistic$feedingRate_susceptibility <- lmer(
      infectionFate ~ feedingRate + kairomone + (1|clone) + (1|block), # using feeding rate in the first assay as it has more observations
      data = data_wide %>% 
        filter(assay == 1, metsch == T)
    )
  
  # Morphometrics
  #
    
    # Spine
    #
    
      models$linear$spine <- lmer(
        spine ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref), # having day in there with a 4-way interaction might be questionable, but the model is not at all sensitive to higher-order interactions
        data = data_assay %>%
          filter(spineBroken == F)
      )
      
      models$linear$spineRel <- lmer(
        spineRel ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
        data = data_assay %>%
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )
    
      # rescue
      models$linear$spine_rescue_r1 <- lmer(
        spine ~ clone*kairomone*metsch*day + r1 + (1|block) + (1|ref),
        data = data_wide %>% 
          filter(spineBroken == F)
      )
      
      models$linear$spine_rescue_feeding <- lmer(
        spine ~ clone*kairomone*metsch*day + feedingRate + (1|block) + (1|ref),
        data = data_wide %>% 
          filter(spineBroken == F)
      )
      
      
      models$linear$spineRel_rescue_r1 <- lmer(
        spineRel ~ clone*kairomone*metsch*day + r1 + (1|block) + (1|ref),
        data = data_wide %>% 
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )
      
      models$linear$spineRel_rescue_feeding <- lmer(
        spineRel ~ clone*kairomone*metsch*day + feedingRate + (1|block) + (1|ref),
        data = data_wide %>% 
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )
  
    # body size
    #
    
      models$linear$body <- lmer(
        body ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
        data = data_assay
      )
        
    # infection susceptibility
    #
    
      models$logistic$susceptibility <- glmer(
        infectionFate ~ kairomone + (1|clone) + (1|block), # clone as random because of low observation count
        family = binomial,
        data = data_lifehistory %>%
          filter(metsch == T, fate != "lost")
      )
      
      # rescue
      models$logistic$susceptibility_rescue_r1 <- glmer(
        infectionFate ~ kairomone + r1 + (1|clone) + (1|block), # clone as random because of low observation count
        family = binomial,
        data = data_lifehistory %>%
          filter(metsch == T, fate != "lost")
      )
      
      # models$logistic$susceptibility_rescue_feeding <- glmer(
      #   infectionFate ~ kairomone + feedingRate + (1|clone) + (1|block), # clone as random because of low observation count
      #   family = binomial,
      #   data = data_wide %>%
      #     filter(metsch == T, fate != "lost", assay == 1)
      # )
  
#
# Generate confidence intervals ----
#
  intervals <- list()
  intervals$linear <- lapply(models$linear, confint)
  intervals$logistic <- lapply(models$logistic, confint)
  
#
# Saving objects ----
#
  
  saveRDS(
    models,
    file = './data/04_models.rds'
  )
  
  saveRDS(
    intervals,
    file = './data/04_intervals.rds'
  )
  