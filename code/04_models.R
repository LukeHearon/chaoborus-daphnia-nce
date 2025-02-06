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
      feedingRate ~ clone + kairomone + metsch + day + (1|block) + (1|ref),
      data = data_assay
    )
    
    models$linear$feedingRate_body <- lmer(
      feedingRate ~ clone + kairomone + metsch + day + body + (1|block) + (1|ref),
      data = data_assay
    )
    
    models$logistic$feedingRate_susceptibility <- glmer(
      infectionFate ~ feedingRate + kairomone + (1|clone) + (1|block),  # clone as random due to low sample size
      family = binomial,
      data = data_wide %>% 
        filter(assay == 1, metsch == T)  # using feeding rate in the first assay as it has more observations
    )
  
  # Morphometrics
  #
    # Spine
    #
      models$linear$spine <- lmer(
        spine ~ kairomone*metsch + day + clone + (1|block) + (1|block:ref),
        data = data_assay %>%
          filter(spineBroken == F)
      )
      
      models$linear$spineRel <- lmer(
        spineRel ~ kairomone*metsch + day + clone + (1|block) + (1|block:ref),
        data = data_assay %>%
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )
    
      # rescue
      models$linear$spine_rescue_r1 <- lmer(
        spine ~ kairomone*metsch + day + clone + r1 + (1|block) + (1|block:ref),
        data = data_wide %>% 
          filter(spineBroken == F)
      )
      
      models$linear$spine_rescue_feeding <- lmer(
        spine ~ kairomone*metsch + day + clone + feedingRate + (1|block) + (1|block:ref),
        data = data_wide %>% 
          filter(spineBroken == F)
      )
      
      models$linear$spineRel_rescue_r1 <- lmer(
        spineRel ~ kairomone*metsch + day + clone + r1 + (1|block) + (1|block:ref),
        data = data_wide %>% 
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )
      
      models$linear$spineRel_rescue_feeding <- lmer(
        spineRel ~ kairomone*metsch + day + clone + feedingRate + (1|block) + (1|block:ref),
        data = data_wide %>% 
          filter(spineBroken == F) %>% 
          mutate(spineRel = spine/body)
      )

      
  
    # body size
    #
      models$linear$body <- lmer(
        body ~ kairomone*metsch + day + clone + (1|block) + (1|block:ref),
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
  