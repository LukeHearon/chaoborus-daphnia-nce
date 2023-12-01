library(dplyr)
library(tidyr)
library(survival)
library(lmerTest)


#
# Data import ----
#
  data_long <- read.csv("./data/02_repDF_long.csv")
  data_wide <- read.csv("./data/02_repDF_wide.csv")

  models <- list() 

#
# Morphometrics ----
#
  # body size
  models$linear$body <- lmer(
    body ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
    data = data_long
  )
  
  # absolute spine size
  models$linear$spine <- lmer(
    spine ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
    data = data_long
  )
  
  # relative spine size
  models$linear$spineRel <- lmer(
    spineRel ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
    data = data_long %>% 
      mutate(spineRel = spine/body)
  )

  # relative spine size, rescue by r1
  models$linear$spineRel_rescue_r1 <- lmer(
    spineRel ~ clone*kairomone*metsch + r1 + (1|block),
    data = data_long %>% 
      filter(assay == 1) %>% 
      mutate(spineRel = spine/body)
  )
  
  # relative spine size, rescue by feeding rate
  models$linear$spineRel_rescue_feeding <- lmer(
    spineRel ~ clone*kairomone*metsch + feedingRate + (1|block),
    data = data_long %>% 
      filter(assay == 1) %>% 
      mutate(spineRel = spine/body)
  )


#
# infection ----
#
  # infection susceptiblity
  models$logistic$susceptibility <- glmer(
    infectionFate ~ kairomone + (1|clone) + (1|block),
    family = binomial,
    data = data_wide %>%
      filter(metsch == T, fate != "lost")
  )
  
  # infection susceptibility, rescue with r1
  models$logistic$susceptibility_rescue_r1 <- glmer(
    infectionFate ~ kairomone + r1 + (1|block) + (1|clone),
    family = binomial,
    data = data_wide %>% 
      filter(metsch == T, fate != "lost")
  )
  
  # infection susceptibility, rescue with feeding rate
  models$logistic$susceptibility_rescue_feeding <- glmer(
    infectionFate ~ kairomone + feedingRate + (1|block) + (1|clone),
    family = binomial,
    data = data_long %>% 
      filter(assay == 1, metsch == T, fate != "lost")
  )

# 
# feeding rate ----
#
  # total feeding rate
  models$linear$feedingRate <- lmer(
    feedingRate ~ clone*kairomone*metsch * day + (1|block) + (1|block:ref),
    data = data_assay
  )
  
  # feeding rate conditioned on body size
  models$linear$feedingRate_bodyFactor <- lmer(
    feedingRate ~ body + clone*kairomone*metsch + (1|ref),
    data = data_assay
  )

#
# Saving object ----
#
  
  saveRDS(
    models,
    file = here("code", "analysis", "models.rds")
  )
  
  
  