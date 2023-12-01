library(tidyr)
library(dplyr)
library(stringr)

source("./code/functions.R")

#
# Importing raw data ----
#
  
  data_fluorometry <- read.csv("./data/00_fluorometryData.csv")
  data_morphometrics <- read.csv("./data/00_morphometricsData.csv")
  # data_spinemetsch_raw <- read.table("./data/00_morphometrics_spinemetsch.txt") %>% 
  #   rename("date" = V1, "file" = V2, "measurement" = V3, "value" = V4) %>% 
  #   mutate(ref = str_remove(file, ".tif")) %>% 
  #   select(!file) %>% 
  #   pivot_wider(id_cols = c(date, ref), names_from = measurement, values_from = value) %>% 
  #   mutate()
  
  blockDF <- read.csv("./data/00_blockData.csv")

  
  # a function to relativize a given date in terms of days of that Daphnia's life
  # (this is a good candidate to move to functions.R)
  get.day <- function(block_in, date_in){
    
    if(!(block_in %in% blockDF$block)){return(NA)}
    
    date_blockStart <- subset(blockDF, block == block_in)$date_start
    
    day_relative <- diff.Date(c(as.Date(date_blockStart), as.Date(date_in))) %>%
      as.integer() # as.numeric() returns decimals
    
    return(day_relative)
  }

#
# Shaping ----
#

  data_join <- full_join(
    data_fluorometry, data_morphometrics,
    by = c("date", "ref")
  )
  
  data_long <- data_join %>%
    bind_cols(
      explode.ref(.$ref)
    ) %>%
    filter(clone != "control") %>%
    mutate(
      date = as.POSIXct(date),
      
      day = mapply(
        FUN = get.day,
        block_in = block,
        date_in = date
      ),
      
      assay = round(day/7, digits = 0)
    )
  
  # okay. I think there SHOULD be a way to do this in one fell swoop with pivot_wider. however, it's just over my head at this moment.
  # I'm doing it the long way that should work just as well.
  
    data_assay1 <- data_long %>%
      filter(assay == 1) %>%
      select(-c(clone, kairomone, metsch, block, rep, date, assay, treatment)) %>%
      rename_with(
        .cols = !ref,
        ~ paste0(.x, "_assay1")
      )
    
    data_assay2 <- data_long %>%
      filter(assay == 2) %>%
      select(-c(clone, kairomone, metsch, block, rep, date, assay, treatment)) %>%
      rename_with(
        .cols = !ref,
        ~ paste0(.x, "_assay2")
      )
    
    data_wide <- full_join(
      data_assay1, data_assay2,
      by = "ref"
    )
  
  # dunno why I'm getting some refs with missing data across the whole ref. What dataset are they showing up in?
  # too tired to figure it out today.
  
#
# saving data
#
  
  write.csv(
    data_wide,
    "./data/01_assayData_wide.csv",
    row.names = F
  )
  
  write.csv(
    data_long,
    "./data/01_assayData_long.csv",
    row.names = F
  )
   
  