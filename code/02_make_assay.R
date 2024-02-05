library(tidyr)
library(dplyr)
library(googlesheets4)
library(stringr)

source("./code/01_functions.R")

#
# Importing raw data ----
#
  
  data_fluorometry <- read.csv("./data/01_fluorometryData.csv")
  data_morphometrics <- read.csv("./data/01_morphometricsData.csv")
  data_spineMetsch <- read.csv("./data/01_spineMetsch.csv")
  blockDF <- read.csv('./data/01_blocks.csv')

  
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
  ) %>%
    full_join(
      data_spineMetsch,
      by = c("date", "ref")
    )
  
  data_long <- data_join %>%
    bind_cols(
      explode.ref(.$ref)
    ) %>%
    filter(clone != "control") %>%
    mutate(
      day = mapply(
        FUN = get.day,
        block_in = block,
        date_in = date
      ),
      
      assay = round(day/7, digits = 0)
    )
  
  
#
# saving data ----
#
  
  write.csv(
    data_long,
    "./data/02_assayData.csv",
    row.names = F
  )
   
  