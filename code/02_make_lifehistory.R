library(dplyr)
library(data.table)

data_change <- read.csv('./data/01_changeData.csv')

source("./code/01_functions.R")

#
# Function library
#

  calculate.r1 <- function(ref_in){
    
    lifetable <- data_change %>%
      filter(ref == ref_in, offspring > 0) %>%
      select(ref, day, offspring) %>%
      mutate( # I'm adding some columns here that aren't mathematically necessary (LX = 1; MX = offspring), but it'll make it easier to compare to our spreadsheets for r values
        LX = 1, # the proportion of individuals surviving each day
        MX = offspring,
        LX.MX = LX * MX,
        X = day,
        X.LX.MX = X * LX.MX
      )
    
    if(nrow(lifetable) == 0){return(0)}
    
    R0 <- lifetable %>%
      {(log(sum(.$LX.MX)) * sum(.$LX.MX))/sum(.$X.LX.MX)}
    
    lifetable_r1 <- lifetable %>%
      select(X) %>%
      mutate(
        X.R0neg = X * (- R0),
        exp = exp(X.R0neg),
        exp.LX.MX = exp * lifetable$LX.MX,
        exp.X.LX.MX = exp * lifetable$X.LX.MX
      )
    
      sum_exp.LX.MX <- sum(lifetable_r1$exp.LX.MX)
      sum_exp.X.LX.MX <- sum(lifetable_r1$exp.X.LX.MX)
      
      R1 <- R0 + ((sum_exp.LX.MX * log(sum_exp.LX.MX))/sum_exp.X.LX.MX)
  
    return(R1)
  }
  
  get_lifehistory <- function(ref_in){
    sub <- data_change %>%
      filter(ref == ref_in)
    
    fate <- sub %>%
      filter(day == max(.$day, na.rm = T)) %>%
      .$status
    
    day_fate <- max(sub$day, na.rm = T)
    
    infectionFate <- sub %>%
      filter(day == max(.$day, na.rm = T)) %>%
      {"yes" %in% .$infected}
    
    day_infection <- sub %>%
      filter(infected == "yes") %>%
      {ifelse(
        nrow(.) == 0,
        NA,
        max(.$day, na.rm = T)
      )}
    
    day_clutch1 <- sub %>%
      filter(offspring > 0) %>%
      .$day %>%
      min() %>%
      ifelse(. == Inf, NA, .)
    
    r1 <- calculate.r1(ref_in)
    
    return(
      data.frame(
        fate = fate,
        day_fate = day_fate,
        infectionFate = infectionFate,
        day_infection = day_infection,
        day_clutch1 = day_clutch1,
        r1 = r1
      )
    )
  }
  

#
# Data shaping
#
  
  repDF <- data.frame(
    ref = data_change$ref %>%
      unique()
  ) %>%
    bind_cols(
      sapply(X = .$ref, FUN = explode.ref, simplify = F) %>%
        rbindlist(),
      sapply(X = .$ref, FUN = get_lifehistory, simplify = F) %>%
        rbindlist()
    )
  
write.csv(
  x = apply(repDF, 2, as.character),
  file = "./data/02_lifehistoryData.csv",
  row.names = F
)
