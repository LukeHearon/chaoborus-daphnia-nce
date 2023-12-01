library(dplyr)

data_change <- read.csv("./data/00_changeData.csv")

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
  
  get.fate <- function(ref_in){
    data_change %>%
      filter(ref == ref_in) %>%
      filter(day == max(.$day, na.rm = T)) %>%
      .$status
  }
  
  get.day_fate <- function(ref_in){
    data_change %>%
      filter(ref == ref_in) %>%
      filter(day == max(.$day, na.rm = T)) %>%
      .$day
  }
  
  get.infectionFate <- function(ref_in){
    data_change %>%
      filter(ref == ref_in) %>%
      filter(day == max(.$day, na.rm = T)) %>%
      {"yes" %in% .$infected}
  }
  
  get.day_infection <- function(ref_in){
    df <- data_change %>%
      filter(ref == ref_in) %>%
      filter(infected == "yes")
    
    if(nrow(df) == 0){return(NULL)}
    
    return(
      df %>%
        {max(.$day, na.rm = T)}
    )
  }

#
# Data shaping
#
  
  repDF <- data.frame(
    ref = data_change$ref %>%
      unique()
  ) %>%
    mutate(
      r1 = sapply(X = ref, FUN = calculate.r1),
      fate = sapply(X = ref, FUN = get.fate),
      day_fate = sapply(X = ref, FUN = get.day_fate),
      infectionFate = sapply(X = ref, FUN = get.infectionFate),
      day_infection = sapply(X = ref, FUN = get.day_infection)
    )

    
write.csv(
  x = apply(repDF, 2, as.character),
  file = "./data/01_lifehistoryData.csv",
  row.names = F
)
