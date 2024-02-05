library(dplyr)
library(tidyr)

#
# Data import ----
#

raw <- read.csv('./data/02_lifehistoryData.csv') %>%
  mutate(day_infection = as.numeric(day_infection))

  
#
# Acceptable object for filtering out errors
#

  acceptable <- list(
    clone = c("c1", "m37", "w2"),
    metsch = c(T, F),
    kairomone = c(T, F),
    block = c(1, 2, 4, 5, 6)
  )


  make_acceptable <- function(dataset, return = "acceptable"){
    acceptable_sub <- acceptable[names(acceptable) %in% names(dataset)]
    
    check_acceptability <- function(criterion_name){
      dataset[[criterion_name]] %in% acceptable_sub[[criterion_name]]
    }
    
    acceptableDF <- sapply(X = names(acceptable_sub), FUN = check_acceptability) %>%
      as.data.frame() %>%
      mutate(fullyAcceptable = if_all(.cols = everything(), ~.x==T))
    
    dataset$fullyAcceptable <- acceptableDF$fullyAcceptable
    
    if(return == "acceptable"){return(filter(dataset, fullyAcceptable == T))} else if(return == "unacceptable"){return(filter(dataset, fullyAcceptable == F))}
  }
  
  data_good <- make_acceptable(raw)
  data_bad <- make_acceptable(raw, return = "unacceptable")
  
#
# Saving data ----
#
  
  write.csv(
    data_good %>%
      select(-fullyAcceptable),
    file = "./data/03_lifehistoryData.csv",
    row.names = F
  )
