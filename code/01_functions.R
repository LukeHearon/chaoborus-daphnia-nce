library(stringr)
library(dplyr)

# a function to break down refernces into the constituent information
  explode.ref <- function(refCol){
    explode <- str_split(refCol, pattern = "_", simplify = T) %>%
      as.data.frame()
    
    names(explode) <- c("treatment", "clone", "block.rep")
    
    explode <- explode %>%
      mutate(
        kairomone = str_sub(treatment, end = 2L) == "k+",
        metsch = str_sub(treatment, start = 3L, end = 4L) == "m+",
        block = str_split(block.rep, pattern = "\\.", simplify = T)[,1],
        rep = str_split(block.rep, pattern = "\\.", simplify = T)[,2]
      ) %>%
      subset(
        select = -c(block.rep)
      )
    
    explode$clone[explode$clone == "c"] <- "control"
    
    return(explode)
  }

# a function to relativize a given date in terms of days of that Daphnia's life
# (this is a good candidate to move to functions.R)
get.day <- function(block_in, date_in){
  
  if(!(block_in %in% blockDF$block)){return(NA)}
  
  date_blockStart <- subset(blockDF, block == block_in)$date_start
  
  day_relative <- diff.Date(c(date_blockStart, date_in)) %>%
    as.integer() # as.numeric() returns decimals
  
  # note: in my last dataset, I added 1 to all the day values
  # this was because I set up with "24 hour old neonates"
  # however, the real ages will be between 0 and 24, so 0 is just as valid
  # and since I think 0 is easier to read (day 7 is one week, not day 8), I'm using it for this dataset
  
  return(day_relative)
}