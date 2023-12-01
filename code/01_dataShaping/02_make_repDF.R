#
# Packages and data import
#
  library(dplyr)
  library(stringr)
  source("./code/functions.R")

  data_assay_wide <- read.csv("./data/01_assayData_wide.csv")
  data_assay_long <- read.csv("./data/01_assayData_long.csv")
  data_lifehistory <- read.csv("./data/01_lifehistoryData.csv")
  
  repDF_wide = full_join(data_assay_wide, data_lifehistory, by = "ref") %>%
    {bind_cols(
      explode.ref(.$ref),
      .
    )}
  
  repDF_long = full_join(data_assay_long, data_lifehistory, by = "ref")
  
  write.csv(
    apply(repDF_wide, 2, as.character),
    file = "./data/02_repDF_wide.csv",
    row.names = F
  )
  
  write.csv(
    apply(repDF_long, 2, as.character),
    file = "./data/02_repDF_long.csv",
    row.names = F
  )
  