library(dplyr)

scripts <- list.files('./code', full.names = T)

sapply(scripts, source)
