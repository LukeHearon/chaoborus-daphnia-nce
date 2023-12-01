library(dplyr)
library(here)

t_start <- Sys.time()
list.files(here("code", "dataShaping"), full.names = T) %>%
  {.[1:length(.) - 1]} %>% # trim out this script (prefixed with 99 to make it appear last; yeah it's a kludge but it works and lets me keep my directory structure)
   sapply(source)
t_end <- Sys.time()

print(
  paste0(
    "finished in ",
    round(t_end - t_start),
    " seconds!"
  )
)
