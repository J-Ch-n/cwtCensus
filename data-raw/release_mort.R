library(tidyverse)
library(lubridate)

release_mort = read.csv("./data-raw/raw_release_mort.csv")
release_mort |> colnames() <- c('run_year', 'fishery', 'location', 'month', 'rate')
write.csv("release_mort", "./data-raw/release_mort.csv", row.names = FALSE)
usethis::use_data(release_mort, overwrite = TRUE)
