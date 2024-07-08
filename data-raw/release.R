library(tidyverse)
library(lubridate)

release = read.csv("./data-raw/raw_releases.csv")
release$cwt_1st_mark_count[is.na(release$cwt_1st_mark_count)] <- 0
release$cwt_2nd_mark_count[is.na(release$cwt_2nd_mark_count)] <- 0
release$non_cwt_1st_mark_count[is.na(release$non_cwt_1st_mark_count)] <- 0
release$non_cwt_2nd_mark_count[is.na(release$non_cwt_2nd_mark_count)] <- 0

release$Total_Released <- (release$cwt_1st_mark_count
                           + release$cwt_2nd_mark_count
                           + release$non_cwt_1st_mark_count
                           + release$non_cwt_2nd_mark_count)

release$prod_exp <- release$cwt_1st_mark_count / release$Total_Released

names(release)[7] <- "tag_code"

release <- release %>%
  mutate(release_month = month(ymd(last_release_date)),
         total_release = Total_Released) %>%
  select(release_month,
         brood_year,
         tag_code,
         prod_exp,
         total_release)

write.csv("release", "./data-raw/release.csv", row.names = FALSE)
usethis::use_data(release, overwrite = TRUE)
