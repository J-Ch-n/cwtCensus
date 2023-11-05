library(tidyverse)
library(lubridate)

release = read.csv("./data-raw/raw_releases.csv")
release$cwt_1st_mark_count[is.na(release$cwt_1st_mark_count)] <- 0
release$cwt_2nd_mark_count[is.na(release$cwt_2nd_mark_count)] <- 0
release$non_cwt_1st_mark_count[is.na(release$non_cwt_1st_mark_count)] <- 0
release$non_cwt_2nd_mark_count[is.na(release$non_cwt_2nd_mark_count)] <- 0

#Phi is the proportion of released fish that have CWT and Ad Clips
#Fish with CWT and Ad Clip: cwt_1st_mark_count
#Fish with CWT and No clip: cwt_2nd_mark_count
#Fish with no CWT and Ad Clip: non_cwt_1st_mark_count
#Fish with no CWT and No Clip: non_cwt_2nd_mark_count
release$Total_Released <- (release$cwt_1st_mark_count
                           + release$cwt_2nd_mark_count
                           + release$non_cwt_1st_mark_count
                           + release$non_cwt_2nd_mark_count)

# Add a column for the production expansion factor.
release$prod_exp <- release$cwt_1st_mark_count / release$Total_Released

names(release)[7] <- "tag_code"
# Create a sample REL data frame for the package
release <- release %>%
  mutate(release_month = month(ymd(last_release_date)),
         total_release = Total_Released) %>%
  select(release_month,
         brood_year,
         tag_code,
         prod_exp,
         total_release) %>%
  # Assume to be March.
  mutate(birth_month = 3)

write.csv("release", "./data-raw/release.csv", row.names = FALSE)
usethis::use_data(release, overwrite = TRUE)
