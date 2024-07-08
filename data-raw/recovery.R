
######################################################
library(tidyverse)
library(lubridate)

recovery = read.csv("./data-raw/raw_recoveries.csv")
site_code = read.csv("./data-raw/raw_sitearea.modified.csv")
size_limit = read.csv("./data-raw/raw_size_limits.csv")

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

recovery <- recovery %>%
  mutate(month = month(ymd(recovery_date))) %>%
  select(run_year,
         recovery_id,
         fishery,
         tag_code,
         sex,
         month,
         sampling_site,
         recovery_location_code,
         sampling_site,
         reporting_agency,
         estimated_number,
         length)

recovery <- left_join(recovery, release, by = "tag_code") %>%
  select(-c("release_month",
            "prod_exp",
            "total_release"))

site_code <- site_code %>%
  mutate(sampling_site = sampsite,
         location = area.1) %>%
  select(sampling_site, location)

recovery <- left_join(recovery, site_code, by = "sampling_site")

size_limit <- size_limit %>%
  mutate(location = Location,
         month = Month,
         size_limit = limit) %>%
  select(-c('Month', 'Location', 'limit'))
recovery <- recovery %>%
  filter(fishery %in% c(10, 40, 54, 50, 46)) %>%
  mutate(est_num = estimated_number) %>%
  left_join(size_limit, by = c('run_year', 'fishery', 'location', 'month')) %>%
  select("run_year",
         "fishery",
         "tag_code",
         "length",
         "sex",
         "month",
         "location",
         "size_limit",
         "est_num")

write.csv("recovery", "./data-raw/recovery.csv", row.names = FALSE)
usethis::use_data(recovery, overwrite = TRUE)
