######################################################
#### REMEMBER TO REMOVE LIBRARIES FROM ENVIRONMENT ###
######################################################
library(tidyverse)
library(lubridate)
# REL_COL = c("release_month",
#             "birth_month",
#             "tag_code",
#             "prod_exp",
#             "total_released",
#             "brood_year")
# RECO_COL = c("run_year",
#               "brood_year",
#               "recovery_id",
#               "fishery",
#               "tag_code",
#               "length",
#               "sex",
#               "month",
#               "location",
#               "size_limit",
#               "est_num",
#               "length")

release = read.csv("./temp/releases.csv")
recovery = read.csv("./temp/recoveries.csv")
site_code = read.csv("./temp/sitearea.modified.csv")

# Create a sample TAG_CODE data frame

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
         total_release)

# Create a sample RECO data frame for the package
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

# Join with release to add BROOD_YEAR column.
recovery <- left_join(recovery, release, by = "tag_code") %>%
  select(-c("release_month",
            "prod_exp",
            "total_release"))

# Rename SAMPLING_SITE and LOCATION to match RECOVERY.
site_code <- site_code %>%
  distinct(sampsite, .keep_all = TRUE) %>%
  mutate(sampling_site = sampsite,
         location = area.1) %>%
  select(sampling_site, location)

# Join with location data to fill LOCATION.
recovery <- left_join(recovery, site_code, by = "sampling_site")

# Add LENGTH_AT_AGE and SIZE_LIMIT
##############################################
### FILL WITH TEMP SIZE_LIMI VALUE FOR NOW ###
##############################################
recovery <- recovery %>%
  mutate(size_limit = case_when(
                        location %in% c('FB', 'KO', 'KC') ~ 26,
                        location == 'SF' ~ 24,
                        location == 'MO' ~ 20),
         est_num = estimated_number) %>%
  select(c("run_year",
          "brood_year",
          "recovery_id",
          "fishery",
          "tag_code",
          "length",
          "sex",
          "month",
          "location",
          "size_limit",
          "est_num",
           "length"))


write.csv("release", "./temp/sample_release.csv", row.names = FALSE)
write.csv("recovery", "./temp/sample_recovery.csv", row.names = FALSE)
