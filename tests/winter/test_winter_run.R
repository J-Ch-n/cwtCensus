library(dplyr)
library(tidyr)
library(lubridate)

release = read.csv("./tests/winter/CWTReleased.csv")
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
         total_release = Total_Released,
         tag_code = as.character(tag_code)) %>%
  select(release_month,
         brood_year,
         tag_code,
         prod_exp,
         total_release)

######################################################
recovery = read.csv("./tests/winter/CWTRecoveries.csv")
esc_sp = read.csv("./tests/winter/CWT Recoveries SG.csv")
site_code = read.csv("./tests/winter/sitearea.modified.csv")
size_limit = read.csv("./data-raw/raw_size_limits.csv")

# Create a sample RECO data frame for the package
recovery_hat <- recovery |>
  filter(fishery != 54) |>
  mutate(month = month(ymd(recovery_date)),
         tag_code = as.character(tag_code)) |>
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
         length) |>
  drop_na(estimated_number)

# Join with release to add BROOD_YEAR column.
recovery_hat <- left_join(recovery_hat, release, by = c("tag_code")) %>%
  select(-c("release_month",
            "prod_exp",
            "total_release"))

recovery_sp <- esc_sp |>
  mutate(run_year = year(mdy(recdate)),
         month = month(mdy(recdate)),
         tag_code = cwtcode,
         estimated_number = numtags,
         recovery_id = NA,
         sex = NA,
         sampling_site = NA,
         recovery_location_code = NA,
         reporting_agency = NA,
         length = NA) |>
  select(c(
    "run_year", "recovery_id", "fishery", "tag_code", "sex", "month",
    "sampling_site", "recovery_location_code", "reporting_agency",
    "estimated_number", "length", "brood_year", "Value_Expanded"
  ))
# browser()
  recovery <- bind_rows(recovery_sp |> select(-c("Value_Expanded")), recovery_hat)
  release_sp = data.frame(release_month = NA, brood_year = recovery_sp$brood_year,
                          tag_code = recovery_sp$tag_code, prod_exp = 1 / recovery_sp$Value_Expanded, total_release = NA)
  release <- bind_rows(release, release_sp) |> distinct(tag_code, .keep_all = T)

  # browser()
# Rename SAMPLING_SITE and LOCATION to match RECOVERY.
site_code <- site_code %>%
  distinct(sampling_site, .keep_all = TRUE) %>%
  mutate(location = area.1) %>%
  select(sampling_site, location)

# Join with location data to fill LOCATION.
recovery <- left_join(recovery, site_code, by = "sampling_site")

# Add LENGTH_AT_AGE and SIZE_LIMIT
##############################################
### FILL WITH TEMP SIZE_LIMI VALUE FOR NOW ###
##############################################
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
         "recovery_id",
         "fishery",
         "tag_code",
         "length",
         "sex",
         "month",
         "location",
         "size_limit",
         "est_num")

usethis::use_data(recovery, overwrite = TRUE)

######################################################
length_at_age = read.csv("./tests/winter/length.at.age.csv")

usethis::use_data(length_at_age, overwrite = TRUE)

usethis::use_data(release, overwrite = TRUE)

release_mort = read.csv("./tests/winter/release.mort.rate.csv")
release_mort |> colnames() <- c('run_year', 'fishery', 'location', 'month', 'rate')
usethis::use_data(release_mort, overwrite = TRUE)

