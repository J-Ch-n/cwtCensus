# Use mortality rates from https://www.researchgate.net/publication/279530889_Sacramento_River_Winter_Chinook_Cohort_Reconstruction_Analysis_of_Ocean_Fishery_Impacts
nat_mort_default = data.frame(age = rep(2 : 6, each = 12),
                              month = rep(1 : 12, times = 5),
                              rate = c(rep(0.0561, times = 12), rep(0.0184, times = 3), rep(0.0561, times = 4), rep(0.0184, times = 41)))


usethis::use_data(nat_mort_default, overwrite = TRUE)
