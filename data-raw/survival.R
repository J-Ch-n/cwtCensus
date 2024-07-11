# Use mortality rates from https://www.researchgate.net/publication/279530889_Sacramento_River_Winter_Chinook_Cohort_Reconstruction_Analysis_of_Ocean_Fishery_Impacts
survival_default = data.frame(age = 2:6,
                              rate = c(0.5, rep(0.8, times = 4)))

usethis::use_data(survival_default, overwrite = TRUE)
