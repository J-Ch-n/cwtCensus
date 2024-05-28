### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, birth_month, size_at_age = length_at_age, rel_mort = release, nat_mort = nat_mort_default,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000, min_harvest_rate = 0.01,
                    detail = T) {
  # Throws warning or error if necessary.
  # error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
  #            sex, fisheries, bootstrap, iter)

  # Prepare necessary data for cohort reconstruction.
  clean_data = data_prep(rel, reco, size_at_age, birth_month = birth_month, min_harvest_rate = min_harvest_rate, bootstrap = bootstrap, iter = iter)
  # Conduct cohort reconstruction
  cohort_reconstruct(clean_data$maturation, clean_data$impact, nat_mort, birth_month, clean_data$max_age_month_df, detail = detail, bootstrap = bootstrap)
}


