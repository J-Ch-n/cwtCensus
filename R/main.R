### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, birth_month, size_at_age = length_at_age, rel_mort = release, nat_mort = nat_mort_default,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000, min_harvest_rate = 0.01,
                    detail = T, alpha = 0.05) {
  # Throws warning or error if necessary.
  # TODO: Update error handler.
  # error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
  #            sex, fisheries, bootstrap, iter)

  if (!bootstrap) {
    iter = 1
  }

  # Prepare necessary data for cohort reconstruction.
  clean_data = data_prep(rel, reco, size_at_age,
                         birth_month = birth_month, min_harvest_rate = min_harvest_rate,
                         bootstrap = bootstrap, iter = iter)
  # Conduct cohort reconstruction
  final_data = cohort_reconstruct(clean_data$maturation, clean_data$impact,
                                  nat_mort, birth_month, clean_data$max_age_month_df,
                                  detail = detail, bootstrap = bootstrap,
                                  alpha = alpha, iter = iter,
                                  release_info = clean_data$release_info)
  # TODO: This part is for debugging purposes only.
  view(final_data$cohort)

  if (detail) {
    view(final_data$srr_dt)
    view(final_data$els_dt)
    view(final_data$air_dt)
  }

  # Return and print outputs.
  # TODO: Figure out how to effectively display results.
  return(final_data)
}


