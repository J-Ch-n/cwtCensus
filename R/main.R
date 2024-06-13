#' Title
#'
#' @param rel
#' @param reco
#' @param birth_month
#' @param size_at_age
#' @param rel_mort
#' @param nat_mort
#' @param sex
#' @param fisheries
#' @param bootstrap
#' @param iter
#' @param min_harvest_rate
#' @param detail
#' @param level
#' @param hpd
#'
#' @return
#' @export
#'
#' @examples
ch_reco <- function(rel, reco, birth_month, size_at_age = length_at_age, rel_mort = release, nat_mort = nat_mort_default,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000, min_harvest_rate = 0,
                    detail = T, level = 0.05, hpd = T) {
  # Throws warning or error if necessary.
  error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
             sex, fisheries, bootstrap, iter)

  if (!bootstrap) {
    iter = 1
  }

  # Prepare necessary data for cohort reconstruction.
  clean_data = data_prep(rel, reco, size_at_age,
                         birth_month = birth_month, min_harvest_rate = min_harvest_rate,
                         bootstrap = bootstrap, iter = iter)
  # Conduct cohort reconstruction
  final_data = reconstruct(clean_data$maturation, clean_data$impact,
                                  nat_mort, birth_month, clean_data$max_age_month_df,
                                  detail = detail, bootstrap = bootstrap,
                                  level = level, iter = iter,
                                  release_info = clean_data$release_info, hpd = hpd)

  # Return and print outputs.
  #return(cohort_data(final_data, bootstrap, detail))
  return(create_output(final_data, birth_month = birth_month, bootstrap = bootstrap, iter = iter, detail = detail))
}
