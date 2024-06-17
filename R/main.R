#' Conduct cohort reconstruction for populations with coded wire tags
#'
#' @description
#' `cohort_recon` uses released and recovered coded wire tags to calculate the ocean
#' abundance of the population for each brood year at each age and in each month.
#' The function returns a list of outputs indexed by brood year, age, and month. Each
#' element of the list contains two elements, `data` and `summary`.In general the `data` segment
#' contains the raw data of cohort reconstruction, while the `summary` segment includes
#' statistics from the raw data.
#'
#' Depending on if the `bootstrap` and `detail` flags are set, the `data` and `summary`
#' segments may behave in different ways. For more information, see details.
#'
#' @details
#'
#'
#' @param rel input data frame for CWT releases with columns
#' - release_month
#' - brood_year
#' - tag_code
#' - prod_exp
#' - total_release
#' - birth_month
#' @param reco input data frame for CWT recoveries with columns
#' - run_year
#' - recovery_id
#' - fishery
#' - tag_code
#' - length
#' - sex
#' - month
#' - location
#' - size_limit
#' - est_num
#' @param birth_month integer specifying the birth month of all individuals in the data set.
#' @param size_at_age data frame specifying the mean and standard deviation for the individual body length at each age and month.
#' @param rel_mort data frame specifying the release mortality rate due to ocean fishing in each region.
#' @param nat_mort double specifying the natural mortality rate.
#' @param sex string for which sex or sexes to consider. Must choose from "male", "female", or "both".
#' Here, "male" is a short hand for sperm producing individuals. "Female" is a short hand for egg producing individuals.
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
cohort_recon <- function(rel, reco, birth_month, size_at_age = length_at_age, rel_mort = release, nat_mort = nat_mort_default,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000, min_harvest_rate = 0,
                    detail = TRUE, level = 0.05, hpd = TRUE, verbose = TRUE) {

  error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
             sex, fisheries, bootstrap, iter)

  if (!bootstrap) {
    iter = 1
  }

  if (verbose) {
    message("Preparing data.\n")
  }

  clean_data = data_prep(rel,
                         reco,
                         size_at_age,
                         birth_month = birth_month,
                         min_harvest_rate = min_harvest_rate,
                         bootstrap = bootstrap,
                         iter = iter,
                         sex = sex)
  if (verbose) {
    message("Initiating cohort reconstruction.\n")
  }

  final_data = reconstruct(mat_dt = clean_data$maturation,
                           imp_dt = clean_data$impact,
                           nat_mort = nat_mort,
                           birth_month = birth_month,
                           max_ag_mnth_df = clean_data$max_age_month_df,
                           detail = detail,
                           bootstrap = bootstrap,
                           cr_level = level,
                           iter = iter,
                           rel_info = clean_data$release_info,
                           hpd = hpd)
  if (verbose) {
    message("Cohort reconstruction complete.\n")
  }

  return(create_output(final_data,
                       birth_month = birth_month,
                       bootstrap = bootstrap,
                       iter = iter,
                       detail = detail))
}
