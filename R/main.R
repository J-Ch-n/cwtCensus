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
#' @param fisheries named list for associating each type of fishery with a unique identifier in the release and recovery data.
#' Must contain the follow named elements:
#' - "oc_rec": The identifier for recreational ocean fishery.
#' - "oc_com": The identifier for commercial ocean fishery.
#' - "esc_sp": The identifier for escapement to spawning ground.
#' - "esc_hat": The identifier for escapement to hatchery.
#' - "riv_harv": The identifier for river harvest.
#' @param bootstrap Boolean for if parametric bootstrapping should be conducted. If `bootstrap` is set to false, then `iter` is ignored.
#' @param iter numeric or integer to indicate the number of iterations for which we conduct parametric bootstrapping. The default value is 1000.  Caution, the larger
#' this number is, the most computing power it requires, especially the size of memory.
#' @param min_harvest_rate double for the lower bound for the harvest rate considered in cohort reconstruction.
#' @param detail Boolean for if the reconstruction calculates the break downs and summary information.
#' @param level double between 0 and 1 inclusive for the credible level for credible intervals. This option only matters if `bootstrap` is set to TRUE.
#' @param hpd Boolean to use highest posterior density credible interval. If FALSE, a symmetric credible interval is used.
#'
#' @return
#' @export
#'
#' @examples
cohort_recon <- function(rel, reco, birth_month, fisheries = list(oc_rec = 40,
                                                                  oc_com = 10,
                                                                  esc_sp = 54,
                                                                  esc_hat = 50,
                                                                  riv_harv = 46),
                         size_at_age = length_at_age, rel_mort = release_mort, nat_mort = nat_mort_default,
                         d_mort = 0.05, hr_mort_com = 0.26, hr_mort_rec = 0.14,
                         sex = "both", bootstrap = TRUE, iter = 1000, min_harvest_rate = 0,
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
                         sex = sex,
                         spawn = fisheries[["esc_sp"]],
                         hatchery = fisheries[["esc_hat"]],
                         river = fisheries[["riv_harv"]],
                         ocean_r = fisheries[["oc_rec"]],
                         ocean_c = fisheries[["oc_com"]],
                         d_mort = d_mort,
                         hr_c = hr_mort_com,
                         hr_r = hr_mort_rec,
                         rel_mort = rel_mort)

  if (verbose) {
    message("Initiating cohort reconstruction.\n")
  }

  final_data = reconstruct(mat_dt = clean_data$maturation,
                           imp_dt = clean_data$impact,
                           nat_mort = nat_mort,
                           birth_month = birth_month,
                           max_ag_mnth_dt = clean_data$max_age_month_dt,
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
