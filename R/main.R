#' Conduct cohort reconstruction for populations with coded wire tags
#'
#' @description
#' `cohort_reconstruct` uses released and recovered coded wire tags to calculate the ocean
#' abundance of the population for each brood year at each age and in each month.
#' The function returns a list of outputs indexed by brood year, age, and month. Each
#' element of the list contains two elements, `data` and `summary`. In general, the `data` segment
#' contains the raw data of cohort reconstruction, while the `summary` segment includes
#' statistics from the raw data.
#'
#' Depending on if the `bootstrap` and `detail` flags are set, the `data` and `summary`
#' segments may behave in different ways. For more information, see details.
#'
#' @details
#' ## Aging convention
#' We use fishing age, or year of life, as the age for captured tags. This aging convention starts from age-1. Namely,
#' as soon as the fish is born, it is in the first year of life, thus age-1. Accordingly, the age in the `size_at_age` data frame should use
#' fishing age as well.
#'
#' ## Detail, bootstrap, and results
#' Since `detail` and `bootstrap` are Boolean valued variables, there are four combinations these two variables.
#' \describe{
#'  \item{`detail = True, bootstrap = True`}{Provides results with detailed summary statistics using bootstrapped data.}
#'  \item{`detail = False, bootstrap = True`}{Provides results with cohort data only using bootstrapped data.}
#'  \item{`detail = True, bootstrap = False`}{Provides results with detailed summary statistics using point estimates.}
#'  \item{`detail = False, bootstrap = True`}{Provides results with cohort data only using point estimates.}
#' }
#'
#' ## Danger of incomplete cohort reconstruction
#' If the provided recovery data don't span the entire life time of a stock, cohort reconstruction may fail to
#' yield accurate information. Be sure to provide a complete cohort recovery data set.
#'
#' @param rel Input data frame for CWT releases with columns:
#' \describe{
#'   \item{brood_year}{Integer. The birth year of each CWT batch.}
#'   \item{total_release}{Double. The total number of releases for each batch of CWT.}
#'   \item{prod_exp}{Double. The ratio between the number of tagged fish and the total number of fish in each batch, i.e., number of tagged fish / batch size.}
#'   \item{tag_code}{Any type. The identifier for each batch of CWT release.}
#' }
#' @param reco Input data frame for CWT recoveries with columns:
#' \describe{
#'   \item{run_year}{Integer. The year of each CWT recovery.}
#'   \item{month}{Integer. The month of each CWT recovery.}
#'   \item{length}{Double. The length in inches of each recovered fish.}
#'   \item{tag_code}{Any type. The identifier for each recovered tag.}
#'   \item{fishery}{Double. The identifier for the fishery from which the tag is recovered. The fisheries include river harvest, spawning ground escapement, hatchery escapement, ocean recreational fishing, ocean commercial fishing.}
#'   \item{location}{Character. The identifier for the location at which the tag is recovered.}
#'   \item{size_limit}{Double. The minimum harvest limit (inclusive) in inches for the recovered tag's region and date.}
#'   \item{est_num}{Double. The estimated number of fish each tag represents.}
#'   \item{sex}{Character. The biological sex of the recovered fish.}
#' }
#' @param birth_month Integer specifying the birth month of all individuals in the data set.
#' @param size_at_age Data frame specifying the mean and standard deviation for the individual body length at each age and month.
#' @param rel_mort Data frame specifying the release mortality rate due to ocean fishing in each region.
#' \describe{
#'  \item{run_year}{Integer for the run year.}
#'  \item{fishery}{Identifier for the fishery type.}
#'  \item{location}{Identifier for location.}
#'  \item{month}{Integer for the month.}
#'  \item{rate}{Double for the release mortality rate.}
#' }
#' @param survival Data frame specifying the age specific natural survival rates.
#' \describe{
#'  \item{age}{Numeric specifying the age.}
#'  \item{rate}{Double specifying the agely survival rate.}
#' }
#' @param sex String specifying which sex or sexes to consider. Must choose from "male", "female", or "both". Here, "male" is shorthand for sperm-producing individuals. "Female" is shorthand for egg-producing individuals.
#' @param fisheries Named list associating each type of fishery with a unique identifier in the release and recovery data. Must contain the following named elements:
#' \describe{
#'   \item{oc_rec}{Identifier for recreational ocean fishery.}
#'   \item{oc_com}{Identifier for commercial ocean fishery.}
#'   \item{esc_sp}{Identifier for escapement to spawning ground.}
#'   \item{esc_hat}{Identifier for escapement to hatchery.}
#'   \item{riv_harv}{Identifier for river harvest.}
#' }
#' @param bootstrap Boolean indicating if parametric bootstrapping should be conducted. If `bootstrap` is set to false, then `iter` is ignored.
#' @param iter Numeric or integer specifying the number of iterations for parametric bootstrapping. The default value is 1000. Note that larger values require more computing power and memory.
#' @param min_harvest_rate Double specifying the lower bound for the harvest rate considered in cohort reconstruction.
#' @param detail Boolean indicating if the reconstruction should calculate breakdowns and summary information.
#' @param level Double between 0 and 1 inclusive, specifying the credible level for credible intervals. This option only matters if `bootstrap` is set to TRUE.
#' @param hpd Boolean indicating if the highest posterior density credible interval should be used. If FALSE, a symmetric credible interval is used.
#'
#' @return A three-dimensional list of results. The first dimension encodes brood year information, the second age, and the third month. Depending on `detail` and `bootstrap`, the corresponding output will vary.
#'```
#'     by  age  month
#'
#'├── [[2002]]
#'│   ├── [[2]]
#'│   │   ├── [[1]]
#'│   │   │   ├── `data`
#'│   │       └── `summary`
#'│   ├── [[3]]
#'│   │   ├── [[1]]
#'│   │   │   ├── `data`
#'│   │       └── `summary`
#'```
#'
#' @export
#'
#' @examples
#' cohort_reconstruct(release, recovery, birth_month = 4,
#'   bootstrap = F, last_month = 12, iter = 10,
#'   level = 0.95, detail = F, sex = "both")
cohort_reconstruct <- function(rel, reco, birth_month, last_month, fisheries = list(oc_rec = 40,
                                                                  oc_com = 10,
                                                                  esc_sp = 54,
                                                                  esc_hat = 50,
                                                                  riv_harv = 46),
                         size_at_age = length_at_age, rel_mort = release_mort, survival = survival_default,
                         d_mort = 0.05, hr_mort_com = 0.26, hr_mort_rec = 0.14,
                         bootstrap = TRUE, iter = 1000, min_harvest_rate = 0,
                         detail = TRUE, level = 0.05, hpd = TRUE, verbose = TRUE,
                         sex = "both") {

  error_handler(rel, reco, size_at_age, rel_mort, survival,
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
                         rel_mort = rel_mort,
                         u_bound = last_month,
                         survival = survival)

  if (verbose) {
    message("Initiating cohort reconstruction.\n")
  }

  final_data = reconstruct(mat_dt = clean_data$maturation,
                           imp_dt = clean_data$impact,
                           nat_mort = clean_data$nat_mort,
                           birth_month = birth_month,
                           max_ag_mnth_dt = clean_data$max_age_month_dt,
                           detail = detail,
                           bootstrap = bootstrap,
                           cr_level = level,
                           iter = iter,
                           rel_info = clean_data$release_info,
                           hpd = hpd)

  result = create_output(final_data,
                birth_month = birth_month,
                bootstrap = bootstrap,
                iter = iter,
                detail = detail)

  if (verbose) {
    message("Cohort reconstruction complete.\n")
  }

  return(result)
}
