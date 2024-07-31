
#' Conduct cohort reconstruction for populations with coded wire tags
#'
#' @description
#' `cohort_reconstruct` uses released and recovered coded wire tags to calculate the ocean \cr
#' abundance of the population for each brood year at each age and in each month. The function \cr
#' returns a list of outputs indexed by brood year, age, and month. Each element of the list \cr
#' contains two elements, `data` and `summary`. In general, the `data` segment contains the \cr
#' raw data of cohort reconstruction, while the `summary` segment includes statistics from the \cr
#' raw data. For more about return value, see the value section. \cr
#'
#' Depending on if the `bootstrap` and `detail` flags are set, the `data` and `summary` segments \cr
#' may behave in different ways. For more information, see details.
#'
#' @details
#' ## Aging convention
#' We use fishing age, or year of life, as the age for captured tags. This aging convention \cr
#' starts from age-1. Namely, as soon as the fish is born, it is in the first year of life, \cr
#' thus age-1. Accordingly, the age in the `size_at_age` data frame should use fishing age \cr
#' as well. \cr
#'
#' ## Detail, bootstrap, and results
#' Since `detail` and `bootstrap` are Boolean valued variables, there are four combinations.
#'  \describe{
#'    \item{`detail = True, bootstrap = True`}{Provides results with detailed summary statistics using bootstrapped data.}
#'    \item{`detail = False, bootstrap = True`}{Provides results with cohort data only using bootstrapped data.}
#'    \item{`detail = True, bootstrap = False`}{Provides results with detailed summary statistics using point estimates.}
#'    \item{`detail = False, bootstrap = True`}{Provides results with cohort data only using point estimates.}
#'  }
#'
#' ## Danger of incomplete cohort reconstruction
#' If the provided recovery data don't span the entire life time of a stock, cohort reconstruction \cr
#'  may fail to yield accurate information. Be sure to provide a complete cohort recovery data set.
#'
#' @param rel Input data frame for CWT releases with columns in order:
#'  \describe{
#'    \item{"brood_year"}{Integer. The birth year of each CWT batch.}
#'    \item{"total_release"}{Double. The total number of releases for each batch of CWT.}
#'    \item{"prod_exp"}{Double. The ratio between the number of tagged fish and the total number \cr
#'                      of fish in each batch, i.e., number of tagged fish / batch size.}
#'    \item{"tag_code"}{Any type. The identifier for each batch of CWT release.}
#' }
#' @param reco Input data frame for CWT recoveries with columns in order:
#'  \describe{
#'    \item{"run_year"}{Integer. The year of each CWT recovery.}
#'    \item{"month"}{Integer. The month of each CWT recovery.}
#'    \item{"length"}{Double. The length in inches of each recovered fish.}
#'    \item{"tag_code"}{Any type. The identifier for each recovered tag.}
#'    \item{"fishery"}{Double. The identifier for the fishery from which the tag is recovered. \cr
#'                      The fisheries include river harvest, spawning ground escapement, hatchery \cr
#'                      escapement, ocean recreational fishing, ocean commercial fishing.}
#'    \item{"location"}{Character. The identifier for the location at which the tag is recovered.}
#'    \item{"size_limit"}{Double. The minimum harvest limit (inclusive) in inches for the recovered \cr
#'                        tag's region and date.}
#'    \item{"est_num"}{Double. The estimated number of fish each tag represents.}
#'    \item{"sex"}{Character. The biological sex of the recovered fish.}
#' }
#' @param birth_month Integer specifying the birth month of all individuals in the data set.
#' @param last_month Integer indicating the last month, in numeric form, that counts toward the \cr
#'  current run. This variable is to separate late spawners of the current run and early spawners \cr
#'  of the next run.
#'  The default value is 12L, meaning the entire natural year counts toward the current run.
#' @param fisheries Named list associating each type of fishery with a unique identifier in the \cr
#'  release and recovery data. Must contain the following named elements:
#'  \describe{
#'    \item{"oc_rec"}{Identifier for recreational ocean fishery. The default value is 40.}
#'    \item{"oc_com"}{Identifier for commercial ocean fishery. The default value is 10.}
#'    \item{"esc_sp"}{Identifier for escapement to spawning ground. The default value is 54.}
#'    \item{"esc_hat"}{Identifier for escapement to hatchery. The default value is 50.}
#'    \item{"riv_harv"}{Identifier for river harvest. The default value is 46.}
#'  }
#' @param size_at_age Data frame specifying the mean and standard deviation for the individual \cr
#'  body length at each age and month. The default data frame is included in the package and named \cr
#'  `length_at_age`.
#'  The following describes the required columns in order.
#'  \describe{
#'    \item{"age"}{Integer. The fishing age of the cohort.}
#'    \item{"month"}{Integer. The month in numeric form. For instance, 3L is March.}
#'    \item{"mean"}{Double. The mean total body length in inches.}
#'    \item{"sd"}{Double. The standard deviation of total body length in inches.}
#' }
#' @param rel_mort Data frame specifying the hook-and-release mortality rate due to ocean fishing in each \cr
#'  region. Hook-and-release mortality rate is the proportion of fish that are The default data frame is included in the package and named `release_mort`. \cr
#'  The following describes the required columns in order.
#'  \describe{
#'    \item{"run_year"}{Integer. The year of each CWT recovery.}
#'    \item{"fishery"}{Any type. The identifier for each type of fishery.}
#'    \item{"location"}{Any type. The identifier for location.}
#'    \item{"month"}{Integer. The month of each CWT recovery.}
#'    \item{"rate"}{Double. The release mortality rate for each timestep.}
#'  }
#' @param survival Data frame specifying the age specific natural survival rates. The default data \cr
#'  frame is included in the package and named `survival_default`. \cr
#'  The following describes the required columns in order.
#'  \describe{
#'    \item{"age"}{Numeric specifying the age.}
#'    \item{"rate"}{Double specifying the agely survival rate.}
#'  }
#' @param d_mort Double indicating the drop off mortality of fishery impact. Drop off mortality is \cr
#'  the proportion of fish encountered by the gear that is killed without being brought to the vessel \cr
#'  intact. The default value is 0.05.
#' @param bootstrap Boolean indicating if parametric bootstrapping should be conducted. If `bootstrap` \cr
#'  is set to false, then `iter` is ignored.
#' @param iter Numeric or integer specifying the number of iterations for parametric bootstrapping. The \cr
#'  default value is 1000. Note that larger values require more computing power and memory.
#' @param detail Boolean indicating if the reconstruction should calculate breakdowns and summary \cr
#'  information.
#' @param min_harvest_rate Double specifying the lower bound for the harvest rate considered in cohort \cr
#'  reconstruction. Harvest rate is the ratio between the number of harvestable individuals and the \cr
#'  size of the cohort.
#' @param level Double between 0 and 1 inclusive, specifying the credible level for credible intervals. \cr
#'  This option only matters if `bootstrap` is set to TRUE.
#' @param hpd Boolean indicating if the highest posterior density credible interval should be used. If \cr
#'  FALSE, a symmetric credible interval is used.
#' @param sex String specifying which sex or sexes to consider. Must choose from "male", "female", or \cr
#'  "both". Here, "male" is shorthand for sperm-producing individuals. "Female" is shorthand for \cr
#'  egg-producing individuals.
#' @param verbose Boolean indicating if the function will print progress updates. If `verbose` is set \cr
#'  to TRUE, the function will provide status updates. This is particularly useful for conducting \cr
#'  bootstrapped cohort reconstruction for large iterations that are high in computing cost, as \cr
#'  having progress updates can instill a sense of confidence into an otherwise discouraged user.
#'
#' @return
#'  There are two types of return value, depending on the values of `detail` and `bootstrap`. \cr
#'  In the case when both `detail` and `bootstrap` are `FALSE`, the return value is a data table of the following form: \cr
#'
#'```
#' | by (integer) | age (integer) | month (integer) | ocean_abundance (double) |
#' |--------------|---------------|-----------------|--------------------------|
#' | 1995         | 3             | 5               | 3.814342                 |
#' | 1995         | 3             | 4               | 10.853284                |
#' | 1995         | 3             | 3               | 11.056992                |
#' | 1995         | 3             | 2               | 11.264524                |
#' | 1995         | 3             | 1               | 11.475951                |
#' | 1995         | 3             | 12              | 11.691346                |
#' | 1995         | 3             | 11              | 11.910784                |
#' | 1995         | 3             | 10              | 12.134340                |
#' | 1995         | 3             | 9               | 12.362093                |
#' | 1995         | 3             | 8               | 12.594120                |
#'
#'```
#'  In the cases when either `detail`, `bootstrap`, or both are set to `TRUE`, the return \cr
#'  value is a three dimensional list. The first dimension encodes brood year information (by), \cr
#'  the second age, and the third month. Each age has an age-specific summary information. Each month has \cr
#' `data` and month-specific `summary` information. Depending on `detail` and `bootstrap`,
#'  the corresponding output will vary. \cr
#'
#'  Here is the structure of the return value: \cr
#'
#'```
#'   "by" "age" "month"
#'
#'├── [["2002"]]
#'│   ├──[["2"]]
#'│   │   ├── [["1"]]
#'│   │   │   ├── `data`
#'│   │   │   └── `summary`
#'│   │   ├── ...
#'│   │   │
#'│   │   ├── [["age_summary"]]
#'│   │   │   ├── `data`
#'│   │       └── `summary`
#'│   ├── [["3"]]
#'│   │   ├── [["1"]]
#'│   │   │   ├── `data`
#'│   │   │   └── `summary`
#'│   │   ├── ...
#'│   │   │
#'│   │   ├── [["age_summary"]]
#'│   │   │   ├── `data`
#'│   │       └── `summary`
#'│   ├── ...
#'│   │
#'│   ├──[["by_summary"]]
#'│   │  ├── `data`
#'│   │  └── `summary`
#'├── ...
#'
#'```
#'
#' ## Summary
#'
#'  - Parameter: the statistics in question.
#'  - Median: the median for the parameter.
#'  - SD: the standard deviation for the parameter.
#'  - CrI_low: the lower bound of the credible interval.
#'  - CrI_high: the upper bound of the credible interval.
#'
#' ### Brood-year-specific summary (under `by_summary`)
#'  - srr: spawner reduction rate.
#' ```
#' | Parameter | Median | SD  | CrI_low | CrI_high |
#' |-----------|--------|-----|---------|----------|
#' | srr       | 0.05   | 0.02| 0.04    | 0.04     |
#' ```
#' ### Age-specific summary (under `age_summary`)
#'  - elsr: early life survival rate.
#' ```
#' | Parameter | Median | SD  | CrI_low | CrI_high |
#' |-----------|--------|-----|---------|----------|
#' | elsr      | 0.02   | 0   | 0.01    | 0.01     |
#' ```
#' ### Month-specific summary (under each month)
#'  - ocean_abundance: number of individuals in the ocean at that time.
#'  - impact: mortality due to fishing.
#'  - maturation: number of spawners.
#'  - natural_mort: mortality due to natural causes.
#' ```
#' | Parameter         | Median | SD   | CrI_low | CrI_high |
#' |-------------------|--------|------|---------|----------|
#' | ocean_abundance   | 43.91  | 6.50 | 35.09   | 35.09    |
#' | impact            | 0.00   | 0.00 | 0.00    | 0.00     |
#' | maturation        | 0.00   | 0.00 | 0.00    | 0.00     |
#' | natural_mort      | 0.81   | 0.12 | 0.65    | 0.65     |
#' ```
#' ## Data
#'  Each labeled entry is one bootstrapped iteration, where the label `n` corresponds \cr
#'  to the nth iteration.
#' ### Brood-year-specific data
#' ```
#' | Parameter | 1    | 2    | 3    | 4    | 5    | 6    | 7    | 8    | 9    | 10   |
#' |-----------|------|------|------|------|------|------|------|------|------|------|
#' | srr       | 0.07 | 0.07 | 0.05 | 0.05 | 0.05 | 0.06 | 0.06 | 0.04 | 0.08 | 0.04 |
#'
#' ```
#' ### Age-specific data
#' ```
#' | Measurement | 1    | 2    | 3    | 4    | 5    | 6    | 7    | 8    | 9    | 10   |
#' |-------------|------|------|------|------|------|------|------|------|------|------|
#' | elsr        | 0.02 | 0.02 | 0.02 | 0.01 | 0.01 | 0.02 | 0.02 | 0.02 | 0.01 | 0.02 |
#' ```
#' ### Month-specific data
#'
#' ```
#' | Measurement      | 1    | 2    | 3    | 4    | 5    | 6    | 7    | 8    | 9    | 10   |
#' |------------------|------|------|------|------|------|------|------|------|------|------|
#' | ocean_abundance  | 46.69| 37.41| 43.34| 40.74| 35.09| 44.49| 37.43| 56.25| 45.80| 50.56|
#' | impact           | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
#' | maturation       | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 |
#' | natural_mort     | 0.86 | 0.69 | 0.80 | 0.75 | 0.65 | 0.82 | 0.69 | 1.04 | 0.84 | 0.93 |
#' ```
#'
#' @export
#'
#' @examples
#'
#' # Bootstrapped (10 iterations) and detailed cohort reconstruction.
#' result = cohort_reconstruct(rel = release, reco = recovery, birth_month = 6L,
#'   last_month = 12L, bootstrap = TRUE, iter = 10L, detail = TRUE)
#'
#' # Bootstrapped (10 iterations) but not detailed cohort reconstruction.
#' result = cohort_reconstruct(rel = release, reco = recovery, birth_month = 6L,
#'   last_month = 12L, bootstrap = TRUE, iter = 10L, detail = FALSE)
#'
#' # Point estimated and detailed cohort reconstruction.
#' result = cohort_reconstruct(rel = release, reco = recovery, birth_month = 6L,
#'   last_month = 12L, bootstrap = FALSE, detail = TRUE)
#'
#' # Point estimated but no detailed cohort reconstruction.
#' result = cohort_reconstruct(rel = release, reco = recovery, birth_month = 6L,
#'   last_month = 12L, bootstrap = FALSE, detail = FALSE)
#'
#' # Setting `iter` to a non-zero number when `bootstrap` is `FALSE` doesn't affect the result.
#'
#' result = cohort_reconstruct(rel = release, reco = recovery, birth_month = 6L,
#'   last_month = 12L, bootstrap = FALSE, iter = 10L, detail = FALSE)
#'

cohort_reconstruct <- function(rel, reco, birth_month, last_month = 12L, fisheries = list(oc_rec = 40,
                                                                  oc_com = 10,
                                                                  esc_sp = 54,
                                                                  esc_hat = 50,
                                                                  riv_harv = 46),
                         size_at_age = length_at_age, rel_mort = release_mort, survival = survival_default,
                         d_mort = 0.05, bootstrap = FALSE, iter = 10L, detail = FALSE,
                         min_harvest_rate = 0, level = 0.05, hpd = TRUE, verbose = TRUE,
                         sex = "both") {

  error_handler(rel, reco, size_at_age, rel_mort, survival,
             sex, fisheries, bootstrap, iter, last_month, birth_month,
             d_mort, detail, min_harvest_rate, level, hpd, verbose)

  if (!bootstrap) {
    iter = 1
  }

  if (verbose) {
    message("Preparing data.\n")
  }

  bootstrap = bootstrap && iter > 1
  iter = max(1, iter)

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
