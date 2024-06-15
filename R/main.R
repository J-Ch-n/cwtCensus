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

  error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
             sex, fisheries, bootstrap, iter)

  if (!bootstrap) {
    iter = 1
  }

  clean_data = data_prep(rel,
                         reco,
                         size_at_age,
                         birth_month = birth_month,
                         min_harvest_rate = min_harvest_rate,
                         bootstrap = bootstrap,
                         iter = iter)

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

  return(create_output(final_data, birth_month = birth_month, bootstrap = bootstrap, iter = iter, detail = detail))
}
