### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, size_at_age = stored_df,
                    rel_mort = stored_df, nat_mort = stored_df, sex = "both",
                    fisheries = stored_df, bootstrap = True, bootstrap_iter = 1000) {
  # Check missing inputs
  if (missing(rel) | missing(reco)) {
    stop(paste0("One or both of rel and reco are missing.
                Please supply input for rel and reco"))
  }
  # Check incorrect input data type
  if (check_type(rel, reco, size_at_age, rel_mort, nat_mort,
             sex, fisheries, bootstrap, bootstrap_iter)) {
    stop(paste0("Wrong type"))
  }
  # Check incorrect dimensions for rel, reco, size_at_age, rel_mort, nat_mort, sex, fisheries.
  # Check uniqueness of rel and reco
  # Check for missing values in all inputs
}
