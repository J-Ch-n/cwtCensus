### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, size_at_age = rec,
                    rel_mort = rec, nat_mort = rec, sex = "both",
                    fisheries = rec, bootstrap = TRUE, bootstrap_iter = 1000) {
  # Appends each error message to MSG.
  append_error <- function(elem) {
    msg <<- paste0(msg, "Error in ", elem[1], ", expected ",
                   elem[2], " ", type," but got ",
                   elem[3], " ", type, ".\n  ")
  }
  # Check missing inputs
  if (missing(rel) | missing(reco)) {
    stop(paste0("One or both of rel and reco are missing.
                Please supply input for rel and reco"))
  }
  # Check incorrect input data type.
  type_error = check_type(rel, reco, size_at_age, rel_mort, nat_mort, sex,
                          fisheries, bootstrap, bootstrap_iter)

  if (!is_empty(type_error)) {
    msg = ""
    type = "type"
    lapply(type_error, append_error)
    stop(msg)
  }

  # Check incorrect column dimension.
  col_error = check_dim(rel, reco)

  if (length(col_error) != 0) {
    msg = ""
    type = "columns"
    lapply(col_error, append_error);
    stop(msg)
  }
  # Check incorrect dimensions for rel, reco, size_at_age, rel_mort, nat_mort, sex, fisheries.

  # Check uniqueness of rel and reco
  # Check for missing values in all inputs
}


