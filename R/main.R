### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, size_at_age = rec,
                    rel_mort = rec, nat_mort = rec, sex = "both",
                    fisheries = rec, bootstrap = TRUE, bootstrap_iter = 1000) {
  # Check missing inputs
  if (missing(rel) | missing(reco)) {
    stop(paste0("One or both of rel and reco are missing.
                Please supply input for rel and reco"))
  }
  # Check incorrect input data type.
  type_error = check_type(rel, reco, size_at_age, rel_mort, nat_mort, sex,
                          fisheries, bootstrap, bootstrap_iter)
  if (!is_empty(type_error)) {
    stop(paste0("Wrong type in ", type_error[1], ". Expected ", type_error[2],
                " but got ", "'", type_error[3], "'."))
  }
  # Check incorrect column dimension.
  col_error = check_dim(rel, reco)
  if (length(col_error) != 0) {
    msg = ""
    append_error_col <- function(elem) {
      msg <<- paste0(msg, "Error in ", elem[1], ", expected ",
                   elem[3], " columns, but got ",
                   elem[2], " columns.\n  ")
    }
    lapply(col_error, append_error_col);
    stop(msg)
  }
  # Check incorrect dimensions for rel, reco, size_at_age, rel_mort, nat_mort, sex, fisheries.

  # Check uniqueness of rel and reco
  # Check for missing values in all inputs
}


