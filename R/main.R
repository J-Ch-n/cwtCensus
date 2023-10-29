### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, size_at_age = release, rel_mort = release, nat_mort = release,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000) {

# Handles list of errors in ERR of check type TYPE. Aggregate error messages and
# stops with the composite error message at the end.
error_handler <- function(err, type) {

  # Appends each error message ELEM to the previous message.
  # Modifies the parent variable MSG.
  append_error <- function(elem) {
    msg <<- paste0(msg, "Error in ", elem[1], ", expected ",
                   elem[2], " ", type," but got ",
                   elem[3], " ", type, ".\n  ")
  }
  msg = ""
  lapply(err, append_error)
  stop(msg)
}

# Check missing required inputs REL and RECO.
if (missing(rel) | missing(reco)) {
  stop(paste0("One or both of rel and reco are missing.
              Please supply input for rel and reco"))
}

# Check incorrect input data type.
typ_err = check_type(rel, reco, size_at_age, rel_mort, nat_mort, sex, fisheries, bootstrap, iter)
if (length(typ_err) > 0) error_handler(typ_err, "type")

# Check incorrect number of columns.
col_err = check_dim(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
if (length(col_err) > 0) error_handler(col_err, "column(s)")

# Check incorrec† column names of REL and RECO.
nam_err = check_columns(rel, reco)
if (length(nam_err) > 0) error_handler(nam_err, "columns")

# Check uniqueness of REL and RECO
u_col = check_unique(rel, reco)
rel = u_col[[1]]
reco = u_col[[2]]

# Check for missing values in all inputs
check_nan(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)

}


