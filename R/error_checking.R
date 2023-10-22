### Error Checking Functions ###

# Check for the dimension for the input. Return empty list if dimensions match, and
# return the error information for each input. The error list is in the following
# format: NAME, actual COL_NUM, expected COL_NUM.
check_dim <- function(rel, reco) {
  # Recovery Data Frame Column Numbers.
  RECO_NUM_COL = 11

  #Release Data Frame Column Numbers.
  REL_NUM_COL = 5

  # Release Data Frame Column Names.
  REL_COL = c("release_month",
              "tag_code",
              "production_expansion_factor",
              "total_released",
              "brood_year")

  # Recovery Data Frame Column Names.
  RECO_COL = c("run_year",
               "brood_year",
               "recovery_id",
               "fisheries",
               "tag_code",
               "length_at_age",
               "sex",
               "month",
               "location",
               "size_limit",
               "estimated_num")

  error =  list()
  if (ncol(rel) != REL_NUM_COL) {
    error[["rel"]] = c("'release'", ncol(rel), REL_NUM_COL)
  }

  if (ncol(rec) != RECO_NUM_COL) {
    error[["rec"]] = c("'recoveries'", ncol(rec), RECO_NUM_COL)
  }

  return(error)
}

# Check for the type of the input. Upon unexpected data type, return a vector of
# incorrect argument name and expected data type.
check_type <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                       sex, fisheries, bootstrap, bootstrap_iter) {
  error = c()
  df = "data frame"
  ch = "character"
  lg = "logical"
  nm = "numeric"

  if (!is.data.frame(rel)) {
    error = c("'release'", df, typeof(rel))

  } else if (!is.data.frame(reco)) {
    error = c("'recovery'", df, typeof(reco))

  } else if (!is.data.frame(size_at_age)) {
    error = c("'size_at_age'", df, typeof(size_at_age))

  } else if (!is.data.frame(rel_mort)) {
    error = c("'release mortality'", df, typeof(rel_mort))

  } else if (!is.data.frame(nat_mort)) {
    error = c("'natural mortality'", df, typeof(nat_mort))

  } else if (!is.character(sex)) {
    error = c("'sex'", ch, typeof(sex))

  } else if (!is.data.frame(fisheries)) {
    error = c("'fisheries'", df, typeof(fisheries))

  } else if (!is.logical(bootstrap)) {
    error = c("'bootstrap'", lg, typeof(bootstrap))

  } else if (!is.numeric(bootstrap_iter)) {
    error = c("'boostrap_iter'", nm, typeof(bootstrap_iter))

  }

  return(error)
}

# Check for the length of the input.
check_length <- function() {}

# Check the uniqueness of the data frame.
check_unique <- function() {}

# Check the columns of the data frame.
check_columns <- function() {}

# Check for NAN in the data frame.
check_nan <- function() {}

