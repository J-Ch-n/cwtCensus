### Error Checking Functions ###

# Check for the dimension for the input. Return empty list if dimensions match, and
# return the error information for each input. The error list is in the following
# format: NAME, expected COL_NUM, actual COL_NUM.
check_dim <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
  # Recovery Data Frame Column Numbers.
  RECO_NUM_COL = 11

  #Release Data Frame Column Numbers.
  REL_NUM_COL = 5

  error =  list()
  if (ncol(rel) != REL_NUM_COL) {
    error[["rel"]] = c("'release'", REL_NUM_COL, ncol(rel))
  }

  if (ncol(reco) != RECO_NUM_COL) {
    error[["rec"]] = c("'recoveries'", RECO_NUM_COL, ncol(reco))
  }

  return(error)
}

# Check for the type of the input. Upon unexpected data type, return a list of
# incorrect argument name and expected data type.
# Each element of the list will be a vector of error arguments, where the first
# element is the error column name, the second element is the expected type, and
# third element is the actual type.
check_type <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                       sex, fisheries, bootstrap, bootstrap_iter) {
  error = list()
  df = "'dataframe'"
  ch = "'character'"
  lg = "'logical'"
  nm = "'numeric'"

  if (!is.data.frame(rel))
    error[['rel']] = c("'release'", df, paste0("'", typeof(rel), "'"))
  if (!is.data.frame(reco))
    error[['reco']] = c("'recovery'", df, paste0("'", typeof(reco), "'"))
  if (!is.data.frame(size_at_age))
    error[['size']] = c("'size_at_age'", df, paste0("'", typeof(size_at_age), "'"))
  if (!is.data.frame(rel_mort))
    error[['rel_mort']] = c("'rel_mort'", df, paste0("'", typeof(rel_mort), "'"))
  if (!is.data.frame(nat_mort))
    error[['nat_mort']] = c("'natural mortality'", df, paste0("'", typeof(nat_mort), "'"))
  if (!is.character(sex))
    error[['sex']] = c("'sex'", ch, paste0("'", typeof(sex), "'"))
  if (!is.data.frame(fisheries))
    error[['fisheries']] = c("'fisheries'", df, paste0("'", typeof(fisheries), "'"))
  if (!is.logical(bootstrap))
    error[['bootstrap']] = c("'bootstrap'", lg, paste0("'", typeof(bootstrap), "'"))
  if (!is.numeric(bootstrap_iter))
    error[['iter']] = c("'bootstrap_iter'", nm, paste0("'", typeof(bootstrap_iter), "'"))

  return(error)
}

# Check the uniqueness of the data frame.
# Will warn user about duplicates but remove them and not stop running
check_unique <- function(rel, reco) {
  dist_rel <- rel %>% distinct(tag_code)
  dist_reco <- reco %>% distinct(recovery_id)

  msg <- "df has duplicates in column: "

  if (length(rel[['tag_code']]) != length(dist_rel[['tag_code']])){
    warning(past0("Release ", msg, "tag_code"))
  }

  if (length(reco[['recovery_id']]) != length(dist_reco[['recovery_id']])){
    warning(paste0("Recovery ", msg, "recovery_id"))
  }

  rel <- dist_rel
  reco <- dist_reco
}

# Check the columns of the data frame.
check_columns <- function(rel, reco) {
  REL_COL = exp_col()[[1]]
  RECO_COL = exp_col()[[2]]
  error = list()

  if (!(setequal(colnames(rel), sort(REL_COL)))) {
    #alternatively we can go through each provided colname and see if it in is REL/RECO COL
    #and return list of unmatched input cols
    #We can treat each miss match as an error and put it into an error list.
    error[["rel"]] = c("'release'",
                       paste0("(", paste(REL_COL, collapse = ", "), ")"),
                       paste0("(", paste(colnames(rel), collapse = ", "), ")"))
    #warning("column labels do not match required labels")
  }

  if (!(setequal(colnames(reco), sort(RECO_COL)))) {
    error[["reco"]] = c("'reco'",
                        paste0("(", paste(RECO_COL, collapse = ", "), ")"),
                        paste0("(", paste(colnames(reco), collapse = ", "), ")"))
    #warning("column labels do not match required labels")
  }

  return(error)
}

# Check for NAN in the data frame.
check_nan <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
  input = c(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
  names(input) = c("release",
                   "recovery",
                   "size_at_age",
                   "rel_mort",
                   "nat_mort",
                   "fisheries")
  nan_helper <- function(input) {
   if (sum(is.na(input)) > 0) {
     warning(paste0(names(input), "contains NaN values"))
   }
  }

  lapply(input, nan_helper)
}

# Return a list of expected input columns for REL and RECO. The list contains two
# elements. The first contains the expected column names for REL and
# the second contains the expected column names for RECO.
exp_col <- function() {
  REL_COL = c("release_month",
              "birth_month",
              "tag_code",
              "production_expansion_factor",
              "total_released",
              "brood_year")
  RECO_COL = c("run_year",
               "brood_year",
               "recovery_id",
               "fishery",
               "tag_code",
               "length",
               "sex",
               "month",
               "location",
               "size_limit",
               "est_num",
               "length")
  return(list(REL_COL, RECO_COL))
}

### Error Checking Functions ###

# Check for the dimension for the input. Return empty list if dimensions match, and
# return the error information for each input. The error list is in the following
# format: NAME, expected COL_NUM, actual COL_NUM.
check_dim <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
  # Recovery Data Frame Column Numbers.
  RECO_NUM_COL = 11

  #Release Data Frame Column Numbers.
  REL_NUM_COL = 5

  error =  list()
  if (ncol(rel) != REL_NUM_COL) {
    error[["rel"]] = c("'release'", REL_NUM_COL, ncol(rel))
  }
  if (ncol(reco) != RECO_NUM_COL) {
    error[["rec"]] = c("'recoveries'", RECO_NUM_COL, ncol(reco))
  }


  return(error)
}

# Check for the type of the input. Upon unexpected data type, return a list of
# incorrect argument name and expected data type.
# Each element of the list will be a vector of error arguments, where the first
# element is the error column name, the second element is the expected type, and
# third element is the actual type.
check_type <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                       sex, fisheries, bootstrap, bootstrap_iter) {
  error = list()
  df = "'dataframe'"
  ch = "'character'"
  lg = "'logical'"
  nm = "'numeric'"

  if (!is.data.frame(rel)) error[['rel']] = c("'release'", df, paste0("'", typeof(rel), "'"))
  if (!is.data.frame(reco)) error[['reco']] = c("'recovery'", df, paste0("'", typeof(reco), "'"))
  if (!is.data.frame(size_at_age)) error[['size']] = c("'size_at_age'", df, paste0("'", typeof(size_at_age), "'"))
  if (!is.data.frame(rel_mort)) error[['rel_mort']] = c("'rel_mort'", df, paste0("'", typeof(rel_mort), "'"))
  if (!is.data.frame(nat_mort)) error[['nat_mort']] = c("'natural mortality'", df, paste0("'", typeof(nat_mort), "'"))
  if (!is.character(sex)) error[['sex']] = c("'sex'", ch, paste0("'", typeof(sex), "'"))
  if (!is.data.frame(fisheries)) error[['fisheries']] = c("'fisheries'", df, paste0("'", typeof(fisheries), "'"))
  if (!is.logical(bootstrap)) error[['bootstrap']] = c("'bootstrap'", lg, paste0("'", typeof(bootstrap), "'"))
  if (!is.numeric(bootstrap_iter)) error[['iter']] = c("'bootstrap_iter'", nm, paste0("'", typeof(bootstrap_iter), "'"))

  return(error)
}

# Check the uniqueness of the data frame.
# Will warn user about duplicates but remove them and not stop running
check_unique <- function(rel, reco) {
  dist_rel <- rel %>% distinct(tag_code)
  dist_reco <- reco %>% distinct(recovery_id)

  msg1 <- "df has duplicates in column: "
  msg2 <- "\n Dropping duplicates."

  if (length(rel[['tag_code']]) != length(dist_rel[['tag_code']])){
    warning(past0("Release ", msg1, "tag_code", msg2))
  }
  if (length(reco[['recovery_id']]) != length(dist_reco[['recovery_id']])){
    warning(paste0("Recovery ", msg1, "recovery_id", msg2))
  }

  # pass by value.
  rel <- dist_rel
  reco <- dist_reco
  # return REL, RECO to user.
  return (rel, reco)
}

# Check the columns of the data frame.
check_columns <- function(rel, reco) {
  REL_COL = exp_col()[[1]]
  RECO_COL = exp_col()[[2]]
  error = list()

  if (!(setequal(colnames(rel), sort(REL_COL)))) {
    #alternatively we can go through each provided colname and see if it in is REL/RECO COL
    #and return list of unmatched input cols
    #We can treat each miss match as an error and put it into an error list.
    error[["rel"]] = c("'release'",
                       paste0("(", paste(REL_COL, collapse = ", "), ")"),
                       paste0("(", paste(colnames(rel), collapse = ", "), ")"))
    #warning("column labels do not match required labels")
  }

  if (!(setequal(colnames(reco), sort(RECO_COL)))) {
    error[["reco"]] = c("'reco'",
                        paste0("(", paste(RECO_COL, collapse = ", "), ")"),
                        paste0("(", paste(colnames(reco), collapse = ", "), ")"))
    #warning("column labels do not match required labels")
  }

  return(error)
}

# Check for NAN in the data frame.
check_nan <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
  input = c(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
  names(input) <- c('release', 'recovery', 'size_at_age',
                       'rel_mort', 'nat_mort', 'fisheries')
  for (i in names(input)) {
    if (sum(is.na(input[i])) > 0) {
      warning(paste0('`',i ,'`', ' contains NaN values'))
    }
  }

}

# Return a list of expected input columns for REL and RECO. The list contains two
# elements. The first contains the expected column names for REL and
# the second contains the expected column names for RECO.
exp_col <- function() {
  REL_COL = c("release_month",
              "birth_month",
              "tag_code",
              "prod_exp",
              "total_released",
              "brood_year")
  RECO_COL = c("run_year",
              "brood_year",
              "recovery_id",
              "fishery",
              "tag_code",
              "length",
              "sex",
              "month",
              "location",
              "size_limit",
              "est_num",
              "length")

  SIZE_AT_AGE_COL = c(
              "size",
              "age",
              "month")

  RELEASE_MORT_COL = c("region",
                       "mortality")

  FISHERIES_COL = c("name_fishery",
                    "index_fishery")

  NAT_MORTALITY = c("type_fishery",
                    "mortality")

  return(list(REL_COL,
              RECO_COL,
              SIZE_AT_AGE_COL,
              RELEASE_MORT_COL,
              FISHERIES_COL,
              NAT_MORTALITY))
}

