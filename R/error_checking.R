### Error Checking Functions ###

# Check for the dimension for the input. Return empty list if dimensions match, and
# return the error information for each input. The error list is in the following
# format: NAME, expected COL_NUM, actual COL_NUM.
check_dim <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
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
    error[["rel"]] = c("'release'", REL_NUM_COL, ncol(rel))
  }

  if (ncol(rec) != RECO_NUM_COL) {
    error[["rec"]] = c("'recoveries'", RECO_NUM_COL, ncol(rec))
  }

  return(error)
}

# Check for the type of the input. Upon unexpected data type, return a vector of
# incorrect argument name and expected data type.
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

# Check for the length of the input.
check_length <- function() {}

# Check the uniqueness of the data frame.
# Will warn user about duplicates but remove them and not stop running
check_unique <- function(rel, reco) {
  dist_rel <- rel %>% distinct(tag_code)
  dist_reco <- reco %>% distinct(recovery_id)
  if (length(rel[['tag_code']) != length(dist_rel[['tag_code']])){
    warning("Release df has duplicate tag codes")
  }
  if (length(reco[['recovery_id']) != length(dist_reco[['recovery_id']])){
    warning("Release df has duplicate tag codes")
  }
  rel <- dist_rel
  reco <- dist_reco
}

# Check the columns of the data frame.
check_columns <- function(rel, reco) {
  REL_COL = c("release_month",
              "tag_code",
              "production_expansion_factor",
              "total_released",
              "brood_year")
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
  if (!(setequal(sort(unlist(colnames(rel))), sort(REL_COL)))) {
    #alternatively we can go through each provided colname and see if it in is REL/RECO COL 
    #and return list of unmatched input cols
    warning("column labels do not match required labels")
  }
}

# Check for NAN in the data frame.
check_nan <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
  input = c(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
  for (i in input) {
    if (sum(is.na(i)) > 0) {
      warning(i+"contains NaN values")
      # I know this syntax is not correct
    }
  }
}

