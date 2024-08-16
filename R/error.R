### Error Checking Functions ###
error_handler <- function(rel, reco, size_at_age, rel_mort, survival, sex,
                        fisheries, bootstrap, iter, last_month, birth_month,
                        d_mort, detail, min_harvest_rate,
                        level, hpd, verbose) {
  check_dim <- function(rel, reco, size_at_age, rel_mort, survival, fisheries) {
    REL_NUM_COL = length(exp_col()[[1]])
    RECO_NUM_COL = length(exp_col()[[2]])

    error =  list()
    if (ncol(rel) != REL_NUM_COL) {
      error[["rel"]] = c("'release'", REL_NUM_COL, ncol(rel))
    }
    if (ncol(reco) != RECO_NUM_COL) {
      error[["rec"]] = c("'recoveries'", RECO_NUM_COL, ncol(reco))
    }

    return(error)
  }

  check_type <- function(rel, reco, size_at_age, rel_mort, survival, sex,
                         fisheries, bootstrap, iter, last_month, birth_month,
                         d_mort, detail, min_harvest_rate,
                         level, hpd, verbose) {
    error = list()
    df = "'dataframe'"
    ch = "'character'"
    lg = "'logical'"
    nm = "'numeric'"
    int = "'integer'"
    db = "'double'"

    if (!is.data.frame(rel)) error[['rel']] = c("'release'", df, paste0("'", typeof(rel), "'"))
    if (!is.data.frame(reco)) error[['reco']] = c("'recovery'", df, paste0("'", typeof(reco), "'"))
    if (!is.data.frame(size_at_age)) error[['size']] = c("'size_at_age'", df, paste0("'", typeof(size_at_age), "'"))
    if (!is.data.frame(rel_mort)) error[['rel_mort']] = c("'rel_mort'", df, paste0("'", typeof(rel_mort), "'"))
    if (!is.data.frame(survival)) error[['survival']] = c("'survival'", df, paste0("'", typeof(survival), "'"))
    if (!is.double(d_mort)) error[['d_mort']] = c("'d_mort'", db, paste0("'", typeof(d_mort), "'"))
    # if (!is.double(hr_mort_com)) error[['hr_mort_com']] = c("'hr_mort_com'", db, paste0("'", typeof(hr_mort_com), "'"))
    # if (!is.double(hr_mort_rec)) error[['hr_mort_rec']] = c("'hr_mort_rec'", db, paste0("'", typeof(hr_mort_rec), "'"))
    if (!is.double(min_harvest_rate)) error[['min_harvest_rate']] = c("'min_harvest_rate'", db, paste0("'", typeof(min_harvest_rate), "'"))
    if (!is.double(level)) error[['level']] = c("'level'", db, paste0("'", typeof(level), "'"))
    if (!is.logical(bootstrap)) error[['bootstrap']] = c("'bootstrap'", lg, paste0("'", typeof(bootstrap), "'"))
    if (!is.logical(detail)) error[['detail']] = c("'detail'", lg, paste0("'", typeof(detail), "'"))
    if (!is.logical(hpd)) error[['hpd']] = c("'hpd'", lg, paste0("'", typeof(hpd), "'"))
    if (!is.logical(verbose)) error[['verbose']] = c("'verbose'", lg, paste0("'", typeof(verbose), "'"))
    if (!is.character(sex)) error[['sex']] = c("'sex'", ch, paste0("'", typeof(sex), "'"))
    if (!is.integer(iter)) error[['iter']] = c("'iter'", int, paste0("'", typeof(iter), "'"))
    if (!is.integer(last_month)) error[['last_month']] = c("'last_month'", int, paste0("'", typeof(last_month), "'"))
    if (!is.integer(birth_month)) error[['birth_month']] = c("'birth_month'", int, paste0("'", typeof(birth_month), "'"))
    if (!is.list(fisheries)) error[['fisheries']] = c("'fisheries'", df, paste0("'", typeof(fisheries), "'"))

    return(error)
  }

  check_length <- function(bootstrap, iter, last_month, birth_month,
                           d_mort, detail, min_harvest_rate,
                           level, hpd, verbose) {
    error = list()
    if (length(bootstrap) > 1) error[['bootstrap']] = c("'bootstrap'", 1, length(bootstrap))
    if (length(iter) > 1) error[['iter']] = c("'iter'", 1, length(iter))
    if (length(last_month) > 1) error[['last_month']] = c("'last_month'", 1, length(last_month))
    if (length(birth_month) > 1) error[['birth_month']] = c("'birth_month'", 1, length(birth_month))
    if (length(d_mort) > 1) error[['d_mort']] = c("'d_mort'", 1, length(d_mort))
    # if (length(hr_mort_com) > 1) error[['hr_mort_com']] = c("'hr_mort_com'", 1, length(hr_mort_com))
    # if (length(hr_mort_rec) > 1) error[['hr_mort_rec']] = c("'hr_mort_rec'", 1, length(hr_mort_rec))
    if (length(detail) > 1) error[['detail']] = c("'detail'", 1, length(detail))
    if (length(min_harvest_rate) > 1) error[['min_harvest_rate']] = c("'min_harvest_rate'", 1, length(min_harvest_rate))
    if (length(level) > 1) error[['level']] = c("'level'", 1, length(level))
    if (length(hpd) > 1) error[['hpd']] = c("'hpd'", 1, length(hpd))
    if (length(verbose) > 1) error[['verbose']] = c("'verbose'", 1, length(verbose))

    return(error)
  }

  check_unique <- function(rel, reco) {
    dist_rel <- rel |> dplyr::distinct(tag_code)
    w_msg <- ""

    msg1 <- "df has duplicates in column: "
    msg2 <- ".\n"

    if (length(rel[['tag_code']]) != length(dist_rel[['tag_code']])){
      w_msg = paste0(w_msg, "Release ", msg1, "tag_code", msg2)
    }

    if (nchar(w_msg) > 1) {
      warning(w_msg)
    }

    rel <- dist_rel
    return (list(rel))
  }

  check_columns <- function(rel, reco, length_at_age, release_mort, survival) {
    REL_COL = exp_col()[[1]]
    RECO_COL = exp_col()[[2]]
    LAA_COL = exp_col()[[3]]
    RM_COL = exp_col()[[4]]
    SV_COL = exp_col()[[5]]

    error = list()

    if (!(all(colnames(rel) == REL_COL))) {
      error[["rel"]] = c("'release'",
                         paste0("(", paste(REL_COL, collapse = ", "), ")"),
                         paste0("(", paste(colnames(rel), collapse = ", "), ")"))
    }

    if (!(all(colnames(reco) == RECO_COL))) {
      error[["reco"]] = c("'reco'",
                          paste0("(", paste(RECO_COL, collapse = ", "), ")"),
                          paste0("(", paste(colnames(reco), collapse = ", "), ")"))
    }

    if (!(all(colnames(length_at_age) == LAA_COL))) {
      error[["laa"]] = c("'length_at_age'",
                         paste0("(", paste(LAA_COL, collapse = ", "), ")"),
                         paste0("(", paste(colnames(length_at_age), collapse = ", "), ")"))
    }

    if (!(all(colnames(release_mort) == RM_COL))) {
      error[["rm"]] = c("'release_mort'",
                         paste0("(", paste(RM_COL, collapse = ", "), ")"),
                         paste0("(", paste(colnames(release_mort), collapse = ", "), ")"))
    }

    if (!(all(colnames(survival) == SV_COL))) {
      error[["sur"]] = c("'survival'",
                        paste0("(", paste(SV_COL, collapse = ", "), ")"),
                        paste0("(", paste(colnames(survival), collapse = ", "), ")"))
    }

    return(error)
  }

  check_nan <- function(rel, size_at_age, rel_mort, survival, fisheries) {
    input = list(rel, size_at_age, rel_mort, survival, fisheries)
    names(input) <- c('release', 'size_at_age', 'rel_mort',
                      'survival', 'fisheries')
    w_msg = ""

    for (i in names(input)) {
      if (sum(is.na(input[[i]])) > 0) {
        w_msg = paste0(w_msg, '`',i ,'`', ' contains NaN values')
      }
    }

    if (nchar(w_msg) > 1) {
      warning(w_msg)
    }
  }

  exp_col <- function() {
    REL_COL = c("release_month",
                "brood_year",
                "tag_code",
                "prod_exp",
                "total_release")
    RECO_COL = c("run_year",
                "fishery",
                "tag_code",
                "length",
                "sex",
                "month",
                "location",
                "size_limit",
                "est_num")

    SIZE_AT_AGE_COL = c("age",
                "month",
                "mean",
                "sd")

    RELEASE_MORT_COL = c("run_year",
                         "fishery",
                         "location",
                         "month",
                         "rate")

    SURVIVAL_COL = c("age",
                     "rate")

    return(list(REL_COL,
                RECO_COL,
                SIZE_AT_AGE_COL,
                RELEASE_MORT_COL,
                SURVIVAL_COL))
  }

  error_handler_helper <- function(err, type) {
    append_error <- function(elem) {
      msg <<- paste0(msg, "Error in ", elem[1], ", expected ",
                     elem[2], " ", type," but got ",
                     elem[3], " ", type, ".\n  ")
    }
    msg = ""
    lapply(err, append_error)
    stop(msg)
  }

  if (missing(rel) | missing(reco)) {
    stop(paste0("One or both of rel and reco are missing.
              Please supply input for rel and reco"))
  }

  typ_err = check_type(rel, reco, size_at_age, rel_mort, survival, sex,
                       fisheries, bootstrap, iter, last_month, birth_month,
                       d_mort, detail, min_harvest_rate,
                       level, hpd, verbose)
  if (length(typ_err) > 0) error_handler_helper(typ_err, "type")

  col_err = check_dim(rel, reco, size_at_age, rel_mort, survival, fisheries)
  if (length(col_err) > 0) error_handler_helper(col_err, "column(s)")

  nam_err = check_columns(rel, reco, size_at_age, rel_mort, survival)
  if (length(nam_err) > 0) error_handler_helper(nam_err, "columns")

  len_err = check_length(bootstrap, iter, last_month, birth_month,
                         d_mort, detail, min_harvest_rate,
                         level, hpd, verbose)
  if (length(len_err) > 0) error_handler_helper(len_err, "element(s)")

  if (birth_month < 1 || birth_month > 12) stop("`birth_month` is out of the acceptable range. Must be between 1 and 12.")

  if (last_month < 1 || last_month > 12) stop("`last_month` is out of the acceptable range. Must be between 1 and 12.")

  if (iter < 0) stop("`iter` is out of the acceptable range. Must be larger or equal to 0.")

  check_unique(rel, reco)
  check_nan(rel, size_at_age, rel_mort, survival, fisheries)
}
