### Error Checking Functions ###
error_handler <- function(rel, reco, size_at_age, rel_mort, nat_mort, sex,
                        fisheries, bootstrap, iter) {
  check_dim <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
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
    if (!is.list(fisheries)) error[['fisheries']] = c("'fisheries'", df, paste0("'", typeof(fisheries), "'"))
    if (!is.logical(bootstrap)) error[['bootstrap']] = c("'bootstrap'", lg, paste0("'", typeof(bootstrap), "'"))
    if (!is.numeric(bootstrap_iter)) error[['iter']] = c("'bootstrap_iter'", nm, paste0("'", typeof(bootstrap_iter), "'"))

    return(error)
  }

  check_unique <- function(rel, reco) {
    dist_rel <- rel |> distinct(tag_code)
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

  check_columns <- function(rel, reco) {
    REL_COL = exp_col()[[1]]
    RECO_COL = exp_col()[[2]]
    error = list()

    if (!(setequal(colnames(rel), REL_COL))) {
      error[["rel"]] = c("'release'",
                         paste0("(", paste(REL_COL, collapse = ", "), ")"),
                         paste0("(", paste(colnames(rel), collapse = ", "), ")"))
    }

    if (!(setequal(colnames(reco), RECO_COL))) {
      error[["reco"]] = c("'reco'",
                          paste0("(", paste(RECO_COL, collapse = ", "), ")"),
                          paste0("(", paste(colnames(reco), collapse = ", "), ")"))
    }

    return(error)
  }

  # TODO: update `check_nan`.
  check_nan <- function(rel, reco, size_at_age, rel_mort, nat_mort, fisheries) {
    input = list(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
    names(input) <- c('release', 'recovery', 'size_at_age',
                         'rel_mort', 'nat_mort', 'fisheries')
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
                "tag_code",
                "prod_exp",
                "total_release",
                "brood_year")
    RECO_COL = c("run_year",
                "fishery",
                "tag_code",
                "length",
                "sex",
                "month",
                "location",
                "size_limit",
                "est_num")

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

  error_handler <- function(err, type) {
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

  typ_err = check_type(rel, reco, size_at_age, rel_mort, nat_mort, sex, fisheries, bootstrap, iter)
  if (length(typ_err) > 0) error_handler(typ_err, "type")

  col_err = check_dim(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
  if (length(col_err) > 0) error_handler(col_err, "column(s)")

  nam_err = check_columns(rel, reco)
  if (length(nam_err) > 0) error_handler(nam_err, "columns")

  check_unique(rel, reco)
  # check_nan(rel, reco, size_at_age, rel_mort, nat_mort, fisheries)
}
