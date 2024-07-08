data_prep <- function(rel, reco, size_at_age, birth_month, iter,
                      min_harvest_rate, spawn, hatchery, river,
                      ocean_r, ocean_c, bootstrap, d_mort,
                      hr_c, hr_r, rel_mort, sex, u_bound) {


# Preprocess and Helper Function Definitions ------------------------------
  column_names <- c(
    "month", "fishery", "location", "brood_year", "age", "maturation_grp",
    "size_limit", "total_indiv", "mean", "sd", "catch", "harvest_rate", "rate"
  )

  MNTH_IDX = 1
  FSHRY_IDX = 2
  LOC_IDX = 3
  BY_IDX = 4
  AGE_IDX = 5
  MAT_GRP_IDX = 6
  SIZE_LIM_IDX = 7
  TOTAL_IDX = 8
  CATCH_IDX = 11
  REL_MORT_IDX = 13

  AGE_LAA_IDX = 1
  MNTH_LAA_IDX = 2
  MEAN_LAA_IDX = 3
  SD_LAA_IDX = 4

  num_rows = 0
  size_age_map = r2r::hashmap()

  create_size_age_map <- function(record) {
    record = unname(record)
    key = c(record[AGE_LAA_IDX], record[MNTH_LAA_IDX]) |> as.integer()
    value = c(record[MEAN_LAA_IDX], record[SD_LAA_IDX])

    size_age_map[[key]] <<- value
  }

  missing_size_age_handler <- function(month, age, size_age_map, size_age_df) {

    min_age_mnth_row = size_age_df |>
      arrange(month, age) |>
      head(1) |>
      unname()

    max_age_mnth_row = size_age_df |>
      arrange(month, age) |>
      tail(1) |>
      unname()

    if (is.na(month) || is.na(age)) {
      warning("NA detected in handling missing length at age.")
      return(min_age_mnth_row[c(3,4)] |> unlist())
    }

    cur_min_age = min_age_mnth_row[1] |> unlist()
    cur_min_month = min_age_mnth_row[2] |> unlist()
    cur_max_age = max_age_mnth_row[1] |> unlist()
    cur_max_month = max_age_mnth_row[2] |> unlist()

    if (age < cur_min_age | age == cur_min_age & month < cur_min_month) {
      return(min_age_mnth_row[3:4] |> unlist())
    } else if (age > cur_max_age | age == cur_max_age & month > cur_max_month) {
      return(max_age_mnth_row[3:4] |> unlist())
    }

    for (age in age : cur_min_age) {
      for (i in 1L: 12L) {
        month = ((month - 1L) %% 12L) + 1L
        if (!is.null(size_age_map[[c(age, month)]])) {
          return(size_age_map[[c(age, month)]] |> unlist())
        }
      }
    }

    for (age in age : cur_max_age) {
      for (i in 1L: 12L) {
        month = ((month + 1L) %% 12L) + 1L
        if (!is.null(size_age_map[[c(age, month)]])) {
          return(size_age_map[[c(age, month)]] |> unlist())
        }
      }
    }
  }

  find_mean_sd <- function(month, age) {
    if (month < birth_month) {
      age = age + 1L
    }

    mean_sd = size_age_map[[c(age, month)]]
    if (is.null(mean_sd)) {
      warning(paste("The specified month, age pair does not have any corresponding size at age data.", age, month, sep = "-"))
      return(missing_size_age_handler(month, age, size_age_map, length_at_age))
    }

    return(mean_sd)
  }

  find_catch <- function(mean, sd, size_limit, total_indiv) {
    return(total_indiv / (1 - pnorm(size_limit, mean = mean, sd = sd)))
  }

  find_catch_bootstrap <- function(mean, sd, size_limit, total_indiv) {
   return(pmax(mapply(find_catch, mean = mean, sd = sd, size_limit = size_limit, total_indiv), min_harvest_rate))
  }

  find_bt_sum <- function(est_num, prod_exp) {
    bt_sum_helper <- function(est_num, prod_exp) {
      return((1 + est_num) / prod_exp)
    }

    return(mapply(bt_sum_helper, est_num = est_num, prod_exp = prod_exp) |> rowSums())
  }

  find_num_rows <- function(rel_reco_dt, fisheries, month_fishery) {
    distinct_cols = c("brood_year", "age")

    if (month_fishery) {
      distinct_cols = c(distinct_cols, "month", "fishery")
    }

    rel_reco_dt[fishery %in% fisheries, .SD, .SDcols = distinct_cols][, unique(.SD)][, .N]
  }

  release_info = as.data.table(rel)[,
                                    .(total_release = sum(total_release / prod_exp)),
                                    by = list(brood_year)][order(brood_year)]

  apply(length_at_age, 1, create_size_age_map)

# Create Release and Recovery Joined Table --------------------------------
  rel_reco_dt <- merge(reco, rel, by = 'tag_code')

  if (nrow(rel_reco_dt) == 0 || ncol(rel_reco_dt) == 0) {
    stop("Joined release and recovery data table is empty."
    )
  }

  rel_reco_dt$age <- ifelse(rel_reco_dt$fishery %in% c(spawn, hatchery, river),
                             ifelse(rel_reco_dt$month > u_bound, # The month marking the upper bound for the current run.
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year + 1L,
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year),
                             ifelse(rel_reco_dt$month >= birth_month,
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year + 1L,
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year))
  rel_reco_dt$maturation_grp <- ifelse(rel_reco_dt$fishery %in% c(spawn, hatchery, river),
                                        1,
                                        2)

  setDT(rel_reco_dt, key = c("brood_year",
                             "month",
                             "age",
                             "location"))
  rel_reco_dt[is.na(est_num) | est_num < 1, value := 1]

  if (sex == "male") {
    rel_reco_dt = rel_reco_dt[sex == "M"]
  } else if (sex == "female") {
    rel_reco_dt = rel_reco_dt[sex == "F"]
  }

  rel_reco_dt[, c("month", "age") := .(as.integer(month), as.integer(age))]

  if (bootstrap) {
    num_rows = rel_reco_dt |> nrow()
    prob = pmin(rep(1 / rel_reco_dt[['est_num']], times = iter, each = 1), 1)
    rel_reco_dt = rel_reco_dt[, est_num := split(as.vector(vapply(prob, rnbinom, FUN.VALUE = 1, size = 1, n = 1)), 1 : num_rows)][,
                              {
                                total_indiv = find_bt_sum(est_num, prod_exp)
                                mean_sd = split(mapply(find_mean_sd, month = month, age = age), rep(1:2, times = length(month)))
                                mean = mean_sd[[1]]
                                sd = mean_sd[[2]]
                                catch = find_catch_bootstrap(mean, sd, size_limit, total_indiv)

                                .(total_indiv = .(total_indiv),
                                  mean = mean,
                                  sd = sd,
                                  catch = .(catch),
                                  harvest_rate = 1 - pnorm(size_limit, mean = mean, sd = sd))
                                },
                              by = list(brood_year, month, age, fishery, location, maturation_grp, size_limit, run_year)]

    rel_reco_dt[, month := (month - birth_month) %% 12]
    setorder(rel_reco_dt, brood_year, maturation_grp, age, month, fishery)
    rel_reco_dt[, month := (month + birth_month) %% 12]
    rel_reco_dt <- merge(rel_reco_dt, rel_mort, by = c("run_year", "month", "fishery", "location"), all.x = TRUE, sort = F, no.dups = F)
    rel_reco_dt[, rate := fifelse(is.na(rate) & fishery == ocean_r, hr_r,
                                  fifelse(is.na(rate) & fishery == ocean_c, hr_c, rate))]
    rel_reco_dt[, run_year := NULL]
  } else {
    rel_reco_dt <- rel_reco_dt[, .(total_indiv = sum(est_num / prod_exp)),
             by = .(brood_year, month, age, fishery, location, maturation_grp, size_limit, run_year)]
    rel_reco_dt[, c("mean", "sd") := {
      res <- split(mapply(find_mean_sd, month = month, age = age), rep(1:2, times = length(rel_reco_dt$month)))
      .(res[[1]], res[[2]])
    }]

    rel_reco_dt[, c("catch", "harvest_rate") := .(
      find_catch_bootstrap(mean, sd, size_limit, total_indiv),
      1 - pnorm(size_limit, mean = mean, sd = sd)
    )]
    rel_reco_dt[, month := (month - birth_month) %% 12]
    setorder(rel_reco_dt, brood_year, maturation_grp, age, month, fishery)
    rel_reco_dt[, month := (month + birth_month) %% 12]
    rel_reco_dt <- merge(rel_reco_dt, rel_mort, by = c("run_year", "month", "fishery", "location"), all.x = TRUE, sort = F, no.dups = F)
    rel_reco_dt[, rate := fifelse(is.na(rate) & fishery == ocean_r, hr_r,
                         fifelse(is.na(rate) & fishery == ocean_c, hr_c, rate))]
    rel_reco_dt[, run_year := NULL]
  }

  setcolorder(rel_reco_dt, column_names)
# Find Impact and Maturation ----------------------------------------------

  yr_ag_cnt = find_num_rows(rel_reco_dt, c(spawn, hatchery, river), FALSE)
  yr_ag_mth_fshry_cnt = find_num_rows(rel_reco_dt, c(ocean_r, ocean_c), TRUE)

  mat_init_vec = rep(0, yr_ag_cnt)
  mat_dt = data.table(by = mat_init_vec,
                        age = mat_init_vec,
                        maturation = mat_init_vec)

  imp_init_vec = rep(0, yr_ag_mth_fshry_cnt)
  imp_dt = data.table(by = imp_init_vec,
                         month = imp_init_vec,
                         age = imp_init_vec,
                         fishery = imp_init_vec,
                         impact = imp_init_vec)

  prev_hat_esc <- prev_sp_esc <- prev_riv_harv <- 0
  prev_m_year = rel_reco_dt[1, ..BY_IDX][[1]]
  prev_m_age = rel_reco_dt[1, ..AGE_IDX][[1]]
  row_m_idx = 1L

  prev_m_year_valid <- prev_i_year_valid <- FALSE

  prev_i_month = rel_reco_dt[1, ..MNTH_IDX][[1]]
  is_prev_ocean_r <- is_ocean_r <- FALSE
  prev_com_imp <- prev_rec_imp <- 0
  imp_col <- mat_col <- list()
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_fshry = NA
  row_i_idx = 1L

  find_imp_nat_mat <- function(record) {
    # browser()
    par_env = env_parent(current_env())
    if (bootstrap) {
      cur_yr = record[[BY_IDX]] |> as.integer()
      cur_ag = record[[AGE_IDX]] |> as.integer()
      cur_mnth = record[[MNTH_IDX]] |> as.integer()
      cur_fshry = record[[FSHRY_IDX]] |> as.numeric()
      cur_rel_mort = record[[REL_MORT_IDX]] |> as.numeric()
      cur_indiv = record[[TOTAL_IDX]][[1]]|> as.numeric()
      catch = record[[CATCH_IDX]][[1]] |> as.numeric()
    } else {
      cur_yr = record[BY_IDX] |> as.integer()
      cur_ag = record[AGE_IDX] |> as.integer()
      cur_mnth = record[MNTH_IDX] |> as.integer()
      cur_fshry = record[FSHRY_IDX] |> as.numeric()
      cur_rel_mort = record[REL_MORT_IDX] |> as.numeric()
      cur_indiv = record[TOTAL_IDX] |> as.numeric()
      catch = record[CATCH_IDX] |> as.numeric()
    }

    if (cur_fshry %in% c(spawn, river, hatchery)) {
      if ((cur_yr != prev_m_year && prev_m_year_valid) || (cur_yr == prev_m_year && cur_ag != prev_m_age)) {
        mat_col <<- append(par_env$mat_col, list(par_env$prev_sp_esc +
                 par_env$prev_riv_harv +
                 par_env$prev_hat_esc))
        par_env$mat_dt |>
          set(i = row_m_idx, j = "by", value = prev_m_year)
        par_env$mat_dt |>
          set(i = row_m_idx, j = "age", value = prev_m_age)

        row_m_idx <<- row_m_idx + 1L
        prev_sp_esc <<- prev_riv_harv <<- prev_hat_esc <<- 0
        prev_m_year_valid <<- FALSE
      }

      if (cur_fshry == spawn) {
        prev_sp_esc <<- prev_sp_esc + cur_indiv
      } else if (cur_fshry == river) {
        prev_riv_harv <<- prev_riv_harv + cur_indiv
      } else {
        prev_hat_esc <<- prev_hat_esc + cur_indiv
      }

      prev_m_year_valid <<- TRUE
      prev_m_year <<- cur_yr
      prev_m_age <<- cur_ag

    } else if (cur_fshry %in% c(ocean_r, ocean_c)) {
      if (prev_i_year_valid && (cur_ag != prev_i_age || cur_mnth != prev_i_month || cur_yr != prev_i_year || (prev_fshry != cur_fshry && !is.na(prev_fshry)))) {
        if (par_env$is_prev_ocean_r) {
          # is_ocean_r <<- TRUE
          imp_col <<- append(par_env$imp_col, list(prev_rec_imp))
          par_env$imp_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_imp <<- 0
        } else {
          # is_ocean_r <<- FALSE
          imp_col <<- append(par_env$imp_col, list(prev_com_imp))
          par_env$imp_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_c)
          prev_com_imp <<- 0
        }

        par_env$imp_dt |>
          set(i = row_i_idx, j = "by", value = prev_i_year)
        par_env$imp_dt |>
          set(i = row_i_idx, j = "age", value = prev_i_age)
        par_env$imp_dt |>
          set(i = row_i_idx, j = "month", value = prev_i_month)

        row_i_idx <<- row_i_idx + 1L
        prev_i_year_valid <<- FALSE
      }

      if (cur_fshry == ocean_r) {
        is_prev_ocean_r <<- TRUE
        is_ocean_r <<- TRUE
        prev_rec_imp <<- prev_rec_imp + cur_indiv + catch * d_mort + (catch - cur_indiv) * cur_rel_mort
      } else {
        is_prev_ocean_r <<- FALSE
        is_ocean_r <<- FALSE
        prev_com_imp <<- prev_com_imp + cur_indiv + catch * d_mort + (catch - cur_indiv) * cur_rel_mort
      }

      prev_i_year_valid <<- TRUE
      prev_i_year <<- cur_yr
      prev_i_age <<- cur_ag
      prev_i_month <<- cur_mnth
      prev_fshry <<- cur_fshry
    }
  }

  if (bootstrap) {
    for (i in 1 : num_rows) {
      find_imp_nat_mat(rel_reco_dt[i, ])
    }
  } else {
    apply(rel_reco_dt, 1, find_imp_nat_mat)
  }

  mat_col = append(mat_col, list(prev_sp_esc +
                                   prev_riv_harv +
                                   prev_hat_esc))
  mat_dt |>
    set(i = row_m_idx, j = "by", value = prev_m_year)
  mat_dt |>
    set(i = row_m_idx, j = "age", value = prev_m_age)

  if (is_ocean_r) {
    imp_col = append(imp_col, list(prev_rec_imp))
    tryCatch(imp_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_r),
      error = function(e) {
        if (sex != "both") message("An error occured because there is no ", sex, " ocean fishery data.")
        else message("An error occured in:\n", e)
      })
    prev_rec_imp = 0
  } else {
    imp_col = append(imp_col, list(prev_com_imp))
    tryCatch(imp_dt |>
               set(i = row_i_idx, j = "fishery", value = ocean_c),
             error = function(e) {
               if (sex != "both") message("An error occured because there is no ", sex, " ocean fishery data.")
               else message("An error occured in:\n", e)
             })
    prev_com_imp = 0
  }

  imp_dt |>
    set(i = row_i_idx, j = "by", value = prev_i_year)
  imp_dt |>
    set(i = row_i_idx, j = "age", value = prev_i_age)
  imp_dt |>
    set(i = row_i_idx, j = "month", value = prev_i_month)

  mat_dt[, 'maturation' := .(mat_col)]
  imp_dt[, 'impact' := .(imp_col)]

  max_age_month_dt = rel_reco_dt[, .(max_age = max(age), month = (birth_month - 2 %% 12 + 1)), by = list(brood_year)]

  tibble::view(imp_dt[, .(sum_impact = base::sum(as.numeric(unlist(impact)))), by = .(age, month, by)])
  tibble::view(mat_dt)
  rm(rel_reco_dt)
  return(list(maturation = mat_dt, impact = imp_dt, max_age_month_dt = max_age_month_dt, release_info = release_info))
}
