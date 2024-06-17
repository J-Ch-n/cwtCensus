data_prep <- function(rel, reco, size_at_age = length_at_age, birth_month,
                      iter, min_harvest_rate, spawn = 54, hatchery = 50,
                      river = 46, ocean_r = 40, ocean_c = 10, bootstrap = T,
                      d_mort = 0.05, hr_c = 0.26, hr_r = 0.14,
                      rel_mort = release_mort, sex = "both") {


# Preprocess and Helper Function Definitions ------------------------------

  BY_IDX = 1
  MNTH_IDX = 2
  AGE_IDX = 3
  FSHRY_IDX = 4
  LOC_IDX = 5
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
    key = c(record[AGE_LAA_IDX], record[MNTH_LAA_IDX])
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
      for (i in 1: 12) {
        month = ((month - 1) %% 12) + 1
        if (!is.null(size_age_map[[c(month, age)]])) {
          return(size_age_map[[c(month, age)]] |> unlist())
        }
      }
    }

    for (age in age : cur_max_age) {
      for (i in 1: 12) {
        month = ((month + 1) %% 12) + 1
        if (!is.null(size_age_map[[c(month, age)]])) {
          return(size_age_map[[c(month, age)]] |> unlist())
        }
      }
    }
  }

  find_mean_sd <- function(month, age, size_age_map) {
    if (month < birth_month) {
      age = age + 1
    }
    mean_sd = size_age_map[[c(age, month)]]
    if (is.null(mean_sd)) {
      warning("The specified month, age pair does not have any corresponding size at age data.")
      return(missing_size_age_handler(month, age, size_age_map, length_at_age))
    }

    return(mean_sd)
  }

  find_catch <- function(mean, sd, size_limit, total_indiv) {
    return(total_indiv / max(c((1 - pnorm(size_limit, mean = mean, sd = sd)), min_harvest_rate)))
  }

  find_catch_bootstrap <- function(mean, sd, size_limit, total_indiv) {
   return(mapply(find_catch, mean = mean, sd = sd, size_limit = size_limit, total_indiv))
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

  rel_reco_dt <- merge(reco, rel, by = 'tag_code', all.x = TRUE)
  rel_reco_dt$age <- ifelse(rel_reco_dt$fishery %in% c(spawn, hatchery, river),
                             ifelse(rel_reco_dt$month >= birth_month,
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year,
                                    rel_reco_dt$run_year - rel_reco_dt$brood_year - 1L),
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
  if (sex == "male") {
    rel_reco_dt = rel_reco_dt[sex == "M"]
  } else if (sex == "female") {
    rel_reco_dt = rel_reco_dt[sex == "F"]
  }

  if (bootstrap) {
    num_rows = rel_reco_dt |> nrow()
    prob = rep(1 / rel_reco_dt[['est_num']], times = iter, each = 1)
    rel_reco_dt = rel_reco_dt[, est_num := split(as.vector(vapply(prob, rnbinom, FUN.VALUE = 1, size = 1, n = 1)), 1 : num_rows)][,
                              {
                                total_indiv = find_bt_sum(est_num, prod_exp)
                                mean_sd = find_mean_sd(month, age, size_age_map)
                                mean = mean_sd[1]
                                sd = mean_sd[2]
                                catch = find_catch_bootstrap(mean, sd, size_limit, total_indiv)

                                .(total_indiv = .(find_bt_sum(est_num, prod_exp)),
                                  mean = mean,
                                  sd = sd,
                                  catch = .(find_catch_bootstrap(mean, sd, size_limit, total_indiv)),
                                  harvest_rate = 1 - pnorm(size_limit, mean = mean, sd = sd))
                                },
                              by = list(brood_year, month, age, fishery, location, maturation_grp, size_limit, run_year)]

    rel_reco_dt = rel_reco_dt |>
      lazy_dt(immutable = F) |>
      mutate(month = (month - birth_month) %% 12) |>
      arrange(brood_year, maturation_grp, age, month, fishery) |>
      mutate(month = (month + birth_month) %% 12) |>
      as.data.table(rel_reco_dt) |>
      left_join(rel_mort, by = c("run_year",
                                          "month",
                                          "fishery",
                                          "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-run_year)
  } else {
    rel_reco_dt = rel_reco_dt |>
      lazy_dt(immutable = F, key_by = c("brood_year", "month", "fishery", "location", "size_limit")) |>
      group_by(brood_year, month, age, fishery, location, maturation_grp, size_limit, run_year) |>
      summarize(total_indiv = sum(est_num / prod_exp)) |>
      mutate(mean = find_mean_sd(month, age, size_age_map)[1],
             sd = find_mean_sd(month, age, size_age_map)[2],
             catch = find_catch(mean, sd, size_limit, total_indiv),
             harvest_rate = 1 - pnorm(size_limit, mean = mean, sd = sd)) |>
      mutate(month = (month - birth_month) %% 12) |>
      arrange(brood_year, maturation_grp, age, month, fishery) |>
      mutate(month = (month + birth_month) %% 12)

    rel_reco_dt = as.data.table(rel_reco_dt) |>
      left_join(rel_mort, by = c("run_year",
                                  "month",
                                  "fishery",
                                  "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-c(run_year))
  }

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
  prev_m_year = rel_reco_dt[1, ..BY_IDX] |> unlist()
  prev_m_age = rel_reco_dt[1, ..AGE_IDX] |> unlist()
  row_m_idx = 1L

  prev_m_year_valid <- prev_i_year_valid <- FALSE

  prev_i_month = rel_reco_dt[1, ..MNTH_IDX] |> unlist()
  is_prev_ocean_r <- is_ocean_r <- FALSE
  prev_com_imp <- prev_rec_imp <- 0
  imp_col <- mat_col <- list()
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_fshry = NA
  row_i_idx = 1L

  find_imp_nat_mat <- function(record) {
    par_env = env_parent(current_env())
    if (bootstrap) {
      cur_yr = record[[BY_IDX]] |> as.integer()
      cur_ag = record[[AGE_IDX]] |> as.integer()
      cur_mnth = record[[MNTH_IDX]] |> as.integer()
      cur_fshry = record[[FSHRY_IDX]] |> as.numeric()
      cur_rel_mort = record[[REL_MORT_IDX]] |> as.numeric()
      cur_indiv = record[[TOTAL_IDX]] |> unlist() |> as.numeric()
      catch = record[[CATCH_IDX]] |>  unlist() |> as.numeric()
    } else {
      cur_yr = record[BY_IDX] |> as.integer()
      cur_ag = record[AGE_IDX] |> as.integer()
      cur_mnth = record[MNTH_IDX] |> as.integer()
      cur_fshry = record[FSHRY_IDX] |> as.numeric()
      cur_rel_mort = record[REL_MORT_IDX] |> as.numeric()
      cur_indiv = record[TOTAL_IDX] |> as.numeric()
      catch = record[CATCH_IDX] |>  as.numeric()
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
          is_ocean_r <<- TRUE
          imp_col <<- append(par_env$imp_col, list(prev_rec_imp))
          par_env$imp_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_imp <<- 0
        } else {
          is_ocean_r <<- FALSE
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
        prev_rec_imp <<- prev_rec_imp + cur_indiv + catch * d_mort + (catch - cur_indiv) * cur_rel_mort
      } else {
        is_prev_ocean_r <<- FALSE
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

  max_age_month_df = rel_reco_dt |>
    group_by(brood_year) |>
    summarize(max_age = max(age), month = (birth_month - 2) %% 12 + 1)

  rm(rel_reco_dt)
  return(list(maturation = mat_dt, impact = imp_dt, max_age_month_df = max_age_month_df, release_info = release_info))
}
