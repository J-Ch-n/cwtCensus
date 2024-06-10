#################################
### Data Preparation Function ###
#################################

# TODO: think about if the arguments make sense.
data_prep <- function(rel, reco, size_at_age = length_at_age,
                      birth_month, iter, min_harvest_rate,
                      rel_mort = NA, sex = "both", #TODO: implement cohort reconstruction based on sex.
                      spawn = 54, hatchery = 50, river = 46,
                      ocean_r = 40, ocean_c = 10, bootstrap = T,
                      d_mort = 0.05, hr_c = 0.26, hr_r = 0.14,
                      release_mortality = release_mort) {

  # TODO: REMOVE THIS LINE.
  set.seed(10)
  #####################################################################
  ### Step 1: Declare and define necessary functions and variables. ###
  #####################################################################

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

  ### Create size_age_map ###
  size_age_map = r2r::hashmap()

  # Create a hashmap from length_at_age. The resulting hashmap SIZE_AGE_MAP has c(month, age) as keys.
  # Each key corresponds to a value of c(mean, standard deviation).
  create_size_age_map <- function(record) {
    record = unname(record)
    key = c(record[AGE_LAA_IDX], record[MNTH_LAA_IDX])
    value = c(record[MEAN_LAA_IDX], record[SD_LAA_IDX])

    size_age_map[[key]] <<- value
  }

  # Create the SIZE_AGE_MAP.
  apply(length_at_age, 1, create_size_age_map)

  # If MONTH_FISHERY is true, find the number of unique year - age - month tuples for a specific fishery in PROVIDED_FISHERIES.
  # If MONTH_FISHERY is false, find the number of unique year - age pairs for a specific fishery in PROVIDED_FISHERIES.
  # PROVIDED_FISHERIES is passed in as a vector of fisheries.
  num_uniq_rows <- function(rel_reco_dt, fisheries, month_fishery) {
    distinct_cols = c("brood_year", "age")

    if (month_fishery) {
      distinct_cols = c(distinct_cols, "month", "fishery")
    }

    # return(rel_reco_dt |>
    #   filter(fishery %in% fisheries) |>
    #   select(distinct_cols) |>
    #   distinct() |>
    #   nrow())
    rel_reco_dt[fishery %in% fisheries, .SD, .SDcols = distinct_cols][, unique(.SD)][, .N]
  }

  # TODO: debug this section + think about if this is necessary.
  # Handle the case where c(MONTH, AGE) is not present in SIZE_AGE_DF. Iterate from the current time
  # to earlier time until we find a valid key. If this approach doesn't yield any valid key, iterate from the current time
  # to later time until we find a valid key.
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

  # Find the mean and standard deviation of body length at a certain age.
  find_mean_sd <- function(month, age, size_age_map) {
    # TODO: ask about this hacky fix.
    ####################################################
    ### Changed age to age + 1. This is a hacky fix. ###
    ####################################################
    mean_sd = size_age_map[[c(age + 1, month)]]
    if (is.null(mean_sd)) {
      warning("The specified month, age pair dooes not have any corresponding size at age data. NA is applied to the percent harvestable.")
      return(missing_size_age_handler(month, age, size_age_map, length_at_age))
    }

    return(mean_sd)
  }

  # Find the estimated number of catch.
  find_catch <- function(mean, sd, size_limit, total_indiv) {
    return(total_indiv / max(c((1 - pnorm(size_limit, mean =  mean, sd = sd)), min_harvest_rate)))
  }

  # Find the bootstrapped estimated number of catch.
  find_catch_bootstrap <- function(mean, sd, size_limit, total_indiv) {
   return(mapply(find_catch, mean = mean, sd = sd, size_limit = size_limit, total_indiv))
  }

  # Find the total number of individuals with bootstrapped values.
  bt_sum <- function(est_num, prod_exp) {
    bt_sum_helper <- function(est_num, prod_exp) {
      return((1 + est_num) / prod_exp)
    }

    return(mapply(bt_sum_helper, est_num = est_num, prod_exp = prod_exp) |> rowSums())
  }

  release_info = as.data.table(rel)[,
                                    .(total_release = sum(total_release / prod_exp)),
                                    by = list(brood_year)][order(brood_year)]

  ########################################
  ### Step 2: Construct Intermediate 1 ###
  ########################################
  if (bootstrap) {
    rel_reco_dt = reco |>
      left_join(rel, by = 'tag_code') |>
      setDT() |>
      lazy_dt(immutable = F, key_by = c("brood_year", "month", "fishery", "location", "size_limit")) |>
      mutate(
        age = case_when(
          month >= birth_month ~ run_year - brood_year,
          TRUE ~ run_year - brood_year - 1),
        maturation_grp = case_when(
          fishery %in% c(spawn, hatchery, river) ~ 1,
          TRUE ~ 2)) |>
      as.data.table()

    num_rows = rel_reco_dt |> nrow()
    prob = rep(1 / rel_reco_dt[['est_num']], times = iter, each = 1)
    rel_reco_dt[, est_num := split(as.vector(vapply(prob, rnbinom, FUN.VALUE = 1, size = 1, n = 1)), 1 : num_rows)]
    rel_reco_dt = rel_reco_dt[,
                              {
                                total_indiv = bt_sum(est_num, prod_exp)
                                mean_sd = find_mean_sd(month, age, size_age_map)
                                mean = mean_sd[1]
                                sd = mean_sd[2]
                                catch = find_catch_bootstrap(mean, sd, size_limit, total_indiv)

                                .(total_indiv = .(bt_sum(est_num, prod_exp)),
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
      left_join(release_mortality, by = c("run_year",
                                          "month",
                                          "fishery",
                                          "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-run_year)
  } else {
    rel_reco_ldt = reco |>
      left_join(rel, by = 'tag_code') |>
      setDT() |>
      lazy_dt(immutable = F, key_by = c("brood_year", "month", "fishery", "location", "size_limit")) |>
      mutate(
        age = case_when(
          month >= birth_month ~ run_year - brood_year,
          TRUE ~ run_year - brood_year - 1),
        maturation_grp = case_when(
          fishery %in% c(spawn, hatchery, river) ~ 1,
          TRUE ~ 2)) |>
      group_by(brood_year, month, age, fishery, location, maturation_grp, size_limit, run_year) |>
      summarize(total_indiv = sum(est_num / prod_exp)) |>
      mutate(mean = find_mean_sd(month, age, size_age_map)[1],
             sd = find_mean_sd(month, age, size_age_map)[2],
             catch = find_catch(mean, sd, size_limit, total_indiv),
             harvest_rate = 1 - pnorm(size_limit, mean = mean, sd = sd)) |>
      mutate(month = (month - birth_month) %% 12) |>
      arrange(brood_year, maturation_grp, age, month, fishery) |>
      mutate(month = (month + birth_month) %% 12)

    # Materialize lazy data table.
    rel_reco_dt = as.data.table(rel_reco_ldt) |>
      left_join(release_mortality, by = c("run_year",
                                          "month",
                                          "fishery",
                                          "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-run_year)
  }

  ##############################################
  ### Step 3: Calcuate Maturation and Impact ###
  ##############################################

  yr_ag_cnt = num_uniq_rows(rel_reco_dt, c(spawn, hatchery, river), FALSE)
  yr_ag_mth_fshry_cnt = num_uniq_rows(rel_reco_dt, c(ocean_r, ocean_c), TRUE)

  maturation_init_vec = rep(0, yr_ag_cnt)
  maturation_dt = data.table(by = maturation_init_vec,
                        age = maturation_init_vec,
                        maturation = maturation_init_vec)

  impact_init_vec = rep(0, yr_ag_mth_fshry_cnt)
  impact_dt = data.table(by = impact_init_vec,
                         month = impact_init_vec,
                         age = impact_init_vec,
                         fishery = impact_init_vec,
                         impact = impact_init_vec)

  ### Variables for maturation calculation. ###
  prev_hat_esc <- prev_sp_esc <- prev_riv_harv <- 0
  prev_m_year = rel_reco_dt[1, ..BY_IDX] |> unlist()
  prev_m_age = rel_reco_dt[1, ..AGE_IDX] |> unlist()
  row_m_idx = 1L

  prev_m_year_valid <- prev_i_year_valid <- FALSE

  ### Variables for impact calculation. ###
  prev_com_impact <- prev_rec_impact <- 0
  prev_fishery = NA
  is_prev_ocean_r <- is_ocean_r <- FALSE
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_i_month = rel_reco_dt[1, ..MNTH_IDX] |> unlist()
  row_i_idx = 1L
  impact_column <- maturation_column <- list()

  # Aggregate function for calculating maturation and impact.
  find_imp_nat_mat <- function(record) {
    par_env = env_parent(current_env())
    if (bootstrap) {
      cur_year = record[[BY_IDX]] |> as.integer()
      cur_age = record[[AGE_IDX]] |> as.integer()
      cur_month = record[[MNTH_IDX]] |> as.integer()
      cur_fishery = record[[FSHRY_IDX]] |> as.numeric()
      cur_rel_mort_rate = record[[REL_MORT_IDX]] |> as.numeric()
      cur_indiv = record[[TOTAL_IDX]] |> unlist() |> as.numeric()
      catch = record[[CATCH_IDX]] |>  unlist() |> as.numeric()
    } else {
      cur_year = record[BY_IDX] |> as.integer()
      cur_age = record[AGE_IDX] |> as.integer()
      cur_month = record[MNTH_IDX] |> as.integer()
      cur_fishery = record[FSHRY_IDX] |> as.numeric()
      cur_rel_mort_rate = record[REL_MORT_IDX] |> as.numeric()
      cur_indiv = record[TOTAL_IDX] |> as.numeric()
      catch = record[CATCH_IDX] |>  as.numeric()
    }

    if (cur_fishery %in% c(spawn, river, hatchery)) {
      if (((cur_year != prev_m_year && prev_m_year_valid)
           | (cur_year == prev_m_year && cur_age != prev_m_age))) {
        maturation_column <<- append(par_env$maturation_column, list(par_env$prev_sp_esc +
                 par_env$prev_riv_harv +
                 par_env$prev_hat_esc))
        par_env$maturation_dt |>
          set(i = row_m_idx, j = "by", value = prev_m_year)
        par_env$maturation_dt |>
          set(i = row_m_idx, j = "age", value = prev_m_age)

        row_m_idx <<- row_m_idx + 1L
        prev_sp_esc <<- prev_riv_harv <<- prev_hat_esc <<- 0
        prev_m_year_valid <<- FALSE
      }

      if (cur_fishery == spawn) {
        prev_sp_esc <<- prev_sp_esc + cur_indiv
      } else if (cur_fishery == river) {
        prev_riv_harv <<- prev_riv_harv + cur_indiv
      } else {
        prev_hat_esc <<- prev_hat_esc + cur_indiv
      }

      prev_m_year_valid <<- TRUE
      prev_m_year <<- cur_year
      prev_m_age <<- cur_age

    } else if (cur_fishery %in% c(ocean_r, ocean_c)) {
      # If fishery is recreational or commercial ocean harvest, calculate impact.
      if (prev_i_year_valid && (cur_age != prev_i_age | cur_month != prev_i_month | cur_year != prev_i_year | (prev_fishery != cur_fishery && !is.na(prev_fishery)))) {
        if (par_env$is_prev_ocean_r) {
          is_ocean_r <<- TRUE
          impact_column <<- append(par_env$impact_column, list(prev_rec_impact))
          par_env$impact_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_impact <<- 0
        } else {
          is_ocean_r <<- FALSE
          impact_column <<- append(par_env$impact_column, list(prev_com_impact))
          par_env$impact_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_c)
          prev_com_impact <<- 0
        }

        par_env$impact_dt |>
          set(i = row_i_idx, j = "by", value = prev_i_year)
        par_env$impact_dt |>
          set(i = row_i_idx, j = "age", value = prev_i_age)
        par_env$impact_dt |>
          set(i = row_i_idx, j = "month", value = prev_i_month)

        row_i_idx <<- row_i_idx + 1L
        prev_i_year_valid <<- FALSE
      }

      # Calculate number of individuals for each type of fishery.
      if (cur_fishery == ocean_r) {
        is_prev_ocean_r <<- TRUE
        prev_rec_impact <<- prev_rec_impact + cur_indiv + catch * d_mort + (catch - cur_indiv) * cur_rel_mort_rate
      } else {
        is_prev_ocean_r <<- FALSE
        prev_com_impact <<- prev_com_impact + cur_indiv + catch * d_mort + (catch - cur_indiv) * cur_rel_mort_rate
      }

      prev_i_year_valid <<- TRUE
      prev_i_year <<- cur_year
      prev_i_age <<- cur_age
      prev_i_month <<- cur_month
      prev_fishery <<- cur_fishery
    }
  }

  # TODO: Think about possible ways to remove this for loop.
  if (bootstrap) {
    for (i in 1 : num_rows) {
      find_imp_nat_mat(rel_reco_dt[i, ])
    }
  } else {
    apply(rel_reco_dt, 1, find_imp_nat_mat)
  }

  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of maturation here.
  maturation_column = append(maturation_column, list(prev_sp_esc +
                                                     prev_riv_harv +
                                                     prev_hat_esc))
  maturation_dt |>
    set(i = row_m_idx, j = "by", value = prev_m_year)
  maturation_dt |>
    set(i = row_m_idx, j = "age", value = prev_m_age)

  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of impact here.
  if (is_ocean_r) {
    impact_column = append(impact_column, list(prev_rec_impact))
    impact_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_r)
    prev_rec_impact = 0
  } else {
    impact_column = append(impact_column, list(prev_com_impact))
    impact_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_c)
    prev_com_impact = 0
  }

  impact_dt |>
    set(i = row_i_idx, j = "by", value = prev_i_year)
  impact_dt |>
    set(i = row_i_idx, j = "age", value = prev_i_age)
  impact_dt |>
    set(i = row_i_idx, j = "month", value = prev_i_month)

  maturation_dt[, 'maturation' := .(maturation_column)]
  impact_dt[, 'impact' := .(impact_column)]

  max_age_month_df = rel_reco_dt |>
    # This a hacky fix for the age offset. NEED TO CHANGE.
    mutate(age = case_when(
      fishery %in% c(ocean_r, ocean_c) ~ age + 1,
      TRUE ~ age
    )) |>
    group_by(brood_year) |>
    summarize(max_age = max(age), month = (birth_month - 2) %% 12 + 1)

  rm(rel_reco_dt)
  return(list(maturation = maturation_dt, impact = impact_dt, max_age_month_df = max_age_month_df, release_info = release_info))
}


