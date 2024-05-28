#################################
### Data Preparation Function ###
#################################

data_prep <- function(rel, reco, size_at_age = length_at_age, birth_month, rel_mort = NA,
                      sex = "both", spawn = 54, hatchery = 50, river = 46,
                      ocean_r = 40, ocean_c = 10, bootstrap = F, iter = 1000,
                      d_mort = 0.05, hr_c = 0.26, hr_r = 0.14, min_harvest_rate = 0.0001, release_mortality = release_mort) {
  set.seed(10)
  #####################################################################
  ### Step 1: Declare and define necessary functions and variables. ###
  #####################################################################

  # Create mappings from column name to column indices in rel_col_dt.
  BY_IDX = 1
  MNTH_IDX = 2
  AGE_IDX = 3
  FSHRY_IDX = 4
  LOC_IDX = 5
  MAT_GRP_IDX = 6
  SIZE_LIM_IDX = 7
  TOTAL_IDX = 8
  CATCH_IDX = 12
  REL_MORT_IDX = 14

  # Create mappings from column name to column indices in length_at_age.
  AGE_LAA_IDX = 1
  MNTH_LAA_IDX = 2
  MEAN_LAA_IDX = 3
  SD_LAA_IDX = 4

  num_rows = 0

  ### Create size_age_map ###
  size_age_map = hashmap()

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

    rel_reco_dt |>
      filter(fishery %in% fisheries) |>
      select(distinct_cols) |>
      distinct() |>
      nrow()
  }

  # Handle the case where c(MONTH, AGE) is not present in SIZE_AGE_DF. Iterate from the current time
  # to earlier time until we find a valid key. If this approach doesn't yield any valid key, iterate from the current time
  # to later time until we find a valid key.
  missing_size_age_handler <- function(month, age, size_age_df, size_age_map) {
    min_age_mnth_row = size_age_df |>
      arrange(month, age) |>
      head(1) |>
      unname()

    max_age_mnth_row = size_age_df |>
      arrange(month, age) |>
      tail(1) |>
      unname()

    if (age < min_age_mnth_row[1] | age == min_age_mnth_row[1] & month < min_age_mnth_row[2]) {
      return(min_age_mnth_row_row[3:4])
    } else if (age > min_age_mnth_row[1] | age == min_age_mnth_row[1] & month > min_age_mnth_row[2]) {
      return(max_age_mnth_row[3:4])
    }

    for (age in age : min_age_mnth_row[1]) {
      for (i in 1: 12) {
        month = ((month - 1) %% 12) + 1
        if (!is.null(size_age_map[[c(month, age)]])) {
          return(size_age_map[[c(month, age)]])
        }
      }
    }

    for (age in age : max_age_mnth_row[1] ) {
      for (i in 1: 12) {
        month = ((month + 1) %% 12) + 1
        if (!is.null(size_age_map[[c(month, age)]])) {
          return(size_age_map[[c(month, age)]])
        }
      }
    }
  }

  find_mean_sd <- function(month, age, size_age_map, size_age_df) {
    ####################################################
    ### Changed age to age + 1. This is a hacky fix. ###
    ####################################################
    mean_sd = size_age_map[[c(age + 1, month)]]

    if (is.null(mean_sd)) {
      warning("The specified month, age pair dooes not have any corresponding size at age data. NA is applied to the percent harvestable.")
      missing_size_age_handler(month, age, size_age_df)
    }

    return(mean_sd)
  }

  find_catch_vectorized <- function(mean, sd, size_limit, total_indiv) {
    total_indiv / max(c((1 - pnorm(size_limit, mean =  mean, sd = sd)), min_harvest_rate))
  }

  find_catch_bootstrap <- function(mean, sd, size_limit, total_indiv) {
    mapply(find_catch_vectorized, mean = mean, sd = sd, size_limit = size_limit, total_indiv)
  }

  bt_sum_helper <- function(est_num, prod_exp) {
    return(est_num / prod_exp)
  }

  bt_sum <- function(est_num, prod_exp) {
    return(mapply(bt_sum_helper, est_num = est_num, prod_exp = prod_exp) |> rowSums())
  }

  ########################################
  ### Step 2: Construct Intermediate 1 ###
  ########################################

  # Add parametric bootstrap if the flag is selected.
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

    # Add bootstrapped values.
    num_rows = rel_reco_dt |> nrow()
    prob = rep(1 / rel_reco_dt[['est_num']], times = iter, each = 1)
    bt_est_num = split(as.vector(vapply(prob, rnbinom, FUN.VALUE = 1, size = 1, n = 1)), 1 : num_rows)
    rel_reco_dt[, est_num := bt_est_num]
    rel_reco_dt = rel_reco_dt[, {
      total_indiv = bt_sum(est_num, prod_exp)
      mean_sd = find_mean_sd(month, age, size_age_map, size_age_df)
      mean = mean_sd[1]
      sd = mean_sd[2]
      catch = find_catch_bootstrap(mean, sd, size_limit, total_indiv)

      .(total_indiv = .(bt_sum(est_num, prod_exp)),
        mean = find_mean_sd(month, age, size_age_map, size_age_df)[1],
        sd = find_mean_sd(month, age, size_age_map, size_age_df)[2],
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
      left_join(release_mortality, by = c("run_year", "month", "fishery", "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-run_year)

    view(rel_reco_dt)
  } else {
    # Create the first intermediate data table with lazy data table.
    # This data table contains columns: BROOD_YEAR, MONTH, AGE, FISHERY, LOCATION, MATURATION_GRP, SIZE_LIMIT, TOTAL_INDIV, and CATCH.
    # The rows are sorted such that all fisheries related to maturation precede those related to impact.
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
      summarize(total_indiv = sum(est_num / prod_exp), est_num = sum(est_num)) |>
      mutate(mean = find_mean_sd(month, age, size_age_map, size_age_df)[1],
             sd = find_mean_sd(month, age, size_age_map, size_age_df)[2],
             catch = find_catch_vectorized(mean, sd, size_limit, total_indiv),
             harvest_rate = 1 - pnorm(size_limit, mean =  mean, sd = sd)) |>
      mutate(month = (month - birth_month) %% 12) |>
      arrange(brood_year, maturation_grp, age, month, fishery) |>
      mutate(month = (month + birth_month) %% 12)

    # Materialize lazy data table.
    rel_reco_dt = as.data.table(rel_reco_ldt) |>
      left_join(release_mortality, by = c("run_year", "month", "fishery", "location")) |>
      mutate(rate = case_when(
        is.na(rate) & fishery == ocean_r ~ hr_r,
        is.na(rate) & fishery == ocean_c ~ hr_c,
        TRUE ~ rate)) |>
      select(-run_year)
  }

  ##############################################
  ### Step 3: Calcuate Maturation and Impact ###
  ##############################################

  # Find the minimum necessary number of rows for maturation and impact data tables.
  yr_ag_cnt = num_uniq_rows(rel_reco_dt, c(spawn, hatchery, river), FALSE)
  yr_ag_mth_fshry_cnt = num_uniq_rows(rel_reco_dt, c(ocean_r, ocean_c), TRUE)

  # Declare and zero-fill a data table with columns "by", "age", and "maturation" for storing maturation.
  maturation_init_vec = rep(0, yr_ag_cnt)
  maturation_dt = data.table(by = maturation_init_vec,
                        age = maturation_init_vec,
                        maturation = maturation_init_vec)

  # Declare and zero-fill a data table for storing impact and natural mortality.
  impact_init_vec = rep(0, yr_ag_mth_fshry_cnt)
  impact_dt = data.table(by = impact_init_vec,
                         month = impact_init_vec,
                         age = impact_init_vec,
                         fishery = impact_init_vec,
                         impact = impact_init_vec)

  ### Variables for maturation calculation. ###
  prev_hat_esc = 0
  prev_sp_esc = 0
  prev_riv_harv = 0
  prev_m_year = rel_reco_dt[1, ..BY_IDX] |> unlist()
  prev_m_age = rel_reco_dt[1, ..AGE_IDX] |> unlist()
  row_m_idx = 1L

  # The value is true if the previous year has data from spawn, hatchery,
  # or river, and false otherwise. The value defaults to false.
  prev_m_year_valid = FALSE
  prev_i_year_valid = FALSE

  ### Variables for impact calculation. ###
  prev_com_impact = 0
  prev_rec_impact = 0
  prev_fishery = NA
  is_prev_ocean_r = FALSE
  is_ocean_r = FALSE
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_i_month = rel_reco_dt[1, ..MNTH_IDX] |> unlist()
  row_i_idx = 1L
  impact_column = list()
  maturation_column = list()

  # Aggregate function for calculating maturation and impact.
  find_imp_nat_mat <- function(record) {
    # browser()
    par_env = env_parent(current_env())
    if (bootstrap) {
      cur_year = record[[BY_IDX]] |> as.integer()
      cur_age = record[[AGE_IDX]] |> as.integer()
      cur_month = record[[MNTH_IDX]] |> as.integer()
      cur_fishery = record[[FSHRY_IDX]] |> as.numeric()
      # TODO: CHANGE THIS HACK.
      cur_rel_mort_rate = record[[REL_MORT_IDX - 1]] |> as.numeric()
      cur_indiv = record[[TOTAL_IDX]] |> unlist() |> as.numeric()
      catch = record[[CATCH_IDX]] |>  as.numeric()
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
      # if the record marks a changed brood year or age, push the maturation value onto the data table.
      if (((cur_year != prev_m_year && prev_m_year_valid)
           | (cur_year == prev_m_year && cur_age != prev_m_age))) {
        # par_env$maturation_dt |>
        #   set(i = row_m_idx, j = "maturation", value =
                # par_env$prev_sp_esc +
                # par_env$prev_riv_harv +
                # par_env$prev_hat_esc)
        maturation_column <<- append(par_env$maturation_column, list(par_env$prev_sp_esc +
                 par_env$prev_riv_harv +
                 par_env$prev_hat_esc))
        par_env$maturation_dt |>
          set(i = row_m_idx, j = "by", value = prev_m_year)
        par_env$maturation_dt |>
          set(i = row_m_idx, j = "age", value = prev_m_age)

        row_m_idx <<- row_m_idx + 1L
        prev_sp_esc = prev_riv_harv = prev_hat_esc = 0
        prev_m_year_valid <<- FALSE
      }

      # If fishery is spawning ground, river harvest, or hatchery escapement,
      # calculate maturation. Aggregate maturation to `maturation_temp` until we encounter
      # a new year.


      # Calculate number of individuals for each type of fishery.
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
      #cur_indiv = record[TOTAL_IDX] |> as.numeric()
      # browser()
      # If fishery is recreational or commercial ocean harvest, calculate impact.
      if (prev_i_year_valid && (cur_age != prev_i_age | cur_month != prev_i_month | cur_year != prev_i_year | (prev_fishery != cur_fishery && !is.na(prev_fishery)))) {
        if (par_env$is_prev_ocean_r) {
          is_ocean_r <<- TRUE
          # par_env$impact_dt |>
          #   set(i = row_i_idx, j = "impact", value = par_env$prev_rec_impact)
          impact_column <<- append(par_env$impact_column, list(prev_rec_impact))
          par_env$impact_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_impact <<- 0
        } else {
          is_ocean_r <<- FALSE
          # par_env$impact_dt |>
          #   set(i = row_i_idx, j = "impact", value = par_env$prev_com_impact)
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

  if (bootstrap) {
    for (i in 1 : num_rows) {
      find_imp_nat_mat(rel_reco_dt[i, ])
    }
  } else {
    apply(rel_reco_dt, 1, find_imp_nat_mat)
  }

  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of maturation here.
  # maturation_dt |>
  #   set(i = row_m_idx, j = "maturation", value =
  #         prev_sp_esc +
  #         prev_riv_harv +
  #         prev_hat_esc)
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
    # impact_dt |>
    #   set(i = row_i_idx, j = "impact", value = prev_rec_impact)
    impact_column = append(impact_column, list(prev_rec_impact))
    impact_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_r)
    prev_rec_impact = 0
  } else {
    # impact_dt |>
    #   set(i = row_i_idx, j = "impact", value = prev_com_impact)
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
    group_by(brood_year) |>
    arrange(desc(age), desc(month)) |>
    summarize(max_age = max(age), month = (birth_month - 2) %% 12 + 1)

  view(max_age_month_df)
  # view(rel_reco_dt)
  view(maturation_dt)
  view(impact_dt)

  return(list(maturation = maturation_dt, impact = impact_dt, max_age_month_df = max_age_month_df))
}


