#################################
### Data Preparation Function ###
#################################

data_prep <- function(rel, reco, size_at_age = length_at_age, rel_mort, nat_mort,
                      sex, spawn = 54, hatchery = 50, river = 46,
                      ocean_r = 40, ocean_c = 10, bootstrap, iter, d_mort = 0.05, hr_c = 0.26, hr_r = 0.14) {

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
  CATCH_IDX = 9

  # Create mappings from column name to column indices in length_at_age.
  AGE_LAA_IDX = 1
  MNTH_LAA_IDX = 2
  MEAN_LAA_IDX = 3
  SD_LAA_IDX = 4


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

  # Find the percent harvestable for a specific MONTH and AGE given SIZE_LIMIT and SIZE_AGE_MAP.
  # Assume population follows a Gaussian distribution with mean and variance provided by SIZE_AGE_MAP.
  percent_harvest <- function(month, age, size_limit, size_age_map, size_age_df) {
    mean_sd = size_age_map[[c(age + 1, month)]]
    if (is.null(mean_sd)) {
      warning("The specified month, age pair dooes not have any corresponding size at age data. NA is applied to the percent harvestable.")
      missing_size_age_handler(month, age, size_age_df)
    }

    1 - pnorm(size_limit, mean = mean_sd[1], sd = mean_sd[2])
  }

  # Helper function for find_catch to iterate over all rows through apply.
  find_catch_helper <- function(i, month, age, size_limit, fishery, total_indiv) {
    if (fishery %in% c(ocean_r, ocean_c)) {
      return(total_indiv / percent_harvest(month, age, size_limit, size_age_map, size_at_age))
    } else {
      return(NA)
    }
  }

  # Find the number of caught individuals represented by each valid recovered tag code.
  find_catch <- function(month, age, size_limit, fishery, total_indiv) {
    par_env = env_parent(current_env())
    catch_result = 1:length(month)

    return(sapply(X = 1:length(month), FUN = find_catch_helper, m = month, a = age, s = size_limit, f = fishery, t = total_indiv))
  }

  ########################################
  ### Step 2: Construct Intermediate 1 ###
  ########################################

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
    group_by(brood_year, month, age, fishery, location, maturation_grp, size_limit) |>
    summarize(total_indiv = sum(est_num / prod_exp)) |>
    mutate(catch = find_catch(month, age, size_limit, fishery, total_indiv)) |>
    arrange(brood_year, maturation_grp, age, month, fishery)

  # Materialize lazy data table.
  rel_reco_dt = as.data.table(rel_reco_ldt)

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
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_i_month = rel_reco_dt[1, ..MNTH_IDX] |> unlist()
  row_i_idx = 1L

  # Aggregate function for calculating maturation and impact.
  find_imp_nat_mat <- function(record) {
    par_env = env_parent(current_env())

    cur_year = record[BY_IDX] |> as.integer()
    cur_age = record[AGE_IDX] |> as.integer()
    cur_month = record[MNTH_IDX] |> as.integer()
    cur_fishery = record[FSHRY_IDX] |> as.numeric()

    if (cur_fishery %in% c(spawn, river, hatchery)) {
      # if the record marks a changed brood year or age, push the maturation value onto the data table.
      if (((cur_year != prev_m_year && prev_m_year_valid)
           | (cur_year == prev_m_year && cur_age != prev_m_age))) {
        par_env$maturation_dt |>
          set(i = row_m_idx, j = "maturation", value =
                par_env$prev_sp_esc +
                par_env$prev_riv_harv +
                par_env$prev_hat_esc)
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
      total_indiv = record[TOTAL_IDX] |> as.numeric()

      # Calculate number of individuals for each type of fishery.
      if (cur_fishery == spawn) {
        prev_sp_esc <<- prev_sp_esc + total_indiv
      } else if (cur_fishery == river) {
        prev_riv_harv <<- prev_riv_harv + total_indiv
      } else {
        prev_hat_esc <<- prev_hat_esc + total_indiv
      }

      prev_m_year_valid <<- TRUE
      prev_m_year <<- cur_year
      prev_m_age <<- cur_age

    } else if (record[FSHRY_IDX] %in% c(ocean_r, ocean_c)) {
      total_indiv = record[TOTAL_IDX] |> as.numeric()
      catch = record[CATCH_IDX] |>  as.numeric()

      # If fishery is recreational or commercial ocean harvest, calculate impact.
      if (prev_i_year_valid && (cur_age != prev_i_age | cur_month != prev_i_month | cur_year != prev_i_year | (prev_fishery != cur_fishery && !is.na(prev_fishery)))) {
        if (par_env$is_prev_ocean_r) {
          is_ocean_r <<- TRUE
          par_env$impact_dt |>
            set(i = row_i_idx, j = "impact", value = par_env$prev_rec_impact)
          par_env$impact_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_impact = 0
        } else {
          is_ocean_r <<- FALSE
          par_env$impact_dt |>
            set(i = row_i_idx, j = "impact", value = par_env$prev_com_impact)
          par_env$impact_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_c)
          prev_com_impact = 0
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
        prev_rec_impact <<- prev_rec_impact + total_indiv + catch * d_mort + (catch - total_indiv) * hr_r
      } else {
        is_prev_ocean_r <<- FALSE
        prev_com_impact <<- prev_com_impact + total_indiv + catch * d_mort + (catch - total_indiv) * hr_c
      }

      prev_i_year_valid <<- TRUE
      prev_i_year <<- cur_year
      prev_i_age <<- cur_age
      prev_i_month <<- cur_month
      prev_fishery <<- cur_fishery
    }
  }

  apply(rel_reco_dt, 1, find_imp_nat_mat)

  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of maturation here.
  maturation_dt |>
    set(i = row_m_idx, j = "maturation", value =
          prev_sp_esc +
          prev_riv_harv +
          prev_hat_esc)
  maturation_dt |>
    set(i = row_m_idx, j = "by", value = prev_m_year)
  maturation_dt |>
    set(i = row_m_idx, j = "age", value = prev_m_age)

  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of impact here.
  if (is_ocean_r) {
    impact_dt |>
      set(i = row_i_idx, j = "impact", value = prev_rec_impact)
    impact_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_r)
    prev_rec_impact = 0
  } else {
    impact_dt |>
      set(i = row_i_idx, j = "impact", value = prev_com_impact)
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

  view(rel_reco_dt)
  view(maturation_dt)
  view(impact_dt)
  return(maturation_dt)
  return(impact_dt)
}


