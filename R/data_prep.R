### Data Preparation Function ###
data_prep <- function(rel, reco, size_at_age = length_at_age, rel_mort, nat_mort,
                      sex, spawn = 54, hatchery = 50, river = 46,
                      ocean_r = 10, ocean_c = 40, bootstrap, iter, d_mort = 0.05, hr_c = 0.26, hr_r = 0.14) {

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

  # If MONTH is true, find the number of unique year - age - month tuples for a specific fishery in PROVIDED_FISHERIES.
  # If MONTH is false, find the number of unique year - age pairs for a specific fishery in PROVIDED_FISHERIES.
  # PROVIDED_FISHERIES is passed in as a vector of fisheries.
  num_uniq_rows <- function(rel_reco_dt, fisheries, month) {
    distinct_cols = c("brood_year", "age")

    if (month) {
      distinct_cols = c(distinct_cols, "month")
    }

    rel_reco_dt |>
      filter(fishery %in% fisheries) |>
      select(distinct_cols) |>
      distinct() |>
      nrow()
  }

  # Find the hook and release mortality rate give LOCATION and MORT_MAP, which describes the
  # mapping between location and hook and releae mortality rate. MORT_MAP is a hashmap. LOCATION can be
  # any data type. If MORT_MAP is NA, use the mapping provided in
  # "Sacramento River Winter Chinook Cohort Reconstruction: Analysis of Ocean Fishery Impacts"
  find_hook_rel_mort <- function(location, mort_map, recreational) {
    if (is.na(mort_map)) {
      if (recreational) {
        return(0.14)
      } else {
        return(0.26)
      }
    } else {
      # when constructing the hashmap, use (location, recreational) as key.
      return(mort_map[[c(location, recreational)]])
    }
  }

  # Handle the case where c(MONTH, AGE) is not present in SIZE_AGE_DF. Iterate from the current time
  # backward until we find a valid key. If backward doesn't yield any valid key, iterate from the current time
  # forward until we find a valid key.
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
  # Assume population distributes in normal distribution.
  percent_harvest <- function(month, age, size_limit, size_age_map, size_age_df) {
    mean_sd = size_age_map[[c(age, month)]]
    if (is.null(mean_sd)) {
      warning("The specified month, age pair dooes not have any corresponding size at age data. NA is applied to the percent harvestable.")
      mean_sd = NA #missing_size_age_handler(month, age, size_age_df)
    }

    1 - pnorm(size_limit, mean = mean_sd[1], sd = mean_sd[2])
  }

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

  apply(length_at_age, 1, create_size_age_map)

  # Assign value according to fishery group. If the provided fishery is ocean_r or ocean_c,
  # return the estimated number of catch. Return NA otherwise.
  # find_catch <- function(month, age, size_limit, fishery, total_indiv) {
  #   par_env = env_parent(current_env())
  #   result = c()
  #   for (i in 1:length(month)) {
  #       if (fishery[i] %in% c(ocean_r, ocean_c)) {
  #         result = c(result, total_indiv[i] * percent_harvest(month[i], age[i], size_limit[i], par_env$size_age_map, par_env$size_at_age))
  #       } else {
  #         result = c(result, NA)
  #       }
  #   }
  #   return(result)
  # }

  # Helper function for find_catch to iterate over all rows through apply.
  find_catch_helper <- function(i, month, age, size_limit, fishery, total_indiv) {
    par_env = env_parent(current_env())

    if (fishery %in% c(ocean_r, ocean_c)) {
      return(total_indiv * percent_harvest(month, age, size_limit, par_env$size_age_map, size_at_age))
    } else {
      return(NA)
    }
  }

  # find_catch with sapply.
  find_catch <- function(month, age, size_limit, fishery, total_indiv) {
    par_env = env_parent(current_env())
    catch_result = 1:length(month)
    
    return(sapply(X = 1:length(month), FUN = find_catch_helper, m = month, a = age, s = size_limit, f = fishery, t = total_indiv))
  }

  ######################
  ### Intermediate 1 ###
  ######################

  # Create the first intermediate data table with lazy data table.
  rel_reco_ldt = reco |>
    left_join(rel, by = 'tag_code') |>
    setDT() |>
    lazy_dt(immutable = F, key_by = c("brood_year", "month", "fishery", "location", "size_limit")) |>
    mutate(age = case_when(
      month >= birth_month ~ run_year - brood_year,
      TRUE ~ run_year - brood_year - 1
    )) |>
    mutate(maturation_grp = case_when(
      fishery %in% c(spawn, hatchery, river) ~ 1,
      TRUE ~ 2
    )) |>
    group_by(brood_year, month, age, fishery, location, maturation_grp, size_limit) |>
    summarize(total_indiv = sum(est_num / prod_exp)) |>
    mutate(catch = find_catch(month, age, size_limit, fishery, total_indiv)) |>
    arrange(brood_year, maturation_grp, age, month, fishery)


  # Materialize lazy data table.
  rel_reco_dt = as.data.table(rel_reco_ldt)

  yr_ag_cnt = num_uniq_rows(rel_reco_dt, c(spawn, hatchery, river), FALSE)
  yr_ag_mth_cnt = num_uniq_rows(rel_reco_dt, c(ocean_r, ocean_c), TRUE)

  ######################
  ### Maturation Calc ###
  ######################

  # Declare and zero-fill a data table with columns "by", "age", and "maturation"
  # for storing maturation.

  m_init_vec = rep(0, yr_ag_cnt)

  maturation_dt = data.table(by = m_init_vec,
                             age = m_init_vec,
                             maturation = m_init_vec)

  # Declare and zero-fill a data table for storing impact and natural mortality.
  inm_init_vec = rep(0, yr_ag_mth_cnt)

  impact_nat_mort_dt = data.table(by = inm_init_vec,
                                  month = inm_init_vec,
                                  age = inm_init_vec,
                                  impact = inm_init_vec,
                                  nat_mort = inm_init_vec,
                                  fishery = inm_init_vec)

  ### Variables for maturation calculation. ###
  prev_hat_esc = 0
  prev_sp_esc = 0
  prev_riv_harv = 0

  # The value is true if the previous year has data from spawn, hatchery,
  # or river, and false otherwise. The value defaults to false.
  prev_m_year_valid = FALSE
  prev_i_year_valid = FALSE

  ### Variables for impact calculation. ###
  prev_com_impact = 0
  prev_rec_impact = 0
  is_prev_ocean_r = FALSE

  ### Iterator variables ###
  prev_m_year = rel_reco_dt[1, ..BY_IDX] |> unlist()
  prev_m_age = rel_reco_dt[1, ..AGE_IDX] |> unlist()
  prev_i_year = prev_m_year
  prev_i_age = prev_m_age
  prev_i_month = rel_reco_dt[1, ..MNTH_IDX] |> unlist()
  row_m_idx = 1L
  row_i_idx = 1L


  # Aggregate function for calculating impact, maturation, and natural mortality.
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
      # If fishery is recreational or commercial ocean harvest, calculate natural
      # mortality and impact.

      # Calculate impact.
      ######################
      ### Impact Calc ###
      ######################
      if (prev_i_year_valid && (cur_age != prev_i_age | cur_month != prev_i_month | cur_year != prev_i_year)) {
        if (par_env$is_prev_ocean_r) {
          is_ocean_r <<- TRUE
          par_env$impact_nat_mort_dt |>
            set(i = row_i_idx, j = "impact", value = par_env$prev_rec_impact)
          par_env$impact_nat_mort_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_r)
          prev_rec_impact = 0
        } else {
          is_ocean_r <<- FALSE
          par_env$impact_nat_mort_dt |>
            set(i = row_i_idx, j = "impact", value = par_env$prev_com_impact)
          par_env$impact_nat_mort_dt |>
            set(i = row_i_idx, j = "fishery", value = ocean_c)
          prev_com_impact = 0
        }

        par_env$impact_nat_mort_dt |>
          set(i = row_i_idx, j = "by", value = prev_i_year)
        par_env$impact_nat_mort_dt |>
          set(i = row_i_idx, j = "age", value = prev_i_age)
        par_env$impact_nat_mort_dt |>
          set(i = row_i_idx, j = "month", value = prev_i_month)

        row_i_idx <<- row_i_idx + 1L
        prev_i_year_valid <<- FALSE
      }

      # If fishery is spawning ground, river harvest, or hatchery escapement,
      # calculate maturation. Aggregate maturation to `maturation_temp` until we encounter
      # a new year.
      
      total_indiv = record[TOTAL_IDX] |> as.numeric()
      catch = record[CATCH_IDX] |>  as.numeric()
      # Calculate number of individuals for each type of fishery.
      if (cur_fishery == ocean_r) {
        is_prev_ocean_r <<- TRUE
        prev_rec_impact <<- prev_rec_impact + total_indiv + catch * d_mort
        + (catch - total_indiv) * hr_r
      } else {
        is_prev_ocean_r <<- FALSE
        prev_com_impact <<- prev_com_impact + total_indiv + catch * d_mort
        + (catch - total_indiv) * hr_c
      }

      prev_i_year_valid <<- TRUE

      prev_i_year <<- cur_year
      prev_i_age <<- cur_age
      prev_i_month <<- cur_month
    } #end impact calc

    return(NULL)
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
    impact_nat_mort_dt |>
      set(i = row_i_idx, j = "impact", value = prev_rec_impact)
    impact_nat_mort_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_r)
    prev_rec_impact = 0
  } else {
    impact_nat_mort_dt |>
      set(i = row_i_idx, j = "impact", value = prev_com_impact)
    impact_nat_mort_dt |>
      set(i = row_i_idx, j = "fishery", value = ocean_c)
    prev_com_impact = 0
  }

  impact_nat_mort_dt |>
    set(i = row_i_idx, j = "by", value = prev_i_year)
  impact_nat_mort_dt |>
    set(i = row_i_idx, j = "age", value = prev_i_age)
  impact_nat_mort_dt |>
    set(i = row_i_idx, j = "month", value = prev_i_month)

  view(rel_reco_dt)
  view(maturation_dt)
  view(impact_nat_mort_dt)
}
