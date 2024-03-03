### Reconstruction Functions ###

cohort_reconstruct <- function(maturation_dt, impact_dt, nat_mort, birth_month, max_age_month_df,
                               max_age = 6, impact_fisheries = c(40, 10)) {
    # Natural mortality indices
    NM_AGE_IDX = 1
    NM_RATE_IDX = 2

    # Impact indices
    IP_BY_IDX = 1
    IP_MNTH_IDX = 2
    IP_AGE_IDX = 3
    IP_FSHRY_IDX = 4
    IP_IMP_IDX = 5

    # Maturation indices
    MA_BY_IDX = 1
    MA_AGE_IDX = 2
    MA_MA_IDX = 3

    ###########################################
    ### Step 1: Setup Cohort Reconstruction ###
    ###########################################
    nat_mort_hp = hashmap()
    view(max_age_month_df)
    # TODO: find the number of BY. This should be done during data prep.
    num_by = nrow(max_age_month_df)
    num_by_age_month = sum((max_age_month_df$max_age - 2) * 12 + ((max_age_month_df$month - birth_month) %% 12 + 1))

    init_vec = rep(0, num_by_age_month)

    cohort = data.table(by = init_vec,
                age = init_vec,
                month = init_vec,
                ocean_abundance = init_vec)

    # Create a hashmap from NAT_MORT. The resulting hashmap NAT_MORT_MAP has AGE as its key.
    # Each key corresponds to the natural mortality rate of that age.
    create_nat_mort_map <- function(record) {
        record = unname(record)
        key = record[NM_AGE_IDX] |> unname()
        value = record[NM_RATE_IDX] |> unname()
        nat_mort_hp[[key]] <<- value
    }

    apply(nat_mort, 1, create_nat_mort_map)
    ###########################################
    ### Step 2: Reconstruct Ocean Abundance ###
    ###########################################

    # Use mortality rates from https://www.researchgate.net/publication/279530889_Sacramento_River_Winter_Chinook_Cohort_Reconstruction_Analysis_of_Ocean_Fishery_Impacts
    handle_missing_mort_rate <- function(age) {
      if (age == 2) {
        return(0.0561)
      }

      return(0.0184)
    }

    # Takes in MATURATION, MONTH, OCEAN_ABUNDANCE, NAT_MORT_RATE and calculate the corresponding mortality count.
    find_mortality <- function(maturation, prev_mnth_N, nat_mort_rate) {
      return(prev_mnth_N * (nat_mort_rate) / (1 - nat_mort_rate))
    }

    # Finds the morality rate of a particular AGE. If the age doesn't exist, invoke the error handler.
    find_mortality_rate <- function(age) {
      mort_rate = nat_mort_hp[[age]]
      if (is.null(mort_rate)) {
        mort_rate = handle_missing_mort_rate(age)
      }

      return(mort_rate)
    }

    # Variables for Reconstruction
    row_idx = 1L
    max_age_month_idx = 1L

    cur_year = impact_dt[1, ..IP_BY_IDX] |> unlist() |> unname()
    cur_age = max_age_month_df$max_age[[max_age_month_idx]]
    cur_month = max_age_month_df$month[[max_age_month_idx]]

    # Stores the previous month and its ocean abundance.
    prev_mnth_N = 0

    cohort_helper <- function(record) {
        # Start from min BY. For each BY:
        # Start from Age 6 (Oldest theoretically non-zero cohort) and if no data for that month, zero fill the cell.
        # Go backwards in time, from least recent to most recent, month by month.
        # Start reconstruction at the first existent data point.
        # Apply the iterative rule.
        # Save the prev_ocean_abundnace.
        # Query impact and maturation for the right age/year.
        # When done with that brood year/month/age pair, write it in the cohort table.

        # Local variables to accumulate maturation and impact.
        cur_maturation = 0
        cur_impact = 0

        par_env = env_parent(current_env())

        cur_maturation_rows = 0
        # TODO: The maturation query can be optimized to activate at every age change.
        if (cur_month %% 12 + 1 == birth_month) {
          cur_maturation_rows = maturation_dt[by == cur_year & age == cur_age, ..MA_MA_IDX]
        }
        cur_impact_rows = impact_dt[by == cur_year & age == cur_age & month == cur_month & (fishery %in% impact_fisheries), ..IP_IMP_IDX]

        cur_maturation_mat = as.matrix(cur_maturation_rows)
        cur_impact_mat = as.matrix(cur_impact_rows)

        cur_maturation_mat_size = length(cur_maturation_mat)
        cur_impact_mat_size = length(cur_impact_mat)

        # TODO: This is too messy. Make the following logic concise.
        if (cur_maturation_mat_size == 0 && cur_impact_mat_size == 0) {
          cur_maturation_mat = 0
          cur_impact_mat = 0
        } else if (cur_maturation_mat_size == 0) {
          cur_maturation = 0
          for (impact in cur_impact_mat[, 1]) {
            cur_impact = cur_impact + impact
          }
        } else if (cur_impact_mat_size == 0) {
          cur_impact_mat = 0
          for (maturation in cur_maturation_mat[, 1]) {
            cur_maturation = cur_maturation + maturation
          }
        } else {
          for (maturation in cur_maturation_mat[, 1]) {
            cur_maturation = cur_maturation + maturation
          }
          for (impact in cur_impact_mat[, 1]) {
            cur_impact = cur_impact + impact
          }
        }

        nat_mort_rate = find_mortality_rate(cur_age)
        cur_mortality = find_mortality(cur_maturation, prev_mnth_N, nat_mort_rate)

        cur_ocean_abundance = cur_impact + cur_mortality + prev_mnth_N
        if (cur_month == (birth_month - 2) %% 12 + 1) {
          cur_ocean_abundance = cur_ocean_abundance + cur_maturation
        } else {
          cur_ocean_abundance = cur_ocean_abundance
        }

        par_env$cohort |>
          set(i = row_idx, j = "ocean_abundance", value = cur_ocean_abundance)
        par_env$cohort |>
          set(i = row_idx, j = "by", value = cur_year)
        par_env$cohort |>
          set(i = row_idx, j = "month", value = cur_month)
        par_env$cohort |>
          set(i = row_idx, j = "age", value = cur_age)

        prev_mnth_N <<- cur_ocean_abundance
        row_idx <<- row_idx + 1L

        if (cur_month == birth_month) {
          cur_age <<- cur_age - 1
        }

        if (cur_age == 1) {
          if (cur_month == birth_month) {
            if (max_age_month_idx < num_by) {
              max_age_month_idx <<- max_age_month_idx + 1
            }
            cur_year <<- max_age_month_df$brood_year[[max_age_month_idx]]
          }

          cur_age <<- max_age_month_df$max_age[[max_age_month_idx]]
          cur_month <<- max_age_month_df$month[[max_age_month_idx]]
          prev_mnth_N <<- 0
        } else {
          cur_month <<- (cur_month - 2) %% 12 + 1
        }
    }

    recon_result = apply(cohort, 1, cohort_helper)
    view(cohort)
}

