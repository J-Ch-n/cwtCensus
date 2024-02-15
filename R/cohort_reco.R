### Reconstruction Functions ###

cohort_reconstruct <- function(maturation_dt, impact_dt, nat_mort, birth_month, max_age = 6, impact_fisheries = c(40, 10)) {
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
    ### Step 1: Setup Cohort Reconsutrction ###
    ###########################################
    nat_mort_hp = hashmap()

    num_by = 10
    num_age = max_age - 2
    num_month = 12
    init_vec = rep(0, num_by * num_age * num_month)

    cohort = data.table(by = init_vec,
                month = init_vec,
                age = init_vec,
                ocean_abundance = init_vec)

    # Create a hashmap from NAT_MORT. The resulting hashmap NAT_MORT_MAP has AGE as its key.
    # Each key corresponds to the natural mortality rate of that age.
    create_nat_mort_map <- function(record) {
        record = unname(record)
        key = record[NM_RATE_IDX]
        value = record[NM_AGE_IDX]
        nat_mort_hp[[key]] <<- value
    }

    apply(nat_mort, 1, create_nat_mort_map)
    ###########################################
    ### Step 2: Reconstruct Ocean Abundance ###
    ###########################################

    # Variables for Reconstruction
    cur_year = impact_dt[1, ..IP_BY_IDX] |> unlist() |> unname()
    cur_month = birth_month %% 12 - 1
    cur_age = max_age
    row_idx = 1L

    # Stores the previous month and its ocean abundance.
    prev_mnth_N = c(cur_month, 0)

    # Stores the previous age, month, and their ocean abundance.
    prev_age_mnth_N = c(cur_age, cur_month, 0)

    cohort_helper <- function(record) {
        # Start from min BY. For each BY:
        # Start from Age 6 (Oldest theoretically non-zero cohort) and if no data for that month, zero fill the cell.
        # Go backwards in time, from least recent to most recent, month by month.
        # Start reconstruction at the first existent data point.
        # Apply the iterative rule.
        # Save the prev_ocean_abundnace.
        # Query impact and maturation for the right age/year.
        # When done with that brood year/month/age pair, write it in the cohort table.
        # browser()


        # Local variables to accumulate maturation and impact.
        cur_maturation = 0
        cur_impact = 0

        # TODO: The maturation query can be optimized to activate at every age change.
        cur_maturation_rows = maturation_dt[by == cur_year & age == cur_age, ..MA_MA_IDX]

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

        cur_month <<- cur_month %% 12 + 1
        if (cur_month == birth_month) {
            cur_age <<- cur_age - 1
        }

        if (cur_age == 1) {
          cur_year <<- cur_year + 1
          cur_age <<- max_age
        }

        row_idx <<- row_idx + 1
    }

    recon_result = apply(cohort, 1, cohort_helper)
}

