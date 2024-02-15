### Reconstruction Functions ###

cohort_reconstruct <- function(maturation_dt, impact_dt, nat_mort, birth_month) {
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
    num_age = 4
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
    cur_year = impact_dt[1, ..IP_BY_IDX] |> unlist()
    cur_month = birth_month %% 12 - 1
    cur_age = num_age - 1
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


        cur_month <<- (cur_month - 1) %% 12
        if (cur_month == birth_month - 1) {
            cur_age <<- cur_age - 1
        }

        row_idx <<- row_idx + 1
    }

    recon_result = apply(cohort, 1, cohort_helper)
}

