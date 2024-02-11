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

    # Create a hashmap from NAT_MORT. The resulting hashmap NAT_MORT_MAP has AGE as its key.
    # Each key corresponds to the natural mortality rate of that age.
    create_nat_mort_map <- function(record) {
        record = unname(record)
        key = record[NM_RATE_IDX]
        value = record[NM_AGE_IDX]
        nat_mort_hp[[key]] <<- value
    }

    ###########################################
    ### Step 2: Reconstruct Ocean Abundance ###
    ###########################################

    # Variables for Reconstruction
    cur_year = impact_dt[1, ..IM_BY_IDX] |> unlist()
    cur_month = impact_dt[1, ..IM_MNTH_IDX] |> unlist()
    cur_age = impact_dt[1, ..IM_AGE_IDX] |> unlist()
}