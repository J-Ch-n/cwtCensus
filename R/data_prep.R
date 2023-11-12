### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, fisheries, bootstrap, iter){


  BY <- CWT_Releases %>% #BY for each batch
        select(tag_code, brood_year, Phi)

  release_data <- function(release){
    release<- release %>%
      group_by(brood_year) %>%
      summarise(Individuals_Released = sum(Total_Released))
    return(release)
  }

  escape_to_hat <- function(recoveries){
    CWT_Hatchery<-CWT_Recoveries %>%
      filter(fishery ==50) %>%
      group_by(run_year, brood_year) %>%
      summarise(Escapement_to_Hatchery = sum(estimated_number/Phi)) %>%
      mutate(Age = run_year-brood_year)%>%
      group_by(Age, run_year, brood_year) %>%
      summarize(Hat = sum(Escapement_to_Hatchery))
    return(CWT_Hatchery)
    # of the form
    #       Age run_year   brood_year   Hat
    # 1     4     2002       1998       1.01
    # 2     4     2006       2002       1.04
  }
  escape_to_spawn <- function(recoveries, bootstrap, iter){

  }

  river_harvest <- function(recoveries){

  }

  ocean_harvest <- function(release, recoveries){

  }
  rec_harvest <- function(release, recoveries) {

  }
  com_harvest <- function(release, recoveries){

  }
  harvest_compute <- function(releases, recoveries, bootstrap, iter){

  }
}
