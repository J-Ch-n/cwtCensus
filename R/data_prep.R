### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, fisheries, bootstrap, iter) {

  # Cohort reconstruction list.
  Cohot_List <- list();

  # Shared recovery data
  Recovery <- lazy_dt(reco)
  # Shared release data
  Release <- lazy_dt(rel)

  # Combined data that includes recovery and release, adding necessary columns.
  Comb_dat <- left_join(Release, Recovery, by = 'tag_code') %>%
    mutate(age = run_year - brood_year)

  BY <- CWT_Releases %>% #BY for each batch
        select(tag_code, brood_year, Phi)

  # Finds the initial ocean abundance by brood_year.
  release_data <- function(release) {
    release<- release %>%
      group_by(brood_year) %>%
      summarise(Individuals_Released = sum(Total_Released))
    return(release)
  }

  # Returns a data frame for hatchery escapement grouped by run_year and brood_year.
  # of the form
  #       Age run_year   brood_year   Hat
  # 1     4     2002       1998       1.01
  # 2     4     2006       2002       1.04
  escape_to_hat <- function(recoveries) {
    Recovery %>%
      # How to deal with dynamic fishery mapping?.
      filter(fishery == 50) %>%
      group_by(run_year, brood_year) %>%
      # We don't have Phi yet.
      summarise(Escapement_to_Hatchery = sum(estimated_number / Phi)) %>%
      mutate(Age = run_year-brood_year)%>%
      group_by(Age, run_year, brood_year) %>%
      summarize(Hat = sum(Escapement_to_Hatchery)) %>%
      return()
  }


  # Returns a list of bootstrapped estimates if bootstrap is true.
  # Returns a list of one point estimate if bootrstrap is false.
  escape_to_spawn <- function(Recovery, bootstrap, iter) {
    if (!bootstrap) {
      Cohort_List[[1]] <<- Comb_dat %>%
        filter(fishery == 54) %>% #from spawning ground surveys
        group_by(run_year, brood_year, age) %>%
        summarise(Individuals = sum(estimated_number / Phi)) %>%
        group_by(age, run_year) %>%
        summarize(population = n(age, na.rm = T))
    } else {
      #BOOTSTRAP for uncertainty
      CWT_SG<- CWT_Recoveries %>%
        select(reporting_agency,run_year,period, fishery, sex, tag_code, estimated_number, recovery_location_code, brood_year,  estimated_number, Phi) %>%
        filter(fishery == 54)
      CWT_SG$Individuals<-NA #Empty space for resampled value
      CWT_SG_null<-CWT_SG
      Cohort<-list() #Cohort will be a table of Spawners from each age from 20 years, sampled 1000 times
      for(j in 1:iter){
        CWT_SG<-CWT_SG_null #at the start of every iteration, reset Individuals column
        for(i in 1:268){ # 1: CWT_SG$tag_code
          #how many individuals each tag represents equals (1 + resampled number of unrecovered)/Phi
          CWT_SG$Individuals[i]<-sum(c(1,rnbinom(1, 1, (1/(CWT_SG$estimated_number[i])))))/CWT_SG$Phi[i]
        }
        #summing up age-specific escapement
        CWT_SG<-CWT_SG %>%
          mutate(Age = run_year - brood_year) %>%
          group_by(run_year, brood_year, Age) %>%
          summarise(Individuals= sum(Individuals))
        CWT_SG<-CWT_SG %>% #making each Age into its own column
          pivot_wider(names_from = Age, values_from = Individuals, names_sort=TRUE)
        CWT_SG<-CWT_SG %>%
          group_by(brood_year) %>%
          summarize(Age2Sp = sum(`2`, nfa.rm = TRUE), Age3Sp = sum(`3`, na.rm = TRUE)
                    , Age4Sp = sum(`4`, na.rm = TRUE))
        Cohort[[j]] <- CWT_SG
      }
    }
  }

  river_harvest <- function(recoveries) {

  }

  ocean_harvest <- function(release, recoveries) {

  }
  rec_harvest <- function(release, recoveries) {

  }
  com_harvest <- function(release, recoveries) {

  }
  harvest_compute <- function(releases, recoveries, bootstrap, iter) {

  }
}
