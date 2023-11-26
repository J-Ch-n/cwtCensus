### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, ret_spawn_id, ret_hat_id, river_harvest,
                      ocean_rec, ocean_com, bootstrap, iter) {

  # Mapping between internal fishery id and user provided fishery ids. #
  spawn = ret_spawn_id
  hat = ret_hat_id
  river = river_harvest
  ocean_c = ocean_com
  ocean_r = ocean_rec

  # Use data.frame left join (faster than lazy_dt) see `cohort_recon_pk/exploratory_files/data_prep_exploration.R`
  rel_reco = reco %>% left_join(rel, by = 'tag_code')

}
