## Testing for data table vs data frame left join speed.
# Create a lazy_dt with brood_year, month, age, total_individual, fishery, and location columns. #
release_test = release
recovery_test = recovery

for (i in 1:8) {
  release_test = rbind(release_test, release_test)
  recovery_test = rbind(recovery_test, recovery_test)
}

rel_1 = release_test
rel_2 = release_test

reco_1 = recovery_test
reco_2 = recovery_test

{rel = lazy_dt(as.data.table(rel_1), immutable = FALSE, key_by = c('tag_code')) # Add keys for possible speedup during joins (used for index nested loop joins or sorted merge join)
reco = lazy_dt(as.data.table(reco_1), immutable = FALSE, key_by = c('tag_code')) # Add keys for possible speedup during joins (used for index nested loop joins or sorted merge join)
rel_reco = reco %>% left_join(rel, by = 'tag_code') %>% as.data.table} %>% system.time()

rm(rel_1)
rm(reco)
rm(rel)
rm(reco_1)
rm(release_test)
rm(recovery_test)

{rel_reco = reco_2 %>% left_join(rel_2, by = 'tag_code') } %>% system.time()

rm(rel_2)
rm(reco_2)


{data_prep(release, recovery)} |> system.time()


