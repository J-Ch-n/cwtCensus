find_impact = function(catch, total_indiv, d_mort, hr_m) {
    harvest = total_indiv
    drop_off = catch * d_mort
    hook_release = (catch - harvest) * hr_m
    harvest + drop_off + hook_release
}

