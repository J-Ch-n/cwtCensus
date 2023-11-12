### User Exposed Functions ###

#' Generates a cohort reconstruction for salmon or other anadromous fish.
#'
#' @return list of cohort reconstructions with or without bootstrapping.
#' @export
#' @examples
ch_reco <- function(rel, reco, size_at_age = release, rel_mort = release, nat_mort = release,
                    sex = "both", fisheries = release, bootstrap = TRUE, iter = 1000) {
  # Throws warning or error if necessary.
  error_handler(rel, reco, size_at_age, rel_mort, nat_mort,
              sex, fisheries, bootstrap, iter)

  # Prepare necessary data for conhort reconstruction.
  data_prep(rel, reco, size_at_age, rel_mort, nat_mort,
            sex, fisheries, bootstrap, iter)

  #
}


