library(dplyr)

test_that("bootstrap normal", {
  set.seed(1000)

  actual = cohort_reconstruct(
    rel = release,
    reco = recovery,
    birth_month = 6L,
    last_month = 12L,
    iter = 30L,
    verbose = FALSE,
    bootstrap = TRUE,
    detail = TRUE
  )

  set.seed(NULL)
})

if ("package:dplyr" %in% search()) detach("package:dplyr", unload = TRUE)
