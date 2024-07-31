<!-- README.md is generated from README.Rmd. Please edit that file -->

# cwtCensus

<!-- badges: start -->

<!-- badges: end -->

The goal of cwtCensus is to provide a fast and memory efficient tool for cohort reconstruction using point and bootstrapped estimates. Writing efficient code for cohort reconstruction can be challenging, so we developed cwtCensus to abstract away the details of tweaking lines of R and designing algorithms. With this package, you can generate bootstrapped and non-bootstrapped cohort reconstructions with just one click!

## Installation

You can install the development version of cwtCensus from [GitHub](https://github.com/J-Ch-n/cwtCensus/tree/main) with:

``` r
install.packages("devtools")
devtools::install_github("J-Ch-n/cohort_reco_pk")
```

## Example

This is a basic example showing you how to solve a common cohort reconstruction problem:

``` r
library(cwtCensus)
set.seed(998784)

result = cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = F, last_month = 12L, iter = 10L,
  level = 0.95, detail = F, sex = "both", verbose = F)

# Time spent on generating point estimate cohort reconstruction.
cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = F, last_month = 12L, iter = 10L,
  level = 0.95, detail = F, sex = "both", verbose = F) |> system.time()
#>    user  system elapsed 
#>   0.421   0.008   0.429

# Viewing the 1998 brood year age 3 cohort in June.
result[result$by == 2007 & result$age == 3 & result$month == 6] |> unlist()
#>              by             age           month ocean_abundance 
#>        2007.000           3.000           6.000        7965.572

result = cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = T, last_month = 12L, iter = 1000L,
  level = 0.95, detail = T, sex = "both", verbose = F)

# Viewing the bootstrapped summary of 1998 brood year age 3 cohort in June.
result[["2007"]][["3"]][["6"]][["summary"]]
#>                  median     sd CrI_low CrI_high
#> ocean_abundance 7927.95 279.29 7413.22   8499.8
#> impact             0.00   0.00    0.00      0.0
#> maturation         0.00   0.00    0.00      0.0
#> natural_mort     146.06   5.15  136.58    156.6

# Time spent on generating bootstrapped cohort reconstruction with 1000 iterations.
cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = T, last_month = 12L, iter = 1000L,
  level = 0.95, detail = T, sex = "both", verbose = F) |> system.time()
#>    user  system elapsed 
#>  57.705   3.277  61.202
```

## More Information

You can find a more detailed tutorial at <https://J-Ch-n.github.io/cwtCensus>.

## Getting Help

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/J-Ch-n/cwtCensus/issues).
