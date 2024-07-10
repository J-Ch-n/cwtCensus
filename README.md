
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cwtCensus

<!-- badges: start -->
<!-- badges: end -->

The goal of cwtCensus is to provide a fast and memory efficient tool for
cohort reconstruction using point and bootstrapped estimates. Writing
efficient code for cohort reconstruction can be challenging, so we
developed cwtCensus to abstract away the details of tweaking R code and
designing algorithms. With this package, you can generate bootstrapped
cohort reconstruction with just one click!

## Installation

You can install the development version of cwtCensus from
[GitHub](https://github.com/J-Ch-n/cwtCensus/tree/main) with:

``` r
install.packages("devtools")
devtools::install_github("J-Ch-n/cohort_reco_pk")
```

## Example

This is a basic example which shows you how to solve a common cohort
reconstruction problem:

``` r
library(cwtCensus)
set.seed(998784)

result = cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = F, last_month = 12, iter = 10,
  level = 0.95, detail = F, sex = "both", verbose = F)

# Time spent on generating point estimate cohort reconstruction.
cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = F, last_month = 12, iter = 10,
  level = 0.95, detail = F, sex = "both", verbose = F) |> system.time()
#>    user  system elapsed 
#>   0.891   0.018   0.924

# Viewing the 1998 brood year age 3 cohort in June.
result[result$by == 2006 & result$age == 3 & result$month == 6] |> unlist()
#>              by             age           month ocean_abundance 
#>      2006.00000         3.00000         6.00000        15.33229

result = cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = T, last_month = 12, iter = 1000,
  level = 0.95, detail = T, sex = "both", verbose = F)

# Viewing the bootstrapped summary of 1998 brood year age 3 cohort in June.
result[["2006"]][["3"]][["6"]][["summary"]]
#>                 median   sd CrI_low CrI_high
#> ocean_abundance  14.94 3.46   10.66    22.21
#> impact            0.00 0.00    0.00     0.00
#> maturation        0.00 0.00    0.00     0.00
#> natural_mort      0.27 0.06    0.20     0.41

# Time spent on generating bootstrapped cohort reconstruction with 1000 iterations.
cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = T, last_month = 12, iter = 1000,
  level = 0.95, detail = T, sex = "both", verbose = F) |> system.time()
#>    user  system elapsed 
#>   2.992   0.132   3.149
```

## More Information

You can find a more detailed tutorial at

## Getting Help

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/J-Ch-n/cwtCensus/issues).
