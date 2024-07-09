<!-- README.md is generated from README.Rmd. Please edit that file -->

# cwtCensus

<!-- badges: start -->

<!-- badges: end -->

The goal of cwtCensus is to …

## Installation

You can install the development version of cwtCensus from [GitHub](https://github.com/J-Ch-n/cwtCensus/tree/main) with:

``` r
install.packages("devtools")
devtools::install_github("J-Ch-n/cohort_reco_pk")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cwtCensus)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

result = cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = F, last_month = 12, iter = 10,
  level = 0.95, detail = F, sex = "both", verbose = F)

# Viewing the 1998 brood year age 3 cohort in June.l
result |> filter(by == 2006, age == 3, month == 6) |> unlist()
#>              by             age           month ocean_abundance 
#>      2006.00000         3.00000         6.00000        15.33229

result = cohort_reconstruct(release, recovery, birth_month = 4,
  bootstrap = T, last_month = 12, iter = 1000,
  level = 0.95, detail = T, sex = "both", verbose = F)

# Viewing the bootstrapped summary of 1998 brood year age 3 cohort in June.
result[["2006"]][["3"]][["6"]][["summary"]]
#>                 median   sd CrI_low CrI_high
#> ocean_abundance  14.92 3.32   10.66    21.35
#> impact            0.00 0.00    0.00     0.00
#> maturation        0.00 0.00    0.00     0.00
#> natural_mort      0.27 0.06    0.20     0.39
```

## More Information

You can find a more detailed tutorial at (Link)

## Getting Help

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/J-Ch-n/cwtCensus/issues).
