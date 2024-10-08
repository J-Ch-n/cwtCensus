
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cwtCensus

<!-- badges: start -->

[![R-CMD-check](https://github.com/J-Ch-n/cohort_reco_pk/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/J-Ch-n/cohort_reco_pk/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/J-Ch-n/cohort_reco_pk/graph/badge.svg)](https://app.codecov.io/gh/J-Ch-n/cohort_reco_pk)
<!-- badges: end -->

The goal of cwtCensus is to provide a fast and memory efficient tool for
cohort reconstruction using point and bootstrapped estimates. Writing
efficient code for cohort reconstruction can be challenging, so we
developed cwtCensus to abstract away the details of tweaking lines of R
and designing algorithms. With this package, you can generate
bootstrapped and non-bootstrapped cohort reconstructions with just one
click!

## Installation

You can install the latest stable version of cwtCensus from CRAN with:

``` r
install.packages("cwtCensus")
```

You can install the development version of cwtCensus from
[GitHub](https://github.com/J-Ch-n/cwtCensus/tree/main) with:

``` r
install.packages("devtools")
devtools::install_github("J-Ch-n/cwtCensus")
```

## Example

This is a basic example showing you how to solve a common cohort
reconstruction problem:

``` r
library(cwtCensus)
set.seed(998784)
```

Creating a cohort reconstruction with point estimates:

``` r
result = cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = FALSE, last_month = 12L, iter = 10L,
  level = 0.95, detail = FALSE, sex = "both", verbose = FALSE)
```

Time spent on generating point estimate cohort reconstruction:

``` r
cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = FALSE, last_month = 12L, iter = 10L,
  level = 0.95, detail = FALSE, sex = "both", verbose = FALSE) |> system.time()
#>    user  system elapsed 
#>   0.314   0.008   0.324
```

Viewing the 1998 brood year age 3 cohort in June:

``` r
result[result$by == 2007 & result$age == 3 & result$month == 6] |> unlist()
#>              by             age           month ocean_abundance 
#>        2007.000           3.000           6.000        7965.572
```

Creating a bootstrapped cohort reconstruction with 1000 iterations:

``` r
result = cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = TRUE, last_month = 12L, iter = 1000L,
  level = 0.95, detail = TRUE, sex = "both", verbose = FALSE)
```

Viewing the bootstrapped summary of 2007 brood year age 3 cohort in
June:

``` r
result[["2007"]][["3"]][["6"]][["summary"]]
#>                    median        sd   CrI_low  CrI_high
#> ocean_abundance 7927.9520 279.29474 7443.6070 8549.7916
#> impact             0.0000   0.00000    0.0000    0.0000
#> maturation         0.0000   0.00000    0.0000    0.0000
#> natural_mort     146.0604   5.14558  137.1371  157.5168
```

Time spent on generating bootstrapped cohort reconstruction with 1000
iterations:

``` r
cohort_reconstruct(release, recovery, birth_month = 4L,
  bootstrap = TRUE, last_month = 12L, iter = 1000L,
  level = 0.95, detail = TRUE, sex = "both", verbose = FALSE) |> system.time()
#>    user  system elapsed 
#>  23.442   0.189  23.671
```

## More Information

You can find a more detailed tutorial at
<https://J-Ch-n.github.io/cwtCensus>.

## Getting Help

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/J-Ch-n/cwtCensus/issues).
