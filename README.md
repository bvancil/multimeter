
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multimeter

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/multimeter)](https://CRAN.R-project.org/package=multimeter)
[![Codecov test
coverage](https://codecov.io/gh/bvancil/multimeter/branch/main/graph/badge.svg)](https://codecov.io/gh/bvancil/multimeter?branch=main)
[![R-CMD-check](https://github.com/bvancil/multimeter/workflows/R-CMD-check/badge.svg)](https://github.com/bvancil/multimeter/actions)
<!-- badges: end -->

Inspect data pipelines (especially magrittr pipelines) for before-after
changes during tricky operations.

## Installation

You can install the latest development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bvancil/multimeter")
```

## Example

The basic `Multimeter` class powers the `mm_get_*_meter()` functions,
which are easier to use.

``` r
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.0.5
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(multimeter)
```

### Monitoring missingness during operations

``` r
# Function to intentionally introduce some NAs
maybe_na <- function(x, prob = 0.2) {
  n <- base::length(x)
  missing_of_type_x <- vctrs::vec_cast(NA, to = vctrs::vec_ptype(x))
  dplyr::if_else(stats::runif(n) < prob, missing_of_type_x, x)
}

missing_meter <- multimeter::mm_get_missing_meter()
set.seed(2021L + 06L + 28L)
starwars2 <- dplyr::starwars %>%
  missing_meter$probe() %>% 
  dplyr::mutate(homeworld = maybe_na(homeworld)) %>% 
  missing_meter$probe()
#> # A tibble: 1 x 3
#>   column    frac_missing_before frac_missing_after
#>   <chr>                   <dbl>              <dbl>
#> 1 homeworld               0.115              0.241
```

You can access this comparison through

``` r
missing_meter$comparison
#> # A tibble: 1 x 3
#>   column    frac_missing_before frac_missing_after
#>   <chr>                   <dbl>              <dbl>
#> 1 homeworld               0.115              0.241
```

### Summarizing changes in specific columns

``` r
value_meter <- multimeter::mm_get_value_meter()
mtcars2 <- mtcars %>%
  value_meter$probe(cyl) %>%
  dplyr::mutate(square_error_from_five = (cyl - 5)^2) %>%
  value_meter$probe(square_error_from_five)
#>   cyl square_error_from_five  n
#> 1   4                      1 11
#> 2   6                      1  7
#> 3   8                      9 14
```

You can access a summary of changed column values and frequency counts
through

``` r
value_meter$comparison
#>   cyl square_error_from_five  n
#> 1   4                      1 11
#> 2   6                      1  7
#> 3   8                      9 14
```

It would be nice to do this automatically, perhaps by decorating
`dplyr::mutate`.

## Contributing

Run to install development packages:

``` r
renv::restore()
```

Run before committing:

``` r
source("R/dev/before_commit.R")
```

## Code of Conduct

Please note that the multimeter project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
