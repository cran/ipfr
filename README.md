<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/ipfr)](https://cran.r-project.org/package=ipfr)
[![Travis build status](https://travis-ci.org/dkyleward/ipfr.svg?branch=master)](https://travis-ci.org/dkyleward/ipfr)
[![Codecov test coverage](https://codecov.io/gh/dkyleward/ipfr/branch/master/graph/badge.svg)](https://codecov.io/gh/dkyleward/ipfr?branch=master)
<!-- badges: end -->

# ipfr

A package for iterative proportional fitting on multiple marginal distributions
in R. The goal of this package is to make survey raking, matrix balancing, and
population synthesis easier.

## Installation
Install the latest official version from CRAN:

```r
install.packages("ipfr")
```

Install the development version of the package:

```r
library(devtools)
install_github("dkyleward/ipfr", build_vignettes = TRUE)
```

## Basic Usage

(See vignettes at the bottom for advanced topics.)

A basic matrix balance task:

```r
mtx <- matrix(data = runif(9), nrow = 3, ncol = 3)
row_targets <- c(3, 4, 5)
column_targets <- c(5, 4, 3)
result <- ipu_matrix(mtx, row_targets, column_targets)
```

A basic survey balance task:

```r
survey <- tibble(
  size = c(1, 2, 1, 1),
  autos = c(0, 2, 2, 1),
  weight = 1
)
targets <- list()
targets$size <- tibble(
  `1` = 75,
  `2` = 25
)
targets$autos <- tibble(
  `0` = 25,
  `1` = 50,
  `2` = 25
)
result <- ipu(survey, targets)
```

Creating synthetic households from the `ipu()` result:

```r
synthesize(result$weight_tbl)
```

## Vignettes
Using ipfr: https://cran.r-project.org/web/packages/ipfr/vignettes/using_ipfr.html  
Common ipf problems: https://cran.r-project.org/web/packages/ipfr/vignettes/common_ipf_problems.html
