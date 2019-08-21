<!-- badges: start -->
[![Travis build status](https://travis-ci.org/dkyleward/ipfr.svg?branch=master)](https://travis-ci.org/dkyleward/ipfr)
[![Codecov test coverage](https://codecov.io/gh/dkyleward/ipfr/branch/master/graph/badge.svg)](https://codecov.io/gh/dkyleward/ipfr?branch=master)
<!-- badges: end -->

# ipfr

A general function for conducting iterative proportional fitting on multiple
marginal distributions in R.

## Installation
Install the package using:

```r
library(devtools)
install_github("dkyleward/ipfr", build_vignettes = TRUE)
```

## Basic Usage

(More in the vignettes) 

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
