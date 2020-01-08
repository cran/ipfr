## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ipfr)
  library(dplyr)
})

## ---- warning=TRUE------------------------------------------------------------
hh_seed <- tibble(
  geo_region = 1,
  id = c(1:8),
  hhsiz = c(1, 1, 1, 2, 2, 2, 2, 2),
  hhveh = c(0, 2, 1, 1, 1, 2, 1, 0)
)

hh_targets <- list()
hh_targets$hhsiz <- tibble(
  geo_region = 1,
  `1` = 35,
  `2` = 65
)
hh_targets$hhveh <- tibble(
  geo_region = 1,
  `0` = 100,
  `1` = 100,
  `2` = 100
)

result <- ipu(hh_seed, hh_targets, max_iterations = 30, verbose = TRUE)

## -----------------------------------------------------------------------------
result$primary_comp

## ----balance example inputs---------------------------------------------------
result <- setup_arizona()
hh_seed <- result$hh_seed
hh_targets <- result$hh_targets
per_seed <- result$per_seed
per_targets <- result$per_targets

avg_hh_weight <- (rowSums(hh_targets$hhtype) - 1) / nrow(hh_seed)
avg_per_weight <- (rowSums(per_targets$pertype) - 1) / nrow(per_seed)

## -----------------------------------------------------------------------------
new_per_targets <- per_targets
new_per_targets$pertype <- per_targets$pertype %>%
  mutate_at(
    .vars = vars(`1`, `2`, `3`),
    .funs = list(~. * 2)
  )

result <- ipu(hh_seed, hh_targets, per_seed, new_per_targets, max_iterations = 30)

## -----------------------------------------------------------------------------
result$weight_dist

## -----------------------------------------------------------------------------
result_80 <- ipu(
  hh_seed, hh_targets, per_seed, new_per_targets,
  max_iterations = 30,
  secondary_importance = .80
)

result_80$weight_dist
result_80$primary_comp
result_80$secondary_comp

## -----------------------------------------------------------------------------
result_20 <- ipu(
  hh_seed, hh_targets, per_seed, new_per_targets,
  max_iterations = 30,
  secondary_importance = .20
)

result_20$weight_dist
result_20$primary_comp
result_20$secondary_comp

## -----------------------------------------------------------------------------
hh_seed <- tibble(
  id = c(1, 2, 3, 4),
  siz = c(1, 2, 2, 1),
  weight = c(1, 1, 1, 1),
  geo_cluster = c(1, 1, 2, 2)
)

hh_targets <- list()
hh_targets$siz <- tibble(
  geo_cluster = c(1, 2),
  `1` = c(75, 100),
  `2` = c(25, 150)
)

result <- ipu(hh_seed, hh_targets, max_iterations = 10,
              max_ratio = 1.2, min_ratio = .8)

## -----------------------------------------------------------------------------
result$weight_dist

## -----------------------------------------------------------------------------
result$primary_comp

## -----------------------------------------------------------------------------
hh_targets <- list()
hh_targets$siz <- tibble(
  geo_cluster = c(1, 2),
  `1` = c(100000, 100),
  `2` = c(10, 150)
)

result <- ipu(hh_seed, hh_targets, max_iterations = 10,
              max_ratio = 5, min_ratio = .2)

result$weight_tbl

result$primary_comp

