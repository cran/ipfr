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

## -----------------------------------------------------------------------------
set.seed(42)
mtx <- matrix(data = runif(9), nrow = 3, ncol = 3)
row_targets <- c(3, 4, 5)
column_targets <- c(5, 4, 3)
mtx

## -----------------------------------------------------------------------------
result <- ipu_matrix(mtx, row_targets, column_targets)
result
rowSums(result)
colSums(result)

## -----------------------------------------------------------------------------
survey <- tibble(
  size = c(1, 2, 1, 1),
  autos = c(0, 2, 2, 1),
  weight = 1
)

survey

## -----------------------------------------------------------------------------
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

targets

## -----------------------------------------------------------------------------
result <- ipu(survey, targets)
names(result)

## -----------------------------------------------------------------------------
result$weight_tbl

## -----------------------------------------------------------------------------
result$weight_dist

## -----------------------------------------------------------------------------
result$primary_comp

## ----arizona inputs-----------------------------------------------------------
result <- setup_arizona()
hh_seed <- result$hh_seed
hh_targets <- result$hh_targets
per_seed <- result$per_seed
per_targets <- result$per_targets

## ----arizona ipu--------------------------------------------------------------
result <- ipu(hh_seed, hh_targets, per_seed, per_targets, max_iterations = 30)

## ----arizona results----------------------------------------------------------
result$weight_tbl
  
result$primary_comp

result$secondary_comp

## ----multigeo inputs----------------------------------------------------------
# Repeat the hh_seed to create tract 1 and 2 households
new_hh_seed <- hh_seed %>%
  mutate(geo_tract = 1)
new_hh_seed <- bind_rows(
  new_hh_seed,
  new_hh_seed %>% 
    mutate(geo_tract = 2, id = id + 8)
)
new_hh_seed$geo_region <- 1

new_hh_seed

# Repeat the household targets for two tracts
new_hh_targets <- hh_targets
new_hh_targets$hhtype <- bind_rows(hh_targets$hhtype, hh_targets$hhtype)
new_hh_targets$hhtype <- new_hh_targets$hhtype %>%
  mutate(geo_tract = c(1, 2))

new_hh_targets$hhtype

# Repeat the per_seed to create tract 1 and 2 persons
new_per_seed <- bind_rows(
  per_seed,
  per_seed %>% 
    mutate(id = id + 8)
)

new_per_seed

# Double the regional person targets
new_per_targets <- per_targets
new_per_targets$pertype <- per_targets$pertype %>%
  mutate_all(list(~. * 2)) %>%
  mutate(geo_region = 1)

new_per_targets$pertype

## ----multigeo ipu-------------------------------------------------------------
result <- ipu(
  new_hh_seed, new_hh_targets,
  new_per_seed, new_per_targets,
  max_iterations = 30
)

## ----multigeo results---------------------------------------------------------
result$primary_comp

result$secondary_comp

## -----------------------------------------------------------------------------
set.seed(42)
synthesize(result$weight_tbl, group_by = "geo_tract") %>%
  head()

## -----------------------------------------------------------------------------
set.seed(42)
result$weight_tbl %>%
  group_by(geo_tract) %>%
  synthesize() %>%
  head()

