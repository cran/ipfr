test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ipu_matrix works", {
  set.seed(42)
  mtx <- matrix(data = runif(9), nrow = 3, ncol = 3)
  row_targets <- c(3, 4, 5)
  column_targets <- c(5, 4, 3)
  result <- ipu_matrix(mtx, row_targets, column_targets)
  expect_equal(round(rowSums(result)[1], 4), 3)
  expect_equal(colSums(result)[3], 3)
})

test_that("basic ipu works", {
  result <- setup_arizona()
  hh_seed <- result$hh_seed
  hh_targets <- result$hh_targets
  per_seed <- result$per_seed
  per_targets <- result$per_targets
  
  result <- ipu(hh_seed, hh_targets, per_seed, per_targets, max_iterations = 30)
  expect_type(result, "list")
  expect_equal(length(result), 4)
  expect_equal(
    names(result),
    c("weight_tbl", "weight_dist", "primary_comp", "secondary_comp")
  )
  expect_equal(round(result$weight_tbl$weight[1], 3), 3.769)
  
  expect_message(ipu(
    hh_seed, hh_targets, per_seed, per_targets, max_iterations = 1,
    verbose = TRUE
  ))
  
  seed_missing_cat <- hh_seed
  seed_missing_cat$hhtype <- 1
  expect_error(
    ipu(seed_missing_cat, hh_targets),
    "Marginal hhtype category 2 missing from geo_all 1"
  )
})

test_that("single marginal targets work", {
  result <- setup_arizona()
  hh_seed <- result$hh_seed
  hh_targets <- result$hh_targets
  per_seed <- result$per_seed
  per_targets <- result$per_targets
  
  # Modify if only a regional person count is known
  per_seed <- per_seed %>%
    mutate(pertype = "any")
  per_targets$pertype <- tibble(
    any = 260
  )
  
  result <- ipu(hh_seed, hh_targets, per_seed, per_targets, max_iterations = 1)
  expect_equal(result$secondary_comp$category[[1]], "pertype_any")
})

test_that("weight constraint works", {
  result <- setup_arizona()
  hh_seed <- result$hh_seed
  hh_targets <- result$hh_targets
  per_seed <- result$per_seed
  per_targets <- result$per_targets
  
  min_ratio <- .2
  max_ratio <- 1.2
  result <- ipu(hh_seed, hh_targets, per_seed, per_targets, max_iterations = 30,
                min_ratio = min_ratio, max_ratio = max_ratio)
  expect_true(max(result$weight_tbl$weight_factor) == max_ratio)
  expect_true(min(result$weight_tbl$weight_factor) == min_ratio)
})

test_that("multiple geographies work", {
  result <- setup_arizona()
  hh_seed <- result$hh_seed
  hh_targets <- result$hh_targets
  per_seed <- result$per_seed
  per_targets <- result$per_targets
  
  result <- ipu(hh_seed, hh_targets, per_seed, per_targets,
      secondary_importance = .5, max_iterations = 10,
      verbose = TRUE)
  expect_equal(round(result$secondary_comp$pct_diff[1], 2), -.41)
  
  # This example is taken from the vignette and tests the portion of
  # balance_secondary_targets that handles multiple geographies.
  
  # Repeat the hh_seed to create cluster 1 and 2 households
  new_hh_seed <- hh_seed %>%
    mutate(geo_tract = 1)
  new_hh_seed <- bind_rows(
    new_hh_seed,
    new_hh_seed %>% 
      mutate(geo_tract = 2, id = id + 8)
  )
  new_hh_seed$geo_region = 1
  # Repeat the household targets for two clusters
  new_hh_targets <- hh_targets
  new_hh_targets$hhtype <- bind_rows(hh_targets$hhtype, hh_targets$hhtype)
  new_hh_targets$hhtype <- new_hh_targets$hhtype %>%
    mutate(geo_tract = c(1, 2))
  # Repeat the per_seed to create cluster 1 and 2 persons
  new_per_seed <- bind_rows(
    per_seed,
    per_seed %>% 
      mutate(id = id + 8)
  )
  # Double the regional person targets
  new_per_targets <- per_targets
  new_per_targets$pertype <- per_targets$pertype %>%
    mutate_all(list(~. * 2)) %>%
    mutate(geo_region = 1)
  result <- ipu(
    new_hh_seed, new_hh_targets,
    new_per_seed, new_per_targets,
    max_iterations = 10,
    secondary_importance = .5
  )
  expect_equal(round(result$weight_tbl$weight[1], 2), 8.03)
  
  set.seed(42)
  synth_hh <- synthesize(result$weight_tbl, group_by = "geo_tract")
  expect_equal(nrow(synth_hh), 186)
  expect_equal(synth_hh$id[1:5], c(8, 8, 2, 6, 7))
})

test_that("single value marginals work", {
  result <- setup_arizona()
  hh_seed <- result$hh_seed
  hh_targets <- result$hh_targets
  # per_seed <- result$per_seed
  # per_targets <- result$per_targets
  
  hh_seed$hhtype <- 1
  hh_targets$hhtype$`2` <- NULL
  result <- ipu(hh_seed, hh_targets)
  expect_type(ipu(hh_seed, hh_targets), "list")
})