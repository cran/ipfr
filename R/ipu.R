#' Iterative Proportional Updating
#' 
#' @description A general case of iterative proportional fitting. It can satisfy
#'   two, disparate sets of marginals that do not agree on a single total. A
#'   common example is balancing population data using household- and person-level
#'   marginal controls. This could be for survey expansion or synthetic
#'   population creation. The second set of marginal/seed data is optional, meaning
#'   it can also be used for more basic IPF tasks.
#' 
#' @references \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.537.723&rep=rep1&type=pdf}
#' 
#' @param primary_seed In population synthesis or household survey expansion, 
#'   this would be the household seed table (each record would represent a 
#'   household). It could also be a trip table, where each row represents an 
#'   origin-destination pair.
#' @param primary_targets A \code{named list} of data frames.  Each name in the
#'   list defines a marginal dimension and must match a column from the
#'   \code{primary_seed} table. The data frame associated with each named list
#'   element can contain a geography field (starting with "geo_"). If so, each
#'   row in the target table defines a new geography (these could be TAZs,
#'   tracts, clusters, etc.). The other column names define the marginal
#'   categories that targets are provided for. The vignette provides more
#'   detail.
#' @param primary_id The field used to join the primary and secondary seed
#'   tables. Only necessary if \code{secondary_seed} is provided.
#' @param secondary_seed Most commonly, if the primary_seed describes
#'   households, the secondary seed table would describe the persons in each
#'   household. Must contain the same \code{primary_id} column that links each
#'   person to their respective household in \code{primary_seed}.
#' @param secondary_targets Same format as \code{primary_targets}, but they constrain 
#'   the \code{secondary_seed} table.
#' @param secondary_importance A \code{real} between 0 and 1 signifying the 
#'   importance of the secondary targets. At an importance of 1, the function
#'   will try to match the secondary targets exactly. At 0, only the percentage
#'   distributions are used (see the vignette section "Target Agreement".)
#' @param relative_gap After each iteration, the weights are compared to the
#' previous weights and the %RMSE is calculated. If the %RMSE is less than
#' the \code{relative_gap} threshold, then the process terminates.
#' @param max_iterations maximum number of iterations to perform, even if 
#'    \code{relative_gap} is not reached.
#' @param absolute_diff Upon completion, the \code{ipu()} function will report
#'   the worst-performing marginal category and geography based on the percent
#'   difference from the target. \code{absolute_diff} is a threshold below which
#'   percent differences don't matter.
#'   
#'   For example, if if a target value was 2, and the expanded weights equaled
#'   1, that's a 100% difference, but is not important because the absolute value
#'   is only 1.
#'   
#'   Defaults to 10.
#' @param weight_floor Minimum weight to allow in any cell to prevent zero
#'   weights. Set to .0001 by default.  Should be arbitrarily small compared to
#'   your seed table weights.
#' @param verbose Print iteration details and worst marginal stats upon 
#'   completion? Default \code{FALSE}.
#' @param max_ratio \code{real} number. The average weight per seed record is
#' calculated by dividing the total of the targets by the number of records.
#' The max_scale caps the maximum weight at a multiple of that average. Defaults
#' to \code{10000} (basically turned off).
#' @param min_ratio \code{real} number. The average weight per seed record is
#' calculated by dividing the total of the targets by the number of records.
#' The min_scale caps the minimum weight at a multiple of that average. Defaults
#' to \code{0.0001} (basically turned off).
#' @return a \code{named list} with the \code{primary_seed} with weight, a 
#'   histogram of the weight distribution, and two comparison tables to aid in
#'   reporting.
#' @export
#' @examples
#' hh_seed <- dplyr::tibble(
#'   id = c(1, 2, 3, 4),
#'   siz = c(1, 2, 2, 1),
#'   weight = c(1, 1, 1, 1),
#'   geo_cluster = c(1, 1, 2, 2)
#' )
#' 
#' hh_targets <- list()
#' hh_targets$siz <- dplyr::tibble(
#'   geo_cluster = c(1, 2),
#'   `1` = c(75, 100),
#'   `2` = c(25, 150)
#' )
#' 
#' result <- ipu(hh_seed, hh_targets, max_iterations = 5)

ipu <- function(primary_seed, primary_targets, 
                secondary_seed = NULL, secondary_targets = NULL,
                primary_id = "id",
                secondary_importance = 1,
                relative_gap = 0.01, max_iterations = 100, absolute_diff = 10,
                weight_floor = .00001, verbose = FALSE,
                max_ratio = 10000, min_ratio = .0001){

  # If person data is provided, both seed and targets must be
  if (xor(!is.null(secondary_seed), !is.null(secondary_targets))) {
    stop("You provided either secondary_seed or secondary_targets, but not both.") # nocov
  }
  
  # Check for valid values of secondary_importance.
  if (secondary_importance > 1 | secondary_importance < 0) {
    stop("`secondary_importance` argument must be between 0 and 1") # nocov
  }
  
  # Check hh and person tables
  if (!is.null(secondary_seed)) {
    result <- check_tables(
      primary_seed, primary_targets, primary_id = primary_id,
      secondary_seed, secondary_targets
    )
  } else {
    result <- check_tables(
      primary_seed, primary_targets, primary_id = primary_id)
  }
  primary_seed <- result[[1]]
  primary_targets <- result[[2]]
  secondary_seed <- result[[3]]
  secondary_targets <- result[[4]]
  
  # Scale target tables. 
  # All tables in the list will match the totals of the first table.
  primary_targets <- scale_targets(primary_targets, verbose)
  if (!is.null(secondary_seed)) {
    secondary_targets <- scale_targets(secondary_targets, verbose) 
  }
  
  # Balance secondary targets to primary.
  if (secondary_importance != 1 & !is.null(secondary_seed)){
    if (verbose) {message("Balancing secondary targets to primary")}
    secondary_targets_mod <- balance_secondary_targets(
      primary_targets, primary_seed, secondary_targets, secondary_seed,
      secondary_importance, primary_id
    )
  } else {
    secondary_targets_mod <- secondary_targets
  }
  
  # Pull off the geo information into a separate equivalency table
  # to be used as needed.
  geo_equiv <- primary_seed %>%
    dplyr::select(dplyr::starts_with("geo_"), primary_id, "weight")
  primary_seed_mod <- primary_seed %>%
    dplyr::select(-dplyr::starts_with("geo_"))

  # Remove any fields that aren't in the target list and change the ones
  # that are to factors.
  col_names <- names(primary_targets)
  primary_seed_mod <- primary_seed_mod %>%
    # Keep only the fields of interest (marginal columns and id)
    dplyr::select(dplyr::one_of(c(col_names, primary_id))) %>%
    # Convert to factors and then to dummy columns if the column has more
    # than one category.
    dplyr::mutate_at(
      .vars = col_names,
      .funs = list(~as.factor(.))
    )
  # If one of the columns has only one value, it cannot be a factor. The name
  # must also be changed to match what the rest will be after one-hot encoding.
  for (name in col_names){
    if (length(unique(primary_seed_mod[[name]])) == 1) {
      # unfactor
      primary_seed_mod[[name]] <- type.convert(as.character(primary_seed_mod[[name]]))
      # change name
      value = primary_seed_mod[[name]][1]
      new_name <- paste0(name, ".", value)
      names(primary_seed_mod)[names(primary_seed_mod) == name] <- new_name
    }
  }
  # Use one-hot encoding to convert the remaining factor fields to dummies
  primary_seed_mod <- primary_seed_mod %>%
    mlr::createDummyFeatures()
  
  if (!is.null(secondary_seed)) {
    # Modify the person seed table the same way, but sum by primary ID
    col_names <- names(secondary_targets_mod)
    secondary_seed_mod <- secondary_seed %>%
      # Keep only the fields of interest
      dplyr::select(dplyr::one_of(col_names), primary_id) %>%
      dplyr::mutate_at(
        .vars = col_names,
        .funs = list(~as.factor(.))
      ) %>%
      mlr::createDummyFeatures() %>%
      dplyr::group_by(!!as.name(primary_id)) %>%
      dplyr::summarize_all(
        .funs = sum
      )
    
    # combine the hh and per seed tables into a single table
    seed <- primary_seed_mod %>%
      dplyr::left_join(secondary_seed_mod, by = primary_id)
  } else {
    seed <- primary_seed_mod
  }
  
  # Add the geo information back.
  seed <- seed %>%
    dplyr::left_join(geo_equiv, by = primary_id)
  
  # store a vector of attribute column names to loop over later.
  # don't include primary_id or 'weight' in the vector.
  geo_pos <- grep("geo_", colnames(seed))
  id_pos <- grep(primary_id, colnames(seed))
  weight_pos <- grep("weight", colnames(seed))
  seed_attribute_cols <- colnames(seed)[-c(geo_pos, id_pos, weight_pos)]
  
  # modify the targets to match the new seed column names and
  # join them to the seed table
  if (!is.null(secondary_seed)) {
    targets <- c(primary_targets, secondary_targets_mod)
  } else {
    targets <- primary_targets
  }
  for (name in names(targets)) {
    # targets[[name]] <- targets[[name]] %>%
    temp <- targets[[name]] %>%
      tidyr::gather(key = "key", value = "target", -dplyr::starts_with("geo_")) %>%
      dplyr::mutate(key = paste0(!!name, ".", key, ".target")) %>%
      tidyr::spread(key = key, value = target)
    
    # Get the name of the geo column
    pos <- grep("geo_", colnames(temp))
    geo_colname <- colnames(temp)[pos]
    
    seed <- seed %>%
      dplyr::left_join(temp, by = geo_colname)
  }
  
  # Calculate average, min, and max weights and join to seed. If there are
  # multiple geographies in the first primary target table, then min and max
  # weights will vary by geography.
  pos <- grep("geo_", colnames(targets[[1]]))
  geo_colname <- colnames(targets[[1]])[pos]
  recs_by_geo <- seed %>%
    dplyr::group_by(!!as.name(geo_colname)) %>%
    dplyr::summarize(count = n())
  weight_scale <- targets[[1]] %>%
    tidyr::gather(key = category, value = total, -!!as.name(geo_colname)) %>%
    dplyr::group_by(!!as.name(geo_colname)) %>%
    dplyr::summarize(total = sum(total)) %>% 
    dplyr::left_join(recs_by_geo, by = geo_colname) %>%
    dplyr::mutate(
      avg_weight = total / count,
      min_weight = (!!min_ratio) * avg_weight,
      max_weight = (!!max_ratio) * avg_weight
    ) 
  seed <- seed %>%
    dplyr::left_join(weight_scale, by = geo_colname)
  
  iter <- 1
  converged <- FALSE
  while (!converged & iter <= max_iterations) {
    # Loop over each target and upate weights
    for (seed_attribute in seed_attribute_cols) {
      
      # Create lookups for targets list
      target_tbl_name <- strsplit(seed_attribute, ".", fixed = TRUE)[[1]][1]
      target_name <- paste0(seed_attribute, ".", "target")
      
      # Get the name of the geo column
      target_tbl <- targets[[target_tbl_name]]
      pos <- grep("geo_", colnames(target_tbl))
      geo_colname <- colnames(target_tbl)[pos]

      # Adjust weights
      seed <- seed %>%
        dplyr::mutate(
          geo = !!as.name(geo_colname),
          attr = !!as.name(seed_attribute),
          target = !!as.name(target_name)
        ) %>%
        dplyr::group_by(geo) %>%
        dplyr::mutate(
          total_weight = sum(attr * weight),
          factor = ifelse(attr > 0, target / total_weight, 1),
          weight = weight * factor,
          # Implement the floor on zero weights
          weight = pmax(weight, weight_floor),
          # Cap weights to to multiples of the average weight.
          # Not applicable if target is 0.
          weight = ifelse(attr > 0 & target > 0, pmax(min_weight, weight), weight),
          weight = ifelse(attr > 0 & target > 0, pmin(max_weight, weight), weight)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-geo, -attr, -target, -factor)
    }
    
    # Determine percent differences (by geo field)
    saved_diff_tbl <- NULL
    pct_diff <- 0
    for (seed_attribute in seed_attribute_cols) {
      # create lookups for targets list
      target_tbl_name <- strsplit(seed_attribute, ".", fixed = TRUE)[[1]][1]
      target_name <- paste0(seed_attribute, ".", "target")
      target_tbl <- targets[[target_tbl_name]]
      
      # Get the name of the geo column
      pos <- grep("geo_", colnames(target_tbl))
      geo_colname <- colnames(target_tbl)[pos]
  
      diff_tbl <- seed %>%
        dplyr::filter((!!as.name(seed_attribute)) > 0) %>%
        dplyr::select(
          geo = !!geo_colname, primary_id, attr = !!seed_attribute, weight,
          target = !!target_name
        ) %>%
        dplyr::group_by(geo) %>%
        dplyr::mutate(
          total_weight = sum(attr * weight),
          diff = total_weight - target,
          abs_diff = abs(diff),
          pct_diff = diff / (target + .0000001) # avoid dividing by zero
        ) %>%
        # Removes rows where the absolute gap is smaller than 'absolute_diff'
        dplyr::filter(abs_diff > absolute_diff) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      
      # If any records are left in the diff_tbl, record worst percent difference 
      # and save that percent difference table for reporting.
      if (nrow(diff_tbl) > 0) {
        if (max(abs(diff_tbl$pct_diff)) > pct_diff) {
          pct_diff <- max(abs(diff_tbl$pct_diff))
          saved_diff_tbl <- diff_tbl
          saved_category <- seed_attribute
          saved_geo <- geo_colname
        }
      }
      
    }
    
    # Test for convergence
    if (iter > 1) {
      rmse <- mlr::measureRMSE(prev_weights, seed$weight)
      pct_rmse <- rmse / mean(prev_weights) * 100
      converged <- ifelse(pct_rmse <= relative_gap, TRUE, FALSE)
      if(verbose){
        cat("\r Finished iteration ", iter, ". %RMSE = ", pct_rmse)
      }
    }
    prev_weights <- seed$weight
    iter <- iter + 1
  }

  if (verbose) {
    message(ifelse(converged, "\nIPU converged", "\nIPU did not converge"))
    if (is.null(saved_diff_tbl)) {
      message("All targets matched within the absolute_diff of ", absolute_diff)
    } else {
      message("Worst marginal stats:")
      position <- which(abs(saved_diff_tbl$pct_diff) == pct_diff)[1]
      message("Category: ", saved_category)
      message(saved_geo, ": ", saved_diff_tbl$geo[position])
      message("Worst % Diff: ", round(
        saved_diff_tbl$pct_diff[position] * 100, 2), "%"
      )
      message("Difference: ", round(saved_diff_tbl$diff[position], 2))
    }
    utils::flush.console()
  }

  # Set final weights into primary seed table. Also include average weight
  # and distribution info.
  primary_seed$weight <- seed$weight
  primary_seed$avg_weight <- seed$avg_weight
  primary_seed$weight_factor <- primary_seed$weight / primary_seed$avg_weight
  
  # If the average weight is 0 (meaning the target was 0) set weight
  # and weight factor to 0.
  primary_seed <- primary_seed %>%
    mutate(
      weight = ifelse(avg_weight == 0, 0, weight),
      weight_factor = ifelse(avg_weight == 0, 0, weight_factor)
    )
  
  # Create the result list (what will be returned). Add the seed table and a
  # histogram of weight distribution.
  result <- list()
  result$weight_tbl <- primary_seed
  result$weight_tbl$geo_all <- NULL
  result$weight_dist <- ggplot2::ggplot(
    data = primary_seed, ggplot2::aes(primary_seed$weight_factor)
  ) +
    ggplot2::geom_histogram(bins = 10, fill = "darkblue", color = "gray") +
    ggplot2::labs(
      x = "Weight Ratio = Weight / Average Weight", y = "Count of Seed Records"
    )
  
  # Compare resulting weights to initial targets
  primary_comp <- compare_results(primary_seed, primary_targets)
  result$primary_comp <- primary_comp
  if (!is.null(secondary_seed)) {
    # Add geo fields to secondary seed
    pos <- grep("geo_", colnames(primary_seed))
    geo_cols <- colnames(primary_seed)[pos]
    seed <- secondary_seed %>%
      dplyr::left_join(
        primary_seed %>%
          dplyr::select(dplyr::one_of(geo_cols), primary_id, weight),
        by = primary_id
      )
    
    # Run the comparison against the original, unscaled targets 
    # and store in 'result'
    secondary_comp <- compare_results(
      seed, 
      secondary_targets
    )
    result$secondary_comp <- secondary_comp
  }
  
  return(result)
}

#' Check seed and target tables for completeness
#' 
#' @description Given seed and targets, checks to make sure that at least one
#'   observation of each marginal category exists in the seed table.  Otherwise,
#'   ipf/ipu would produce wrong answers without throwing errors.
#'
#' @inheritParams ipu
#' @return both seed tables and target lists
#' @keywords internal

check_tables <- function(primary_seed, primary_targets, 
                         secondary_seed = NULL, secondary_targets = NULL,
                         primary_id){

  # If person data is provided, both seed and targets must be
  if (xor(!is.null(secondary_seed), !is.null(secondary_targets))) {
    stop("You provided either secondary_seed or secondary_targets, but not both.") # nocov
  }
  
  ## Primary checks ##
  
  # Check that there are no NA values in seed or targets
  if (any(is.na(unlist(primary_seed)))) {
    stop("primary_seed table contains NAs") # nocov
  }
  if (any(is.na(unlist(primary_targets)))) {
    stop("primary_targets table contains NAs") # nocov
  }
  
  # Ensure that a weight field exists in the primary table.
  if (!"weight" %in% colnames(primary_seed)) {
    primary_seed$weight <- 1
  }
  
  # Check the primary_id
  secondary_seed_exists <- !is.null(secondary_seed)
  id_field_exists <- primary_id %in% colnames(primary_seed)
  if (!id_field_exists) {
    if (secondary_seed_exists) { # nocov start
      stop("The primary seed table does not have field, '", primary_id, "'.")
    } else {
      primary_seed[primary_id] <- seq(1, nrow(primary_seed))
    }  # nocov end
  }
  unique_ids <- unique(primary_seed[[primary_id]])
  if (length(unique_ids) != nrow(primary_seed)) {
    stop("The primary seed's ", primary_id, " field has duplicate values.") # nocov
  }
  
  # check primary target tables for correctness
  for (name in names(primary_targets)) {
    tbl <- primary_targets[[name]]

    result <- check_geo_fields(primary_seed, tbl, name)
    primary_seed <- result[[1]]
    primary_targets[[name]] <- result[[2]]
    tbl <- result[[2]]
    
    # Get the name of the geo field
    pos <- grep("geo_", colnames(tbl))
    geo_colname <- colnames(tbl)[pos]
    
    # Check that every non-zero target has at least one observation in
    # the seed table.
    check_missing_categories(primary_seed, tbl, name, geo_colname)
  }
  
  
  ## Secondary checks (if provided) ##
  
  if (secondary_seed_exists) {
    # Check for NAs
    if (any(is.na(unlist(secondary_seed)))) {
      stop("secondary_seed table contains NAs") # nocov
    }
    if (any(is.na(unlist(secondary_targets)))) {
      stop("secondary_targets table contains NAs") # nocov
    }
    
    # Check that secondary seed table has a primary_id field
    if (!primary_id %in% colnames(secondary_seed)) {
      stop("The primary seed table does not have field '", primary_id, "'.") # nocov
    }
    
    # Check that the secondary seed table does not have any geo columns
    check <- grepl("geo_", colnames(secondary_seed))
    if (any(check)) {
      stop("Do not include geo fields in the secondary_seed table (primary_seed only).") # nocov
    }
    
    # check the secondary target tables for correctness
    for (name in names(secondary_targets)) {
      tbl <- secondary_targets[[name]]
      
      result <- check_geo_fields(secondary_seed, tbl, name)
      secondary_seed <- result[[1]]
      # check_geo_fields may add a geo_all column. Make sure that is removed
      # from the secondary seed table, but that it exists on the primary.
      if ("geo_all" %in% colnames(secondary_seed)) {
        secondary_seed$geo_all <- NULL
        primary_seed$geo_all <- 1
      }
      secondary_targets[[name]] <- result[[2]]
      tbl <- result[[2]]
      
      # Get the name of the geo field
      pos <- grep("geo_", colnames(tbl))
      geo_colname <- colnames(tbl)[pos]
      
      # Add the geo field from the primary_seed before checking
      temp_seed <- secondary_seed %>%
        dplyr::left_join(
          primary_seed %>% dplyr::select(primary_id, geo_colname),
          by = primary_id
        )
      
      # Check that every non-zero target has at least one observation in
      # the seed table.
      check_missing_categories(temp_seed, tbl, name, geo_colname)
    }
  }
  
  # return seeds and targets in case of modifications
  return(list(
    primary_seed,
    primary_targets,
    secondary_seed,
    secondary_targets
  ))
}

#' Check for missing categories in seed
#' 
#' Helper function for \code{check_tables}.
#' 
#' @param seed seed table to check
#' @param target data.frame of a single target table
#' @param target_name the name of the target (e.g. size)
#' @param geo_colname the name of the geo column in both the \code{seed} and
#'   \code{target} (e.g. geo_taz)
#' @keywords internal
#' @return Nothing. Throws an error if one is found.

check_missing_categories <- function(seed, target, target_name, geo_colname) {
  
  for (geo in unique(unlist(seed[, geo_colname]))){  
    
    # Get column names for the current geo that have a >0 target
    non_zero_targets <- target[target[geo_colname] == geo,
                            colSums(target[target[geo_colname] == geo, ]) > 0]
    col_names <- colnames(non_zero_targets)
    col_names <- type.convert(col_names[!col_names == geo_colname], as.is = TRUE)
    
    test <- match(col_names, seed[[target_name]][seed[, geo_colname] == geo])
    if (any(is.na(test))) {
      prob_cat <- col_names[which(is.na(test))]
      stop(
        "Marginal ", target_name, " category ", 
        paste(prob_cat, collapse = ", "),
        " missing from ", geo_colname, " ", geo,
        " in the seed table with a target greater than zero."
      )
    }
  }
}

#' Check geo fields
#' 
#' Helper function for \code{\link{check_tables}}. Makes sure that geographies
#' in a seed and target table line up properly.
#' 
#' @inheritParams check_missing_categories
#' @return The seed and target table (which may be modified)
#' @keywords internal

check_geo_fields <- function(seed, target, target_name) {

  # Require a geo field if >1 row
  check <- grepl("geo_", colnames(target))
  if (nrow(target) > 1) {
    if (!any(check)) { # nocov start
      stop("target table '", target_name, "' has >1 row but does not have a",
           "geo column (must start with 'geo_')") # nocov end
    }
    # If the table has 1 row and no geo field, add one.
  } else {
    if (!any(check)) {
      target$geo_all <- 1
      seed$geo_all <- 1
    }
  }
  if (sum(check) > 1) {
    stop("target table '", target_name, "' has more than one geo column (starts with 'geo_'")  # nocov
  }
  
  return(list(seed, target))
}

#' Compare results to targets
#'
#' @param seed \code{data.frame} Seed table with a weight column in the same
#' format required by \code{ipu()}.
#' @param targets \code{named list} of \code{data.frames} in the same format
#' required by \code{ipu()}.
#' @return \code{data frame} comparing balanced results to targets
#' @keywords internal

compare_results <- function(seed, targets){
  
  # Expand the target tables out into a single, long-form data frame
  comparison_tbl <- NULL
  for (name in names(targets)){
    
    # Pull out the current target table
    target <- targets[[name]]
    
    # Get the name of the geo field
    pos <- grep("geo_", colnames(target))
    geo_colname <- colnames(target)[pos]
    
    # Gather the current target table into long form
    target <- target %>%
      dplyr::ungroup() %>%
      dplyr::mutate(geo = paste0(geo_colname, "_", !!as.name(geo_colname))) %>%
      dplyr::select(-dplyr::one_of(geo_colname)) %>%
      tidyr::gather(key = category, value = target, -geo) %>%
      dplyr::mutate(category = paste0(name, "_", category))
    
    # summarize the seed table
    result <- seed %>%
      dplyr::select(geo = !!as.name(geo_colname), category = !!as.name(name), weight) %>%
      dplyr::mutate(
        geo = paste0(geo_colname, "_", geo),
        category = paste0(name, "_", category)
      ) %>%
      dplyr::group_by(geo, category) %>%
      dplyr::summarize(result = sum(weight))
    
    # Join them together
    joined_tbl <- target %>%
      dplyr::left_join(result, by = c("geo" = "geo", "category" = "category"))
    
    # Append it to the master target df
    comparison_tbl <- dplyr::bind_rows(comparison_tbl, joined_tbl)
  }
  
  # Calculate difference and percent difference
  comparison_tbl <- comparison_tbl %>%
    dplyr::mutate(
      diff = result - target,
      pct_diff = round(diff / target * 100, 2),
      diff = round(diff, 2)
    ) %>%
    dplyr::arrange(geo, category) %>%
    # If the temporary geo field geo_all was created, clean it up
    dplyr::mutate(geo = gsub("geo_all.*", "geo_all", geo)) %>%
    rename(geography = geo)
  
  return(comparison_tbl)
}


#' Scale targets to ensure consistency
#' 
#' Often, different marginals may disagree on the total number of units. In the
#' context of household survey expansion, for example, one marginal might say
#' there are 100k households while another says there are 101k. This function
#' solves the problem by scaling all target tables to match the first target
#' table provided.
#' 
#' @param targets \code{named list} of \code{data.frames} in the same format
#' required by \link{ipu}. 
#' @param verbose \code{logical} Show a warning for each target scaled?
#'   Defaults to \code{FALSE}.
#' @return A \code{named list} with the scaled targets
#' @keywords internal

scale_targets <- function(targets, verbose = FALSE){
  
  for (i in c(1:length(names(targets)))) {
    name <- names(targets)[i]
    target <- targets[[name]]
    
    # Get the name of the geo field
    pos <- grep("geo_", colnames(target))
    geo_colname <- colnames(target)[pos]
    
    # calculate total of table
    target <- target %>%
      tidyr::gather(key = category, value = count, -!!geo_colname)
    total <- sum(target$count)
    
    # Start a string that will be used for the warning message if targets
    # are scaled and verbose = TRUE
    warning_msg <- "Scaling target tables: "
    
    # if first iteration, set total to the global total. Otherwise, scale table
    if (i == 1) {
      global_total <- total
      show_warning <- FALSE
    } else {
      fac <- global_total / total
      # Write out warning
      if (fac != 1 & verbose) {
        show_warning <- TRUE # nocov
        warning_msg <- paste0(warning_msg, " ", name) # nocov
      }
      target <- target %>%
        dplyr::mutate(count = count * !!fac) %>%
        tidyr::spread(key = category, value = count)
      targets[[name]] <- target
    }
  }
  
  if (show_warning) {
    message(warning_msg) # nocov
    utils::flush.console() # nocov
  }
  
  return(targets)
}

#' Balances secondary targets to primary
#' 
#' The average weight per record needed to satisfy targets is computed for both
#' primary and secondary targets. Often, these can be very different, which leads
#' to poor performance. The algorithm must use extremely large or small weights
#' to match the competing goals. The secondary targets are scaled so that they
#' are consistent with the primary targets on this measurement.
#' 
#' If multiple geographies are present in the secondary_target table, then
#' balancing is done for each geography separately.
#' 
#' @inheritParams ipu
#' @return \code{named list} of the secondary targets
#' @keywords internal

balance_secondary_targets <- function(primary_targets, primary_seed,
                                      secondary_targets, secondary_seed,
                                      secondary_importance, primary_id){

  # Extract the first table from the primary target list and geo name
  pri_target <- primary_targets[[1]]
  pos <- grep("geo_", colnames(pri_target))
  pri_geo_colname <- colnames(pri_target)[pos]

  for (name in names(secondary_targets)){
    sec_target <- secondary_targets[[name]]

    # Get geography field
    pos <- grep("geo_", colnames(sec_target))
    sec_geo_colname <- colnames(sec_target)[pos]

    # If the geographies used aren't the same, convert the primary table
    if (pri_geo_colname != sec_geo_colname) {
      pri_target <- pri_target %>%
        dplyr::left_join(
          primary_seed %>% 
            dplyr::select(!!pri_geo_colname, sec_geo_colname) %>%
            dplyr::group_by(!!as.name(pri_geo_colname)) %>%
            dplyr::slice(1),
          by = pri_geo_colname
        ) %>%
        dplyr::select(-dplyr::one_of(pri_geo_colname))
    }

    # Summarize the primary and secondary targets by geography
    pri_target <- pri_target  %>%
      tidyr::gather(key = cat, value = count, -sec_geo_colname) %>%
      dplyr::group_by(!!as.name(sec_geo_colname)) %>%
      dplyr::summarize(total = sum(count))
    sec_target <- sec_target %>%
      tidyr::gather(key = cat, value = count, -sec_geo_colname) %>%
      dplyr::group_by(!!as.name(sec_geo_colname)) %>%
      dplyr::summarize(total = sum(count))

    # Get primary and secondary record counts
    pri_rec_count <- primary_seed %>%
      dplyr::group_by(!!as.name(sec_geo_colname)) %>%
      dplyr::summarize(recs = n())
    sec_rec_count <-secondary_seed %>%
      dplyr::left_join(
        primary_seed %>% 
          dplyr::select(dplyr::one_of(c(sec_geo_colname, primary_id))),
        by = primary_id
      ) %>%
      dplyr::group_by(!!as.name(sec_geo_colname)) %>%
      dplyr::summarize(recs = n())

    # Calculate average weights and the secondary factor
    pri_rec_count$avg_weight <- pri_target$total / pri_rec_count$recs
    sec_rec_count$avg_weight <- sec_target$total / sec_rec_count$recs
    sec_rec_count$factor <- adjust_factor(
      pri_rec_count$avg_weight / sec_rec_count$avg_weight,
      # in this context, high importance means you want the final factor
      # in this table to be near 1. Must flip the importance variable.
      1 - secondary_importance
    )

    # Update the secondary targets by the factor
    secondary_targets[[name]] <- secondary_targets[[name]] %>%
      dplyr::left_join(
        sec_rec_count %>% dplyr::select(!!sec_geo_colname, factor),
        by = sec_geo_colname
      ) %>%
      dplyr::mutate_at(
        .vars = dplyr::vars(-factor, -dplyr::one_of(sec_geo_colname)),
        .funs = list(~. * factor)
      ) %>%
      dplyr::select(-factor)
  }

  return(secondary_targets)
}

#' Applies an importance weight to an ipfr factor
#' 
#' @description At lower values of importance, the factor is moved closer to 1.
#' 
#' @param factor A correction factor that is calculated using target/current.
#' @param importance A \code{real} between 0 and 1 signifying the importance of
#'   the factor. An importance of 1 does not modify the factor. An importance of
#'   0.5 would shrink the factor closer to 1.0 by 50 percent.
#' @return The adjusted factor.
#' @keywords internal

adjust_factor <- function(factor, importance){
  
  # return the same factor if importance = 1
  if (importance == 1) {return(factor)} # nocov
  
  if (importance > 1 | importance < 0) {
    stop("`importance` argument must be between 0 and 1") # nocov
  }
  
  # Otherwise, return the adjusted factor
  adjusted <- 1 - ((1 - factor) * (importance + .0001))
  return(adjusted)
}

#' Balance a matrix given row and column targets
#' 
#' This function simplifies the call to `ipu()` for the simple case of a matrix
#' and row/column targets.
#' 
#' @param mtx a \code{matrix}
#' @param row_targets a vector of targets that the row sums must match
#' @param column_targets a vector of targets that the column sums must match
#' @param ... additional arguments that are passed to `ipu()`. See
#'   \code{\link{ipu}} for details.
#' @return A \code{matrix} that matches row and column targets
#' @export
#' @examples
#' mtx <- matrix(data = runif(9), nrow = 3, ncol = 3)
#' row_targets <- c(3, 4, 5)
#' column_targets <- c(5, 4, 3)
#' ipu_matrix(mtx, row_targets, column_targets)

ipu_matrix <- function(mtx, row_targets, column_targets, ...) {
  tbl <- as.table(mtx)
  seed <- as.data.frame(tbl)
  colnames(seed) <- c("row", "col", "weight")
  seed <- seed %>%
    dplyr::mutate(
      geo_all = 1,
      id = seq(1, n())
    )
  targets <- list()
  targets$row <- data.frame(label = rownames(tbl), target = row_targets) %>%
    tidyr::spread(label, target) %>%
    dplyr::mutate(geo_all = 1)
  targets$col <- data.frame(label = colnames(tbl), target = column_targets) %>%
    tidyr::spread(label, target) %>%
    dplyr::mutate(geo_all = 1)
  ipu_result <- ipu(seed, targets, ...)
  final <- matrix(
    ipu_result$weight_tbl$weight, nrow = nrow(mtx), ncol = ncol(mtx)
  )
  rownames(final) <- rownames(mtx)
  colnames(final) <- colnames(mtx)
  return(final)
}


