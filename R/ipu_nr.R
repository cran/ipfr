#' Iterative Proportional Updating (Newton-Raphson)
#' 
#' @description List balancing similar to \code{\link{ipu}}, but using the
#'   Newton-Raphson approach to optimization. Created primarily as a point of
#'   comparison for \code{ipu}.
#' 
#' @param primary_seed In population synthesis or household survey expansion, 
#'   this would be the household seed table (each record would represent a 
#'   household). It could also be a trip table, where each row represents an 
#'   origin-destination pair. Must contain a \code{pid} ("primary ID") field
#'   that is unique for each row. Must also contain a geography field that
#'   starts with "geo_".
#' 
#' @param primary_targets A \code{named list} of data frames.  Each name in the 
#'   list defines a marginal dimension and must match a column from the 
#'   \code{primary_seed} table. The data frame associated with each named list
#'   element must contain a geography field (starts with "geo_"). Each row in
#'   the target table defines a new geography (these could be TAZs, tracts,
#'   clusters, etc.). The other column names define the marginal categories that
#'   targets are provided for. The vignette provides more detail.
#' 
#' @param secondary_seed Most commonly, if the primary_seed describes households, the 
#'   secondary seed table would describe a unique person with each row. Must
#'   also contain the \code{pid} column that links each person to their 
#'   respective household in \code{primary_seed}. Must not contain any geography
#'   fields (starting with "geo_").
#' 
#' @param secondary_targets Same format as \code{primary_targets}, but they constrain 
#'   the \code{secondary_seed} table.
#'   
#' @param target_priority This argument controls how quickly each set of 
#'   targets is relaxed. In other words: how important it is to match the target
#'   exactly. Defaults to \code{10,000,000}, which means that all targets should
#'   be matched exactly.
#' 
#' \describe{
#'   \item{\code{real}}{This priority value will be used for each target table.}
#'   \item{\code{named list}}{Each named entry must match an entry in either
#'   \code{primary_targets} or \code{secondary_targets} and have a \code{real}.
#'   This priority will be applied to that target table. Any targets not in the
#'   list will default to \code{10,000,000}.}
#'   \item{\code{data.frame}}{Column \code{target} must have values that match an
#'   entry in either \code{primary_targets} or \code{secondary_targets}. Column 
#'   \code{priority} contains the values to use for priority. Any targets not in
#'   the table will default to \code{10,000,000}.}
#' }
#' 
#' @param relative_gap After each iteration, the weights are compared to the
#' previous weights and the %RMSE is calculated. If the %RMSE is less than
#' the \code{relative_gap} threshold, then the process terminates.
#' 
#' @param max_iterations maximum number of iterations to perform, even if 
#'    \code{relative_gap} is not reached.
#'    
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
#'   
#' @param weight_floor Minimum weight to allow in any cell to prevent zero
#'   weights. Set to .0001 by default.  Should be arbitrarily small compared to
#'   your seed table weights.
#'   
#' @param verbose Print iteration details and worst marginal stats upon 
#'   completion? Default \code{FALSE}.
#'   
#' @param max_ratio \code{real} number. The average weight per seed record is
#' calculated by dividing the total of the targets by the number of records.
#' The max_scale caps the maximum weight at a multiple of that average. Defaults
#' to \code{10000} (basically turned off).
#' 
#' @param min_ratio \code{real} number. The average weight per seed record is
#' calculated by dividing the total of the targets by the number of records.
#' The min_scale caps the minimum weight at a multiple of that average. Defaults
#' to \code{0.0001} (basically turned off).
#' 
#' @return a \code{named list} with the \code{primary_seed} with weight, a 
#'   histogram of the weight distribution, and two comparison tables to aid in
#'   reporting.
#' 
#' @keywords internal

ipu_nr <- function(primary_seed, primary_targets, 
                secondary_seed = NULL, secondary_targets = NULL,
                target_priority = 10000000,
                relative_gap = 0.01, max_iterations = 100, absolute_diff = 10,
                weight_floor = .00001, verbose = FALSE,
                max_ratio = 10000, min_ratio = .0001){

  # Not updating this function fully as it was only ever meant for
  # research/comparison.
  primary_id <- "pid"
  
  # If secondary data is provided, both seed and targets must be
  if (xor(!is.null(secondary_seed), !is.null(secondary_targets))) {
    stop("You provided either secondary_seed or secondary_targets, but not both.")
  }
  
  # # Check primary and secondary tables
  # if (!is.null(secondary_seed)) {
  #   check_tables(primary_seed, primary_targets, secondary_seed, secondary_targets)
  # } else {
  #   check_tables(primary_seed, primary_targets)
  # }
  
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
  
  # Check for valid target_priority
  valid <- FALSE
  if (is.numeric(target_priority)) {valid <- TRUE}
  if (inherits(target_priority, "list")) {
    if (!is.null(names(target_priority))) {valid <- TRUE}
  }
  if (inherits(target_priority, "data.frame")) {
    if ("target" %in% names(target_priority) & "priority" %in% names(target_priority)) {
      valid <- TRUE
    }
  }
  if (!valid) {stop("'target_priority' is not valid.")}
  
  # Scale target tables. 
  # All tables in the list will match the totals of the first table.
  primary_targets <- scale_targets(primary_targets, verbose)
  if (!is.null(secondary_seed)) {
    secondary_targets <- scale_targets(secondary_targets, verbose) 
  }
  
  # Pull off the geo information into a separate equivalency table
  # to be used as needed.
  geo_equiv <- primary_seed %>%
    dplyr::select(dplyr::starts_with("geo_"), "pid")
  primary_seed_mod <- primary_seed %>%
    dplyr::select(-dplyr::starts_with("geo_"))
  
  # Remove any fields that aren't in the target list and change the ones
  # that are to factors.
  col_names <- names(primary_targets)
  primary_seed_mod <- primary_seed_mod %>%
    # Keep only the fields of interest (marginal columns and pid)
    dplyr::select(dplyr::one_of(c(col_names, "pid"))) %>%
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
    col_names <- names(secondary_targets)
    secondary_seed_mod <- secondary_seed %>%
      # Keep only the fields of interest
      dplyr::select(dplyr::one_of(c(col_names, "pid"))) %>%
      dplyr::mutate_at(
        .vars = col_names,
        .funs = list(~as.factor(.))
      ) %>%
      mlr::createDummyFeatures() %>%
      dplyr::group_by(pid) %>%
      dplyr::summarize_all(
        .funs = sum
      )
    
    # combine the hh and per seed tables into a single table
    seed <- primary_seed_mod %>%
      dplyr::left_join(secondary_seed_mod, by = "pid")
  } else {
    seed <- primary_seed_mod
  }
  
  # Add the geo information back.
  seed <- seed %>%
    dplyr::mutate(weight = 1)  %>%
    dplyr::left_join(geo_equiv, by = "pid")
  
  # store a vector of attribute column names to loop over later.
  # don't include 'pid' or 'weight' in the vector.
  geo_pos <- grep("geo_", colnames(seed))
  pid_pos <- grep("pid", colnames(seed))
  weight_pos <- grep("weight", colnames(seed))
  seed_attribute_cols <- colnames(seed)[-c(geo_pos, pid_pos, weight_pos)]
  
  # Combine primary and secondary targets (if present) into a single named list
  if (!is.null(secondary_seed)) {
    targets <- c(primary_targets, secondary_targets)
  } else {
    targets <- primary_targets
  }
  
  # modify the targets to match the new seed column names and
  # join them to the seed table. Also create a relaxation factor for each.
  for (name in names(targets)) {
    # targets[[name]] <- targets[[name]] %>%
    temp <- targets[[name]] %>%
      tidyr::gather(key = "key", value = "target", -dplyr::starts_with("geo_")) %>%
      dplyr::mutate(key = paste0(!!name, ".", key, ".target")) %>%
      tidyr::spread(key = key, value = target)
    
    rfac <- targets[[name]] %>%
      tidyr::gather(key = "key", value = "rel_fac", -dplyr::starts_with("geo_")) %>%
      dplyr::mutate(
        rel_fac = 1,
        key = paste0(!!name, ".", key, ".rel_fac")
      ) %>%
      tidyr::spread(key = key, value = rel_fac)
    
    # Get the name of the geo column
    pos <- grep("geo_", colnames(temp))
    geo_colname <- colnames(temp)[pos]
    
    seed <- seed %>%
      dplyr::left_join(temp, by = geo_colname) %>%
      dplyr::left_join(rfac, by = geo_colname)
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
  
  # Create a standardized list of named target priorities
  target_priority <- create_target_priority(target_priority, targets)
  
  iter <- 1
  converged <- FALSE
  while (!converged & iter <= max_iterations) {
    # Loop over each target and upate weights
    for (seed_attribute in seed_attribute_cols) {
      
      # Get target info
      target_tbl_name <- strsplit(seed_attribute, ".", fixed = TRUE)[[1]][1]
      target_name <- paste0(seed_attribute, ".", "target")
      priority <- target_priority[[target_tbl_name]]
      
      # Get the relaxation factor column name
      rel_fac_col <- paste0(seed_attribute, ".", "rel_fac")
      
      # Get the name of the geo column
      target_tbl <- targets[[target_tbl_name]]
      pos <- grep("geo_", colnames(target_tbl))
      geo_colname <- colnames(target_tbl)[pos]
    
      # Calculate SUMVAL and SUMVALSQ
      hhagg <- seed %>%
        dplyr::tbl_df() %>%
        dplyr::mutate(geo = !!as.name(geo_colname)) %>%
        dplyr::group_by(geo) %>%
        dplyr::summarize(
          SUMVAL = sum((!!as.name(seed_attribute)) * weight, na.rm=TRUE),
          SUMVALSQ = sum((!!as.name(seed_attribute)) * (!!as.name(seed_attribute)) * weight, na.rm = TRUE)
        ) %>%
        dplyr::select(geo, SUMVAL, SUMVALSQ)
      
      # Update weights and relaxation factors
      seed <- seed %>%
        dplyr::mutate(
          geo = !!as.name(geo_colname),
          attr = !!as.name(seed_attribute),
          target = !!as.name(target_name),
          rel_fac = !!as.name(rel_fac_col)
        ) %>%
        # Join sumval info
        dplyr::left_join(hhagg, by = "geo") %>%
        dplyr::mutate(
          factor = ifelse(
            SUMVAL > 0 & attr > 0,
            1 - ((SUMVAL - target * rel_fac) / (SUMVALSQ + target * rel_fac / priority)),
            1
          ),
          rel_fac = rel_fac * (1 / factor) ^ (1 / priority),
          # Update weights and cap to multiples of the average weight.
          # Not applicable if target is 0.
          weight = weight * factor,
          weight = ifelse(target > 0, pmax(min_weight, weight), weight),
          weight = ifelse(target > 0, pmin(max_weight, weight), weight)
        ) %>%
        dplyr::select(-SUMVAL, -SUMVALSQ)
      seed[, rel_fac_col] <- seed[, "rel_fac"]
    }
    
    seed <- seed %>%
      dplyr::select(-geo, -attr, -target, -factor, -rel_fac)
    
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
          geo = !!geo_colname, pid, attr = !!seed_attribute, weight,
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
    message(ifelse(converged, "IPU converged", "IPU did not converge"))
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
  
  # Create the result list (what will be returned). Add the seed table and a
  # histogram of weight distribution.
  result <- list()
  result$weight_tbl <- primary_seed
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
        primary_seed %>% dplyr::select(dplyr::one_of(geo_cols), pid, weight),
        by = "pid"
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

#' Create a named list of target priority levels.
#' 
#' @inheritParams ipu_nr
#' @param targets The complete list of targets (both primary and secondary)
#' @keywords internal

create_target_priority <- function(target_priority, targets){
  
  # If target_priority is a numeric value, then update with that value and 
  # return.
  if (is.numeric(target_priority)) {
    result <- targets
    for (name in names(targets)) {
      result[[name]] <- target_priority
    }
    return(result)
  }
  
  # For lists and data frames, start by creating a named list with default 
  # priority.
  default_priority <- 10000000
  result <- list()
  for (name in names(targets)) {
    result[[name]] <- default_priority
  }
  
  # If target_priority is a data frame, convert it to a list.
  if (inherits(target_priority, "data.frame")) {
    target_priority <- setNames(target_priority$priority, target_priority$target)
  }
  
  # Update result with priorities
  for (name in names(target_priority)) {
    if (!name %in% names(result)) {stop(paste(name, "not found in targets"))}
    result[[name]] <- target_priority[[name]]
  }
  
  return(result)
}


