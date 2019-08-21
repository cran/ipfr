#' Re-weight a Seed Table to Marginal Controls
#' 
#' @param targets A \code{named list} of data frames.  Each name in the list 
#'   defines a marginal dimension and must match a column from the seed table. 
#'   The data frame associated with each name must start with an identical 
#'   column named \code{cluster}. Each row in the target table defines a new 
#'   cluster (these could be TAZs, tracts, districts, etc.), and every target 
#'   table must have the same number of rows/clusters. The other column names
#'   define the marginal categories that targets are provided for.
#'    
#' @param seed A \code{data frame} including a \code{weight} field and necessary
#'    columns for matching to marginal targets.
#'
#' @param relative_gap target for convergence.  Maximum percent change to allow
#'    any seed weight to move by while considering the process converged.  By 
#'    default, if no weights change by more than 1%, the process has converged.
#'    The process is said to be converged if either \code{relative_gap} or 
#'    \code{absolute_gap} parameters have been met.
#'
#' @param absolute_gap target for convergence.  Maximum absolute change to allow
#'    any seed weight to move by while considering the process converged.  By 
#'    default, if no weights change by more than 10, the process has converged.
#'    The process is said to be converged if either \code{relative_gap} or 
#'    \code{absolute_gap} parameters have been met.
#'
#' @param max_iterations maximum number of iterations to perform, even if 
#'    convergence is not reached.
#'
#' @param min_weight Minimum weight to allow in any cell to prevent zero weights.
#'    Set to .0001 by default.  Should be arbitrarily small compared to your 
#'    seed table weights.
#'   
#' @param verbose Print details on the maximum expansion factor with each 
#'    iteration? Default \code{FALSE}. 
#'   
#' @return the seed \code{data frame} with a column of weights appended for each
#'    row in the target \code{data.frames}
#' 
#' @keywords internal

ipf <- function(seed, targets,
                relative_gap = 0.01, absolute_gap = 1, max_iterations = 50,
                min_weight = .0001, verbose = FALSE){

  stop("ipf is deprecated. Use ipu() instead.")
  
  # Check check that seed and target are provided
  if (is.null(seed)) {
    stop("Seed table not provided")
  }
  if (is.null(targets)) {
    stop("Targets not provided")
  }
  
  # Check that there are no NA values in seed or targets
  if (any(is.na(unlist(seed)))) {
    stop("'seed' contains NAs")
  }
  if (any(is.na(unlist(targets)))) {
    stop("'targets' contains NAs")
  }
  
  # Check that at least one observation of each marginal category exists
  # in the seed table.  Otherwise, the process produces wrong answers without
  # throwing errors.
  for (name in names(targets)) {
    col_names <- colnames(targets[[name]])
    col_names <- type.convert(col_names[!col_names == "cluster"], as.is = TRUE)
    
    test <- match(col_names, seed[[name]])
    if (any(is.na(test))) {
      prob_cat <- col_names[which(is.na(test))]
      stop("Marginal ", name, "; category ", prob_cat[1], " is missing from seed table")
    }
  }
  
  # If the seed table includes a cluster column (assigning certain seed
  # records to specific clusters), repeat the above test to make sure that
  # each cluster has every observation.
  if ("cluster" %in% colnames(seed)){
    for (name in names(targets)) {
      col_names <- colnames(targets[[name]])
      col_names <- type.convert(col_names[!col_names == "cluster"], as.is = TRUE)
      
      for (cluster in seed$cluster){
        test <- match(col_names, seed[[name]][seed$cluster == cluster])
        if (any(is.na(test))) {
          prob_cat <- col_names[which(is.na(test))]
          stop("Marginal ", name, "; category ", prob_cat[1], " is missing from cluster ", cluster, " in the seed table.")
        }   
      }
    }
  }
  
  # Create df of totals from the first marginal table
  # (e.g. total households, persons, etc.)
  totals <- targets[[1]] %>%
    tidyr::gather(key = category, value = count, -cluster) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarize(total = sum(count))
  
  # Create a long data frame by repeating the seed table for each
  # row in the target tables. Call the first column ID to avoid potentially
  # having two cluster columns in the table.
  seed_long <- merge(totals$cluster, seed) %>%
    dplyr::rename(ID = x) %>%
    dplyr::arrange(ID)
  
  # If column 'cluster' is not present in the original seed table, it means
  # that every seed record should be repeated for every cluster. If it is
  # present, it means that specific seed records belong only to specific
  # clusters (common in household expansion). If present, filter the data frame
  # and then remove the cluster column.
  if ("cluster" %in% colnames(seed)){
    seed_long <- seed_long %>%
      dplyr::filter(ID == cluster) %>% 
      dplyr::select(-cluster)
  }
  
  # Rename the ID field to cluster
  seed_long <- seed_long %>%
    rename(cluster = ID)
 
  
  # Convert the weights into percents.  The percents will sum to 1 for each
  # cluster
  seed_long <- seed_long %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(weight = weight / sum(weight)) %>%
    dplyr::ungroup()
  
  # IPF ---
  
  iter <- 1
  converged <- FALSE
  while (!converged & iter <= max_iterations){
    
    # In the following loop, track the maximum gap and convergence
    # stats in these vectors
    rel_gap <- vector("numeric", length(targets))
    rel_id <- vector("numeric", length(targets))
    rel_cat <- vector("numeric", length(targets))
    abs_gap <- vector("numeric", length(targets))
    v_converged <- vector("logical", length(targets))
    
    # For each table in targets
    for (i in 1:length(targets)) {
      mName <- names(targets)[i]
      
      # Prepare the target table
      target <- targets[[mName]] %>%
        tidyr::gather(key = marg, value = target, -cluster) %>%
        dplyr::mutate(marg = type.convert(marg, as.is = TRUE)) %>%
        dplyr::group_by(cluster) %>%
        dplyr::mutate(target = target / sum(target)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(cluster)
      colnames(target) <- c("cluster", mName, "target")
      
      # Prepare a summary of the seed table
      seed_summary <- seed_long %>%
        dplyr::group_by_("cluster", mName) %>%
        dplyr::summarize(weight = sum(weight)) %>%
        dplyr::ungroup()
      
      # Join target to seed to calculate factor
      fac_tbl <- seed_summary %>%
        dplyr::left_join(target, by = setNames(c("cluster", mName), c("cluster", mName))) %>%
        dplyr::mutate(factor = ifelse(weight == 0, 1, target / weight))
      
      # Handle missing targets.  For example, if the seed table had size
      # categories 1-5, but the targets only had 1-4, leave the 5s alone without
      # causing an error
      fac_tbl <- fac_tbl %>%
        dplyr::mutate(factor = ifelse(is.na(target), 1, factor ))
      
      # Prepare fac_tbl for joining to seed table
      fac_tbl <- fac_tbl %>%
        dplyr::select(dplyr::one_of("cluster", mName, "factor"))
      
      # Join to the seed_long table and calculate new weight
      seed_long <- seed_long %>%
        dplyr::left_join(fac_tbl, by = setNames(c("cluster", mName), c("cluster", mName))) %>%
        dplyr::mutate(new_weight = weight * factor)
      
      # Because closure will depend on relative and absolute gap, add the
      # totals vector back to determine the absolute difference.
      # Remove rows from the table if the absolute diff is below the threshold.
      # This will keep them from keeping the IPF running.
      gap_tbl <- seed_long %>%
        dplyr::left_join(totals, by = "cluster") %>%
        dplyr::mutate(
          old = total * weight,
          new2 = total * new_weight,
          rel_diff = ifelse(weight == 0, 0, abs((new_weight - weight) / weight)),
          abs_diff = abs(new2 - old)
        )
            
      # Collect gap information and test if this marginal has converged
      # If every row in gap_tbl is below the absolute gap tolerance, then
      # collect the largest relative difference.  Otherwise, collect the
      # largest relative difference from the rows above the absolute gap
      # tolerance.  Also, collect information on cluster and category
      # to report out after IPF is complete.
      if (all(gap_tbl$abs_diff <= absolute_gap)) {
        rel_gap[i] <- max(gap_tbl$rel_diff)
      } else {
        gap_tbl <- gap_tbl %>%
          dplyr::filter(abs_diff > absolute_gap)
        
        rel_gap[i] <- max(gap_tbl$rel_diff)
      }
      pos <- which(gap_tbl$rel_diff == rel_gap[i])
      pos <- pos[1]
      rel_id[i] <- gap_tbl$cluster[pos]
      rel_cat[i] <- gap_tbl[[mName]][pos]
      abs_gap[i] <- gap_tbl$abs_diff[pos]
      v_converged[i] <- rel_gap[i] <= relative_gap | abs_gap[i] <= absolute_gap
      
      # Clean up seed_long for next iteration
      seed_long <- seed_long %>%
        dplyr::mutate(weight = new_weight) %>%
        dplyr::select(-c(factor, new_weight))
    }
    
    # Check for convergence and increment iter
    if(verbose){
      cat("\r Finished iteration ", iter)
    }
    converged <- all(v_converged)
    iter = iter + 1
  }
  
  # After the loop, scale up the weights to match the totals
  seed_long <- seed_long %>%
    dplyr::left_join(totals, by = "cluster") %>%
    dplyr::mutate(weight = weight * total) %>%
    dplyr::select(-total)
  
  # if iterations exceeded, throw a warning.
  if(iter > max_iterations){
    warning("Failed to converge after ", max_iterations, " iterations")
    utils::flush.console()
  }
  
  if (verbose) {
    position <- which(rel_gap == max(rel_gap))[1]
    message("Max Rel Gap:", rel_gap[position])
    message("Absolute Gap:", abs_gap[position])
    message("cluster:", rel_id[position])
    message("Marginal:", names(targets)[position])
    message("Category:", rel_cat[position])
    utils::flush.console()
  }
  
  # return the table with new weights
  return(seed_long)
}



