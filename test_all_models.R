# Standalone test script for all model types (1-14) with static and dynamic ideal points
# Run interactively to test models one by one

library(idealstan)
library(cmdstanr)

options(cmdstanr_warn_inits = FALSE)

# Helper function to run a single model test
run_model_test <- function(model_type, vary_ideal_pts = "none",
                           time_points = 1, num_person = 20, num_items = 10,
                           verbose = TRUE) {

  # Map model_type to simulation type
  sim_type <- switch(
    as.character(model_type),
    "1" = "binary",
    "2" = "binary",
    "3" = "ordinal_ratingscale",
    "4" = "ordinal_ratingscale",
    "5" = "ordinal_grm",
    "6" = "ordinal_grm",
    "7" = "poisson",
    "8" = "poisson",
    "9" = "normal",
    "10" = "normal",
    "11" = "lognormal",
    "12" = "lognormal",
    "13" = "binary",
    "14" = "binary",
    "binary"
  )

  # Determine if model needs inflation
  needs_inflate <- model_type %in% c(2, 4, 6, 8, 10, 12, 14)

  model_names <- c(
    "1" = "Binary IRT 2-PL",
    "2" = "Binary IRT 2-PL (inflated)",
    "3" = "Ordinal Rating Scale",
    "4" = "Ordinal Rating Scale (inflated)",
    "5" = "Ordinal GRM",
    "6" = "Ordinal GRM (inflated)",
    "7" = "Poisson",
    "8" = "Poisson (inflated)",
    "9" = "Gaussian",
    "10" = "Gaussian (inflated)",
    "11" = "Log-normal",
    "12" = "Log-normal (inflated)",
    "13" = "Latent Space",
    "14" = "Latent Space (inflated)"
  )

  if (verbose) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("Testing Model", model_type, ":", model_names[as.character(model_type)], "\n")
    cat("vary_ideal_pts:", vary_ideal_pts, "\n")
    cat("time_points:", time_points, "\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
  }

  # Simulate data
  if (verbose) cat("Simulating data...\n")

  sim_data <- tryCatch({
    id_sim_gen(
      num_person = num_person,
      num_items = num_items,
      model_type = sim_type,
      inflate = needs_inflate,
      time_points = time_points,
      time_process = if (vary_ideal_pts == "AR1") "AR" else "random",
      ordinal_outcomes = 5
    )
  }, error = function(e) {
    if (verbose) cat("ERROR in simulation:", e$message, "\n")
    return(NULL)
  })

  if (is.null(sim_data)) {
    if (verbose) cat("Simulation failed, skipping estimation.\n")
    return(list(success = FALSE, stage = "simulation", error = "Simulation failed"))
  }

  if (verbose) cat("Simulation successful. Rows:", nrow(sim_data@score_matrix), "\n")

  # Fit model
  if (verbose) cat("Fitting model with pathfinder...\n")

  fit <- tryCatch({
    id_estimate(
      sim_data,
      model_type = model_type,
      use_method = "pathfinder",
      vary_ideal_pts = vary_ideal_pts,
      fixtype = "prefix",
      restrict_ind_high = 1,
      restrict_ind_low = 2,
      spline_knots = if (vary_ideal_pts == "splines") 3 else NULL,
      spline_degree = if (vary_ideal_pts == "splines") 2 else NULL
    )
  }, error = function(e) {
    if (verbose) cat("ERROR in estimation:", e$message, "\n")
    return(NULL)
  })

  if (is.null(fit)) {
    if (verbose) cat("Estimation failed.\n")
    return(list(success = FALSE, stage = "estimation", error = "Estimation failed"))
  }

  if (verbose) {
    cat("SUCCESS! Model fitted.\n")
    cat("Model type stored:", fit@model_type, "\n")
  }

  return(list(success = TRUE, fit = fit))
}

# =============================================================================
# Run all tests
# =============================================================================

run_all_tests <- function(skip_dynamic = FALSE) {

  results <- list()

  cat("\n\n")
  cat(paste(rep("#", 70), collapse = ""), "\n")
  cat("# STATIC IDEAL POINTS TESTS\n")
  cat(paste(rep("#", 70), collapse = ""), "\n")

  # Static tests for all 14 model types
  for (mt in 1:14) {
    set.seed(100 + mt)
    result <- run_model_test(model_type = mt, vary_ideal_pts = "none")
    results[[paste0("static_", mt)]] <- result
  }

  if (!skip_dynamic) {

    cat("\n\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")
    cat("# DYNAMIC IDEAL POINTS: RANDOM WALK\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")

    for (mt in c(1, 3, 5, 7, 9, 11)) {
      set.seed(200 + mt)
      result <- run_model_test(model_type = mt, vary_ideal_pts = "random_walk", time_points = 5)
      results[[paste0("rw_", mt)]] <- result
    }

    cat("\n\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")
    cat("# DYNAMIC IDEAL POINTS: AR(1)\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")

    for (mt in c(1, 3, 5, 7, 9)) {
      set.seed(300 + mt)
      result <- run_model_test(model_type = mt, vary_ideal_pts = "AR1", time_points = 5)
      results[[paste0("ar1_", mt)]] <- result
    }

    cat("\n\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")
    cat("# DYNAMIC IDEAL POINTS: GAUSSIAN PROCESS\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")

    for (mt in c(1, 3, 5, 9)) {
      set.seed(400 + mt)
      result <- run_model_test(model_type = mt, vary_ideal_pts = "GP", time_points = 5)
      results[[paste0("gp_", mt)]] <- result
    }

    cat("\n\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")
    cat("# DYNAMIC IDEAL POINTS: SPLINES\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")

    for (mt in c(1, 3, 5, 9)) {
      set.seed(500 + mt)
      result <- run_model_test(model_type = mt, vary_ideal_pts = "splines", time_points = 10)
      results[[paste0("spline_", mt)]] <- result
    }

    cat("\n\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")
    cat("# INFLATED MODELS WITH DYNAMIC IDEAL POINTS\n")
    cat(paste(rep("#", 70), collapse = ""), "\n")

    for (mt in c(2, 4, 6, 8, 10, 12, 14)) {
      set.seed(600 + mt)
      result <- run_model_test(model_type = mt, vary_ideal_pts = "random_walk", time_points = 5)
      results[[paste0("infl_rw_", mt)]] <- result
    }
  }

  # Summary
  cat("\n\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("SUMMARY\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")

  successes <- sum(sapply(results, function(x) x$success))
  failures <- sum(sapply(results, function(x) !x$success))

  cat("Total tests:", length(results), "\n")
  cat("Successes:", successes, "\n")
  cat("Failures:", failures, "\n")

  if (failures > 0) {
    cat("\nFailed tests:\n")
    for (name in names(results)) {
      if (!results[[name]]$success) {
        cat("  -", name, ":", results[[name]]$stage, "-", results[[name]]$error, "\n")
      }
    }
  }

  invisible(results)
}

# =============================================================================
# Quick test function for a specific model
# =============================================================================

quick_test <- function(model_type, vary = "none", time_pts = 1) {
  set.seed(42)
  run_model_test(
    model_type = model_type,
    vary_ideal_pts = vary,
    time_points = time_pts,
    verbose = TRUE
  )
}

# =============================================================================
# Usage examples:
# =============================================================================

cat("
================================================================================
IDEALSTAN MODEL TEST SCRIPT
================================================================================

Available functions:

1. run_all_tests()
   - Runs all model tests (static + dynamic)
   - Use run_all_tests(skip_dynamic = TRUE) to only test static models

2. quick_test(model_type, vary, time_pts)
   - Test a specific model
   - Examples:
     quick_test(1)                            # Binary static
     quick_test(5, 'random_walk', 5)          # GRM with random walk
     quick_test(3, 'AR1', 5)                  # Ordinal rating scale with AR1
     quick_test(1, 'GP', 5)                   # Binary with GP
     quick_test(1, 'splines', 10)             # Binary with splines

3. run_model_test(model_type, vary_ideal_pts, time_points, ...)
   - Full control over test parameters

Model types:
  1  = Binary IRT 2-PL (no inflation)
  2  = Binary IRT 2-PL (with inflation)
  3  = Ordinal rating scale (no inflation)
  4  = Ordinal rating scale (with inflation)
  5  = Ordinal GRM (no inflation)
  6  = Ordinal GRM (with inflation)
  7  = Poisson (no inflation)
  8  = Poisson (with inflation)
  9  = Gaussian (no inflation)
  10 = Gaussian (with inflation)
  11 = Log-normal (no inflation)
  12 = Log-normal (with inflation)
  13 = Latent space (no inflation)
  14 = Latent space (with inflation)

vary_ideal_pts options: 'none', 'random_walk', 'AR1', 'GP', 'splines'

================================================================================
")
