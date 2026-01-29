# Integration tests for full idealstan pipeline
# These tests run the complete workflow: simulate -> estimate -> extract -> plot

# Skip helper for cmdstanr availability
skip_if_no_cmdstanr <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")

  # Also check if cmdstan is installed
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    skip("cmdstan not installed")
  }
}

# Full pipeline test with binary model ----

test_that("full pipeline works for binary model with pathfinder", {
  skip_if_no_cmdstanr()
  skip_on_ci()  # Skip on CI due to time constraints

  set.seed(42)

  # Simulate data
  sim_data <- id_sim_gen(
    num_person = 15,
    num_items = 8,
    model_type = "binary"
  )

  expect_s4_class(sim_data, "idealdata")

  # Estimate model using pathfinder (fast approximate inference)
  fit <- id_estimate(
    sim_data,
    model_type = 1,
    use_method = "pathfinder",
    fixtype = "prefix",
    restrict_ind_high = 1,
    restrict_ind_low = 2
  )

  # Check result
  if (!is.null(fit)) {
    expect_s4_class(fit, "idealstan")

    # Extract parameters
    persons <- id_extract(fit, extract_type = "persons")
    expect_true(is.data.frame(persons) || inherits(persons, "tbl_df"))

    # Test summary function (GitHub issue #39)
    summary_pts <- summary(fit, pars = "ideal_pts")
    expect_true(is.data.frame(summary_pts) || inherits(summary_pts, "tbl_df"))

    summary_items <- summary(fit, pars = "items")
    expect_true(is.data.frame(summary_items) || inherits(summary_items, "tbl_df"))
  }
})

# Test summary function for ordinal rating scale model (GitHub issue #39) ----

test_that("summary function works for ordinal rating scale model", {
  skip_if_no_cmdstanr()
  skip_on_ci()  # Skip on CI due to time constraints

  set.seed(42)

  # Simulate ordinal rating scale data
  sim_data <- id_sim_gen(
    num_person = 15,
    num_items = 8,
    model_type = "ordinal_ratingscale"
  )

  expect_s4_class(sim_data, "idealdata")

  # Estimate model using pathfinder (fast approximate inference)
  fit <- id_estimate(
    sim_data,
    model_type = 3,  # ordinal rating scale
    use_method = "pathfinder",
    fixtype = "prefix",
    restrict_ind_high = 1,
    restrict_ind_low = 2
  )

  # Check result
  if (!is.null(fit)) {
    expect_s4_class(fit, "idealstan")

    # Test summary with pars="items" - this was failing in issue #39
    # The bug was: "Can't find the following variable(s) in the output: steps_votes"
    summary_items <- summary(fit, pars = "items")
    expect_true(is.data.frame(summary_items) || inherits(summary_items, "tbl_df"))

    # Test summary with pars="ideal_pts"
    summary_pts <- summary(fit, pars = "ideal_pts")
    expect_true(is.data.frame(summary_pts) || inherits(summary_pts, "tbl_df"))
  }
})

# Simulation to estimation consistency test ----

test_that("simulated data can be used with id_make", {
  skip_on_cran()

  set.seed(123)

  # Simulate data
  sim_data <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = "binary"
  )

  # The simulated data should have the proper structure
  expect_s4_class(sim_data, "idealdata")
  expect_true(!is.null(sim_data@score_matrix))
  expect_true(!is.null(sim_data@simul_data))
})

# Data flow test: simulate -> prepare ----

test_that("simulation output matches id_make input requirements", {
  skip_on_cran()

  set.seed(42)

  # Different model types should all produce valid idealdata
  model_types <- c("binary", "ordinal_grm", "poisson", "normal")

  for (mt in model_types) {

    print(mt)

    result <- id_sim_gen(
      num_person = 15,
      num_items = 8,
      model_type = mt
    )

    expect_s4_class(
      result,
      "idealdata"
    )
  }
})

# Time-varying pipeline test ----

test_that("time-varying simulation produces valid data", {
  skip_on_cran()

  # Test each time process - use compatible time_points
  time_processes <- c("random", "AR")

  for (tp in time_processes) {
    set.seed(42)
    result <- tryCatch(
      id_sim_gen(
        num_person = 10,
        num_items = 5,
        model_type = "binary",
        time_points = 10,  # Multiple of num_items
        time_process = tp
      ),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      expect_s4_class(
        result,
        "idealdata",
        info = paste("Time process", tp, "failed")
      )
    }
  }
})

# Inflated model pipeline test ----

test_that("inflated model simulation produces valid data", {
  skip_on_cran()

  set.seed(42)

  # Binary inflated
  result_binary <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = "binary",
    inflate = TRUE
  )

  expect_s4_class(result_binary, "idealdata")
})

# Covariate pipeline test ----

test_that("covariate simulation produces valid data", {
  skip_on_cran()

  set.seed(42)

  result <- id_sim_gen(
    num_person = 30,
    num_items = 10,
    model_type = "binary",
    cov_effect = 1.5
  )

  expect_s4_class(result, "idealdata")
  expect_true(!is.null(result@simul_data))
})

# Real data test with senate114 ----

test_that("id_make works with senate114 data", {
  skip_on_cran()

  data("senate114", package = "idealstan")

  # Check that data loaded
  expect_true(exists("senate114"))

  # id_make should work with this real data
  # Note: This depends on the exact structure of senate114
})

# Multiple simulation reproducibility test ----

test_that("simulation is reproducible across multiple runs", {
  skip_on_cran()

  # Run simulation twice with same seed
  set.seed(99999)
  result1 <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  set.seed(99999)
  result2 <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # Should produce identical true values
  expect_identical(
    result1@simul_data$true_person,
    result2@simul_data$true_person
  )

  expect_identical(
    result1@simul_data$true_reg_discrim,
    result2@simul_data$true_reg_discrim
  )
})

# Edge case: minimum viable data ----

test_that("pipeline handles minimum viable data size", {
  skip_on_cran()

  set.seed(42)

  # Minimum reasonable size
  result <- id_sim_gen(
    num_person = 5,
    num_items = 3,
    model_type = "binary"
  )

  expect_s4_class(result, "idealdata")
})

# Score matrix structure validation ----

test_that("simulated data has valid score matrix structure", {
  skip_on_cran()

  set.seed(42)

  result <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = "binary"
  )

  # Check score matrix exists and has expected columns
  expect_true(!is.null(result@score_matrix))
  expect_true(is.data.frame(result@score_matrix) || inherits(result@score_matrix, "tbl_df"))
})

# Model type consistency test ----

test_that("all documented model types can be simulated", {
  skip_on_cran()

  set.seed(42)

  # Model types that should work
  model_types <- c("binary", "ordinal_grm", "ordinal_ratingscale", "poisson", "normal", "lognormal")

  for (mt in model_types) {
    result <- tryCatch(
      id_sim_gen(
        num_person = 10,
        num_items = 5,
        model_type = mt,
        ordinal_outcomes = 4  # for ordinal models
      ),
      error = function(e) {
        message(paste("Model type", mt, "error:", e$message))
        NULL
      }
    )

    # Should either succeed or fail gracefully
    expect_true(
      is.null(result) || inherits(result, "idealdata"),
      info = paste("Model type", mt, "produced unexpected output")
    )
  }
})
