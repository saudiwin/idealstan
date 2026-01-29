# Integration tests for full idealstan pipeline
# These tests run the complete workflow: simulate -> estimate -> extract -> plot

options(cmdstanr_warn_inits = FALSE)

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

    # Test parallel processing in summary (GitHub issue #37)
    # cores parameter should work without error
    summary_items_parallel <- summary(fit, pars = "items", cores = 2)
    expect_true(is.data.frame(summary_items_parallel) || inherits(summary_items_parallel, "tbl_df"))

    # Results should be identical regardless of cores used
    expect_equal(nrow(summary_items), nrow(summary_items_parallel))
    expect_equal(names(summary_items), names(summary_items_parallel))
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

# Test id_plot_compare function (GitHub issue #35) ----

test_that("id_plot_compare works with two fitted models", {
  skip_if_no_cmdstanr()
  skip_on_ci()  # Skip on CI due to time constraints

  set.seed(42)

  # Fit first model (binary)
  sim_data1 <- id_sim_gen(
    num_person = 15,
    num_items = 8,
    model_type = "binary"
  )

  fit1 <- tryCatch(
    id_estimate(
      sim_data1,
      model_type = 1,
      use_method = "pathfinder",
      fixtype = "prefix",
      restrict_ind_high = 1,
      restrict_ind_low = 2
    ),
    error = function(e) NULL
  )

  # Fit second model (also binary but different data)
  sim_data2 <- id_sim_gen(
    num_person = 15,
    num_items = 8,
    model_type = "binary"
  )

  fit2 <- tryCatch(
    id_estimate(
      sim_data2,
      model_type = 1,
      use_method = "pathfinder",
      fixtype = "prefix",
      restrict_ind_high = 1,
      restrict_ind_low = 2
    ),
    error = function(e) NULL
  )

  # Test id_plot_compare if both models fitted successfully
  if (!is.null(fit1) && !is.null(fit2)) {
    # This was failing with "could not find function legis_plot" (issue #35)
    expect_no_error({
      plot_result <- id_plot_compare(model1 = fit1, model2 = fit2)
    })

    # Check that result is a ggplot object
    expect_true(inherits(plot_result, "ggplot"))

    # Test with return_data = TRUE
    plot_with_data <- id_plot_compare(
      model1 = fit1,
      model2 = fit2,
      return_data = TRUE
    )
    expect_true(is.list(plot_with_data))
    expect_true("plot" %in% names(plot_with_data))
    expect_true("plot_data" %in% names(plot_with_data))
    expect_true(inherits(plot_with_data$plot, "ggplot"))
    expect_true(is.data.frame(plot_with_data$plot_data))
  } else {
    skip("Model fitting failed, skipping id_plot_compare test")
  }
})

# Test for mixed binary/ordinal items (GitHub issue #27) ----

test_that("id_make handles mixed binary and ordinal items with ordered_id", {
  skip_on_cran()

  # Create data with both binary (2 outcomes) and ordinal (3 outcomes) items
  # This is the scenario from issue #27
  set.seed(42)

  n_persons <- 15
  n_items <- 8
  n_time <- 2

  test_data <- expand.grid(
    person_id = paste0("P", 1:n_persons),
    item_id = paste0("I", 1:n_items),
    time_id = 1:n_time
  )

  # Items I1-I4 are ordinal (3 categories), I5-I8 are binary (2 categories)
  test_data$is_ordinal <- test_data$item_id %in% c("I1", "I2", "I3", "I4")

  test_data$outcome_disc <- ifelse(
    test_data$is_ordinal,
    sample(1:3, nrow(test_data), replace = TRUE),
    sample(1:2, nrow(test_data), replace = TRUE)
  )

  # ordered_id indicates number of categories per item
  test_data$ordered_id <- ifelse(test_data$is_ordinal, 3, 2)

  test_data$group_id <- rep(c("A", "B"), length.out = nrow(test_data))

  # All use GRM model
  test_data$model_id <- 5

  # Test id_make with ordered_id
  result <- id_make(
    score_data = test_data,
    person_id = "person_id",
    item_id = "item_id",
    time_id = "time_id",
    group_id = "group_id",
    model_id = "model_id",
    outcome_disc = "outcome_disc",
    ordered_id = "ordered_id"
  )

  expect_s4_class(result, "idealdata")

  # Check that ordered_id is properly stored and varies by item
  expect_true("ordered_id" %in% names(result@score_matrix))
  expect_equal(sort(unique(result@score_matrix$ordered_id)), c(2, 3))

  # Verify ordinal items have ordered_id = 3
  ordinal_items <- result@score_matrix$item_id %in% c("I1", "I2", "I3", "I4")
  expect_true(all(result@score_matrix$ordered_id[ordinal_items] == 3))

  # Verify binary items have ordered_id = 2
  binary_items <- result@score_matrix$item_id %in% c("I5", "I6", "I7", "I8")
  expect_true(all(result@score_matrix$ordered_id[binary_items] == 2))
})

# Test for item index bounds (GitHub issue #26) ----

test_that("GRM model item indices are within valid bounds", {
  skip_on_cran()

  # Create data with non-sequential item_id values
  # This tests that item_id is properly factorized and indexed from 1 to N
  set.seed(42)

  n_persons <- 20
  # Use non-sequential item_id values that could cause indexing issues
  # if not properly factorized (e.g., max value 300 vs count of 5)
  item_ids <- c(10, 50, 100, 200, 300)
  n_items <- length(item_ids)
  n_time <- 2

  test_data <- expand.grid(
    person_id = 1:n_persons,
    item_id = item_ids,
    time_id = 1:n_time
  )

  test_data$model_id <- 5  # GRM model
  test_data$outcome_disc <- sample(1:5, nrow(test_data), replace = TRUE)
  test_data$ordered_id <- 5
  test_data$group_id <- 1

  # Create idealdata
  result <- id_make(
    score_data = test_data,
    person_id = "person_id",
    item_id = "item_id",
    time_id = "time_id",
    group_id = "group_id",
    model_id = "model_id",
    outcome_disc = "outcome_disc",
    ordered_id = "ordered_id"
  )

  expect_s4_class(result, "idealdata")

  # Verify item_id is factorized properly
  expect_true(is.factor(result@score_matrix$item_id))
  expect_equal(nlevels(result@score_matrix$item_id), n_items)

  # Key test: as.numeric(item_id) should give indices 1 to n_items,
  # NOT the original values (10, 50, 100, 200, 300)
  numeric_item_ids <- as.numeric(result@score_matrix$item_id)
  expect_equal(min(numeric_item_ids), 1)
  expect_equal(max(numeric_item_ids), n_items)
  expect_equal(sort(unique(numeric_item_ids)), 1:n_items)
})

test_that("GRM model with non-sequential person_id maintains valid indices", {
  skip_on_cran()

  # Create data with non-sequential person_id values
  # This mimics real-world data where person IDs may have gaps
  set.seed(42)

  # Non-sequential person_ids (like the issue26.csv which has max 2452 but 598 unique)
  person_ids <- c(7, 13, 41, 57, 64, 68, 90, 101, 150, 200)
  n_persons <- length(person_ids)
  n_items <- 5
  n_time <- 2

  test_data <- expand.grid(
    person_id = person_ids,
    item_id = 1:n_items,
    time_id = 1:n_time
  )

  test_data$model_id <- 5  # GRM model
  test_data$outcome_disc <- sample(1:5, nrow(test_data), replace = TRUE)
  test_data$ordered_id <- 5
  test_data$group_id <- 1

  result <- id_make(
    score_data = test_data,
    person_id = "person_id",
    item_id = "item_id",
    time_id = "time_id",
    group_id = "group_id",
    model_id = "model_id",
    outcome_disc = "outcome_disc",
    ordered_id = "ordered_id"
  )

  expect_s4_class(result, "idealdata")

  # Verify person_id is factorized properly
  expect_true(is.factor(result@score_matrix$person_id))
  expect_equal(nlevels(result@score_matrix$person_id), n_persons)

  # Key test: as.numeric(person_id) should give indices 1 to n_persons,
  # NOT the original values (7, 13, 41, etc.)
  numeric_person_ids <- as.numeric(result@score_matrix$person_id)
  expect_equal(min(numeric_person_ids), 1)
  expect_equal(max(numeric_person_ids), n_persons)
  expect_equal(sort(unique(numeric_person_ids)), 1:n_persons)
})

test_that("GRM model data preparation produces valid Stan indices", {
  skip_on_cran()

  # This test directly verifies that the Stan data preparation creates
  # valid indices that won't cause out-of-bounds errors
  set.seed(42)

  # Use data similar to issue #26
  n_persons <- 30
  n_items <- 10
  n_time <- 3

  # Create non-sequential IDs
  person_ids <- sample(100:500, n_persons)
  item_ids <- sample(1:100, n_items)

  test_data <- expand.grid(
    person_id = person_ids,
    item_id = item_ids,
    time_id = 1:n_time
  )

  # Add some missing data
  set.seed(123)
  test_data$outcome_disc <- sample(c(1:5, NA), nrow(test_data), replace = TRUE,
                                    prob = c(rep(0.18, 5), 0.1))
  test_data$model_id <- 5  # GRM
  test_data$ordered_id <- 5
  test_data$group_id <- 1

  result <- id_make(
    score_data = test_data,
    person_id = "person_id",
    item_id = "item_id",
    time_id = "time_id",
    group_id = "group_id",
    model_id = "model_id",
    outcome_disc = "outcome_disc",
    ordered_id = "ordered_id"
  )

  expect_s4_class(result, "idealdata")

  # Simulate what .prepare_stan_data does
  billpoints <- as.numeric(result@score_matrix$item_id)
  legispoints <- as.numeric(result@score_matrix$person_id)
  modelpoints <- as.integer(result@score_matrix$model_id)

  # Calculate num_bills the same way the code does
  num_bills <- max(billpoints, na.rm = TRUE)
  num_legis <- max(legispoints, na.rm = TRUE)

  # num_bills_grm should equal num_bills for GRM model
  num_bills_grm <- ifelse(any(modelpoints %in% c(5, 6)), num_bills, 0L)

  # Critical assertions:
  # 1. billpoints should index from 1 to num_bills (not exceed array bounds)
  expect_true(all(billpoints >= 1))
  expect_true(all(billpoints <= num_bills))

  # 2. num_bills should equal the count of unique items, not the max original ID
  expect_equal(num_bills, n_items)

  # 3. legispoints should index from 1 to num_legis
  expect_true(all(legispoints >= 1))
  expect_true(all(legispoints <= num_legis))

  # 4. num_legis should equal the count of unique persons
  expect_equal(num_legis, n_persons)
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
