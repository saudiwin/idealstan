# Comprehensive tests for all model types (1-14) with static and dynamic ideal points
# Uses pathfinder for fast approximate inference

options(cmdstanr_warn_inits = FALSE)

# Skip helper for cmdstanr availability
skip_if_no_cmdstanr <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")

  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    skip("cmdstan not installed")
  }
}

# Helper function to check that model has valid draws
check_model_draws <- function(fit, min_draws = 100) {
  if (is.null(fit)) return(FALSE)
  if (is.null(fit@stan_samples)) return(FALSE)

  draws <- tryCatch({
    fit@stan_samples$draws()
  }, error = function(e) NULL)

  if (is.null(draws)) return(FALSE)

  n_draws <- posterior::ndraws(draws)
  return(n_draws >= min_draws)
}

# Helper function to run a single model test
run_model_test <- function(model_type, vary_ideal_pts = "none",
                           time_points = 1, inflate = FALSE,
                           description = "") {

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
    "13" = "binary",  # latent space uses binary outcomes
    "14" = "binary",
    "binary"
  )

  # Determine if model needs inflation
  needs_inflate <- model_type %in% c(2, 4, 6, 8, 10, 12, 14)

  # Simulate data
  sim_data <- tryCatch({
    id_sim_gen(
      num_person = 20,
      num_items = 10,
      model_type = sim_type,
      inflate = needs_inflate,
      time_points = time_points,
      time_process = if(vary_ideal_pts == "none") "random" else
                     if(vary_ideal_pts == "random_walk") "random" else
                     if(vary_ideal_pts == "AR1") "AR" else "random",
      ordinal_outcomes = 5
    )
  }, error = function(e) {
    message(paste("Simulation failed for model", model_type, ":", e$message))
    NULL
  })

  if (is.null(sim_data)) return(NULL)

  # Fit model
  fit <- tryCatch({
    id_estimate(
      sim_data,
      model_type = model_type,
      use_method = "pathfinder",
      vary_ideal_pts = vary_ideal_pts,
      fixtype = "prefix",
      restrict_ind_high = 1,
      restrict_ind_low = 2,
      spline_knots = if(vary_ideal_pts == "splines") 3 else NULL,
      spline_degree = if(vary_ideal_pts == "splines") 2 else NULL
    )
  }, error = function(e) {
    message(paste("Estimation failed for model", model_type,
                  "with", vary_ideal_pts, ":", e$message))
    NULL
  })

  return(fit)
}

# =============================================================================
# STATIC IDEAL POINTS TESTS (vary_ideal_pts = "none")
# =============================================================================

# Model Type 1: Binary IRT 2-PL, no inflation ----
test_that("Model 1: Binary IRT 2-PL (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(101)
  fit <- run_model_test(model_type = 1, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 1)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 2: Binary IRT 2-PL, with inflation ----
test_that("Model 2: Binary IRT 2-PL (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(102)
  fit <- run_model_test(model_type = 2, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 2)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 3: Ordinal rating scale, no inflation ----
test_that("Model 3: Ordinal rating scale (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(103)
  fit <- run_model_test(model_type = 3, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 3)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 4: Ordinal rating scale, with inflation ----
test_that("Model 4: Ordinal rating scale (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(104)
  fit <- run_model_test(model_type = 4, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 4)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 5: Ordinal GRM, no inflation ----
test_that("Model 5: Ordinal GRM (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(105)
  fit <- run_model_test(model_type = 5, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 5)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 6: Ordinal GRM, with inflation ----
test_that("Model 6: Ordinal GRM (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(106)
  fit <- run_model_test(model_type = 6, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 6)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 7: Poisson, no inflation ----
test_that("Model 7: Poisson (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(107)
  fit <- run_model_test(model_type = 7, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 7)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 8: Poisson, with inflation ----
test_that("Model 8: Poisson (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(108)
  fit <- run_model_test(model_type = 8, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 8)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 9: Gaussian, no inflation ----
test_that("Model 9: Gaussian (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(109)
  fit <- run_model_test(model_type = 9, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 9)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 10: Gaussian, with inflation ----
test_that("Model 10: Gaussian (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(110)
  fit <- run_model_test(model_type = 10, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 10)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 11: Log-normal, no inflation ----
test_that("Model 11: Log-normal (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(111)
  fit <- run_model_test(model_type = 11, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 11)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 12: Log-normal, with inflation ----
test_that("Model 12: Log-normal (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(112)
  fit <- run_model_test(model_type = 12, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 12)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 13: Latent space, no inflation ----
test_that("Model 13: Latent space (no inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(113)
  fit <- run_model_test(model_type = 13, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 13)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# Model Type 14: Latent space, with inflation ----
test_that("Model 14: Latent space (with inflation) with static ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(114)
  fit <- run_model_test(model_type = 14, vary_ideal_pts = "none")

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
  expect_equal(fit@model_type, 14)
  expect_true(check_model_draws(fit), info = "Model should have valid draws")
})

# =============================================================================
# DYNAMIC IDEAL POINTS: RANDOM WALK
# =============================================================================

test_that("Model 1: Binary with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(201)
  fit <- run_model_test(model_type = 1, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 3: Ordinal rating scale with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(203)
  fit <- run_model_test(model_type = 3, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 5: Ordinal GRM with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(205)
  fit <- run_model_test(model_type = 5, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 7: Poisson with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(207)
  fit <- run_model_test(model_type = 7, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 9: Gaussian with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(209)
  fit <- run_model_test(model_type = 9, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 11: Log-normal with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(211)
  fit <- run_model_test(model_type = 11, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

# =============================================================================
# DYNAMIC IDEAL POINTS: AR(1)
# =============================================================================

test_that("Model 1: Binary with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(301)
  fit <- run_model_test(model_type = 1, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 3: Ordinal rating scale with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(303)
  fit <- run_model_test(model_type = 3, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 5: Ordinal GRM with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(305)
  fit <- run_model_test(model_type = 5, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 7: Poisson with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(307)
  fit <- run_model_test(model_type = 7, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 9: Gaussian with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(309)
  fit <- run_model_test(model_type = 9, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

# =============================================================================
# DYNAMIC IDEAL POINTS: GAUSSIAN PROCESS (GP)
# =============================================================================

test_that("Model 1: Binary with GP ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(401)
  fit <- run_model_test(model_type = 1, vary_ideal_pts = "GP", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 3: Ordinal rating scale with GP ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(403)
  fit <- run_model_test(model_type = 3, vary_ideal_pts = "GP", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 5: Ordinal GRM with GP ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(405)
  fit <- run_model_test(model_type = 5, vary_ideal_pts = "GP", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 9: Gaussian with GP ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(409)
  fit <- run_model_test(model_type = 9, vary_ideal_pts = "GP", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

# =============================================================================
# DYNAMIC IDEAL POINTS: SPLINES
# =============================================================================

test_that("Model 1: Binary with spline ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(501)
  fit <- run_model_test(model_type = 1, vary_ideal_pts = "splines", time_points = 10)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 3: Ordinal rating scale with spline ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(503)
  fit <- run_model_test(model_type = 3, vary_ideal_pts = "splines", time_points = 10)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 5: Ordinal GRM with spline ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(505)
  fit <- run_model_test(model_type = 5, vary_ideal_pts = "splines", time_points = 10)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 9: Gaussian with spline ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(509)
  fit <- run_model_test(model_type = 9, vary_ideal_pts = "splines", time_points = 10)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

# =============================================================================
# INFLATED MODELS WITH DYNAMIC IDEAL POINTS (subset)
# =============================================================================

test_that("Model 2: Binary inflated with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(602)
  fit <- run_model_test(model_type = 2, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 4: Ordinal rating scale inflated with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(604)
  fit <- run_model_test(model_type = 4, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 6: Ordinal GRM inflated with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(606)
  fit <- run_model_test(model_type = 6, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 8: Poisson inflated with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(608)
  fit <- run_model_test(model_type = 8, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 10: Gaussian inflated with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(610)
  fit <- run_model_test(model_type = 10, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 12: Log-normal inflated with AR1 ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(612)
  fit <- run_model_test(model_type = 12, vary_ideal_pts = "AR1", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})

test_that("Model 14: Latent space inflated with random_walk ideal points", {
  skip_if_no_cmdstanr()
  skip_on_ci()

  set.seed(614)
  fit <- run_model_test(model_type = 14, vary_ideal_pts = "random_walk", time_points = 5)

  expect_true(!is.null(fit), info = "Model fitting should succeed")
  expect_s4_class(fit, "idealstan")
})
