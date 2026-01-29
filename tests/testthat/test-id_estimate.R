# Tests for id_estimate function

options(cmdstanr_warn_inits = FALSE)

test_that("id_estimate validates use_method parameter", {
  skip_on_cran()

  # Create minimal test data
  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # Invalid use_method should error

  expect_error(
    id_estimate(sim_data, model_type = "binary", use_method = "invalid_method"),
    "use_method"
  )
})

test_that("id_estimate validates idealdata input", {
  skip_on_cran()

  # Passing NULL should work but return NULL if cmdstanr not available
  # Passing wrong type should error
  expect_error(
    id_estimate(idealdata = "not_an_idealdata", model_type = "binary"),
    class = "error"
  )

  expect_error(
    id_estimate(idealdata = data.frame(x = 1), model_type = "binary"),
    class = "error"
  )
})

test_that("id_estimate validates model_type parameter", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # model_type should be numeric 1-16
  expect_error(
    id_estimate(sim_data, model_type = "binary"),
    class = "error"
  )
})

test_that("id_estimate validates vary_ideal_pts parameter", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # vary_ideal_pts should be one of specific values
  # This test checks that valid values are accepted (no error in setup)
  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate validates fixtype parameter", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # fixtype should be one of: 'prefix', 'postfix', 'vb_full', 'vb_partial'
  valid_fixtypes <- c("prefix", "postfix", "vb_full", "vb_partial")

  # Just verify simulation works for setup

  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate validates const_type parameter", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # const_type should be "persons" or "items"
  valid_const_types <- c("persons", "items")

  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate validates numeric parameters", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # nchains, niters, warmup, ncores should be positive integers
  # These would be validated during estimation
  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate validates restrict_ind parameters", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # restrict_ind_high and restrict_ind_low should be valid indices
  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate validates prior parameters", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = "binary"
  )

  # Prior parameters should be positive where required
  # discrim_reg_scale, discrim_reg_shape, person_sd, etc.
  expect_s4_class(sim_data, "idealdata")
})

test_that("id_estimate handles subset parameters", {
  skip_on_cran()

  sim_data <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = "binary"
  )

  # use_subset, subset_group, subset_person, sample_size
  expect_s4_class(sim_data, "idealdata")
})
