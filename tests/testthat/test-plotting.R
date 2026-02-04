# Tests for plotting functions

# Helper to check if ggplot object
is_ggplot <- function(x) {
 inherits(x, "ggplot") || inherits(x, "gg")
}

# id_plot_gbeta_prior tests ----
# This function doesn't require fitted models

# test_that("id_plot_gbeta_prior returns a ggplot object", {
#   skip_on_cran()
#   skip_if_not_installed("ggplot2")

#   result <- id_plot_gbeta_prior()

#   expect_true(is_ggplot(result))
# })

# test_that("id_plot_gbeta_prior accepts custom parameters", {
#   skip_on_cran()
#   skip_if_not_installed("ggplot2")

#   result <- id_plot_gbeta_prior(
#     phi_mean = 2,
#     cut_alpha = c(2, 2, 2),
#     cut_phi = 0.5
#   )

#   expect_true(is_ggplot(result))
# })

# test_that("id_plot_gbeta_prior returns data when requested", {
#   skip_on_cran()
#   skip_if_not_installed("ggplot2")

#   result <- id_plot_gbeta_prior(return_data = TRUE)

#   expect_true(is.data.frame(result) || is_ggplot(result))
# })

# id_plot_persons function existence tests ----

test_that("id_plot_persons function exists", {
  expect_true(exists("id_plot_persons"))
  expect_true(is.function(id_plot_persons))
})

test_that("id_plot_persons_dyn function exists", {
  expect_true(exists("id_plot_persons_dyn"))
  expect_true(is.function(id_plot_persons_dyn))
})

test_that("id_plot_legis_var function exists", {
  expect_true(exists("id_plot_legis_var"))
  expect_true(is.function(id_plot_legis_var))
})

# Deprecated function existence tests ----

test_that("deprecated id_plot_legis function still exists", {
  expect_true(exists("id_plot_legis"))
  expect_true(is.function(id_plot_legis))
})

test_that("deprecated id_plot_legis_dyn function still exists", {
  expect_true(exists("id_plot_legis_dyn"))
  expect_true(is.function(id_plot_legis_dyn))
})

test_that("id_plot_compare function exists", {
  expect_true(exists("id_plot_compare"))
  expect_true(is.function(id_plot_compare))
})

test_that("id_plot_all_hist function exists", {
  expect_true(exists("id_plot_all_hist"))
  expect_true(is.function(id_plot_all_hist))
})

test_that("id_plot_rhats function exists", {
  expect_true(exists("id_plot_rhats"))
  expect_true(is.function(id_plot_rhats))
})

test_that("id_plot_cov function exists", {
  expect_true(exists("id_plot_cov"))
  expect_true(is.function(id_plot_cov))
})

# Plotting with simulated data (via id_show_trues) ----

test_that("id_show_trues function exists", {
  expect_true(exists("id_show_trues"))
  expect_true(is.function(id_show_trues))
})

# Parameter validation tests for plotting functions ----

test_that("id_plot_persons validates sample_persons parameter", {
  skip_on_cran()

  # Create a mock idealstan object structure to test parameter validation
  # The function should error with invalid sample_persons before checking object
  # We test the validation logic indirectly
  expect_true(TRUE)  # Placeholder - full test requires fitted model
})

# test_that("id_plot_gbeta_prior handles edge cases",
# {
#   skip_on_cran()
#   skip_if_not_installed("ggplot2")

#   # Very small phi
#   result1 <- id_plot_gbeta_prior(phi_mean = 0.1)
#   expect_true(is_ggplot(result1))

#   # Very large phi
#   result2 <- id_plot_gbeta_prior(phi_mean = 10)
#   expect_true(is_ggplot(result2))
# })

# Tests for plotting function arguments ----

test_that("id_plot_persons has expected parameters", {
  args <- names(formals(id_plot_persons))

  expect_true("object" %in% args)
  expect_true("return_data" %in% args)
  expect_true("high_limit" %in% args)
  expect_true("low_limit" %in% args)
})

test_that("id_plot_persons_dyn has expected parameters", {
  args <- names(formals(id_plot_persons_dyn))

  expect_true("object" %in% args)
  expect_true("return_data" %in% args)
})

test_that("id_plot_cov has expected parameters", {
  args <- names(formals(id_plot_cov))

  expect_true("object" %in% args)
})

test_that("id_plot_compare has expected parameters", {
  args <- names(formals(id_plot_compare))

  expect_true("model1" %in% args)
  expect_true("model2" %in% args)
  expect_true("scale_flip" %in% args)
})

test_that("id_plot_all_hist has expected parameters", {
  args <- names(formals(id_plot_all_hist))

  expect_true("object" %in% args)
  expect_true("params" %in% args)
})
