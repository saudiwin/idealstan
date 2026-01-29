# Tests for utility functions

# id_show_trues tests ----

test_that("id_show_trues function exists", {
  expect_true(exists("id_show_trues"))
  expect_true(is.function(id_show_trues))
})

test_that("id_show_trues has expected parameters", {
  args <- names(formals(id_show_trues))

  expect_true("sims" %in% args)
  expect_true("type" %in% args)
})

# id_sim_rmse tests ----

test_that("id_sim_rmse function exists", {
  expect_true(exists("id_sim_rmse"))
  expect_true(is.function(id_sim_rmse))
})

test_that("id_sim_rmse has expected parameters", {
  args <- names(formals(id_sim_rmse))

  expect_true("obj" %in% args)
  expect_true("rep" %in% args)
})

# id_sim_coverage tests ----

test_that("id_sim_coverage function exists", {
  expect_true(exists("id_sim_coverage"))
  expect_true(is.function(id_sim_coverage))
})

test_that("id_sim_coverage has expected parameters", {
  args <- names(formals(id_sim_coverage))

  expect_true("obj" %in% args)
  expect_true("quantiles" %in% args)
})

# id_sim_resid tests ----

test_that("id_sim_resid function exists", {
  expect_true(exists("id_sim_resid"))
  expect_true(is.function(id_sim_resid))
})

test_that("id_sim_resid has expected parameters", {
  args <- names(formals(id_sim_resid))

  expect_true("obj" %in% args)
  expect_true("rep" %in% args)
})

# derive_chain tests ----

test_that("derive_chain function exists", {
  expect_true(exists("derive_chain"))
  expect_true(is.function(derive_chain))
})

test_that("derive_chain has expected parameters", {
  args <- names(formals(derive_chain))

  expect_true("ll_matrix" %in% args)
})

test_that("derive_chain handles NULL input", {
  result <- derive_chain(NULL)
  expect_null(result)
})

# stan_trace tests ----

test_that("stan_trace generic exists", {
  expect_true(isGeneric("stan_trace"))
})

test_that("stan_trace method exists for idealstan class", {
  methods <- showMethods("stan_trace", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

# id_me tests ----

test_that("id_me generic exists", {
  expect_true(isGeneric("id_me"))
})

test_that("id_me method exists for idealstan class", {
  methods <- showMethods("id_me", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

# launch_shinystan tests ----

test_that("launch_shinystan generic exists", {
  expect_true(isGeneric("launch_shinystan"))
})

test_that("launch_shinystan method exists for idealstan class", {
  methods <- showMethods("launch_shinystan", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

# summary method tests ----

test_that("summary method exists for idealstan class", {
  methods <- showMethods("summary", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

# id_plot_ppc tests ----

test_that("id_plot_ppc generic exists", {
  expect_true(isGeneric("id_plot_ppc"))
})

test_that("id_plot_ppc method exists for idealstan class", {
  methods <- showMethods("id_plot_ppc", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})
