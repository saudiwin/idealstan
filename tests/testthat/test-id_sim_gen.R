# Tests for id_sim_gen function

test_that("id_sim_gen creates simulated data for binary model", {
  skip_on_cran()

  result <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = 1  # Binary logit
  )

  expect_s4_class(result, "idealdata")
  expect_true(!is.null(result@simul_data))
})

test_that("id_sim_gen creates simulated data for ordinal model", {
  skip_on_cran()

  result <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = 3  # Ordinal GRM
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_sim_gen handles time-varying ideal points", {
  skip_on_cran()

  result <- id_sim_gen(
    num_person = 20,
    num_items = 10,
    model_type = 1,
    T = 5  # 5 time periods
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_sim_gen respects seed for reproducibility", {
  skip_on_cran()

  result1 <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = 1,
    seed = 12345
  )

  result2 <- id_sim_gen(
    num_person = 10,
    num_items = 5,
    model_type = 1,
    seed = 12345
  )

  expect_equal(
    result1@simul_data$true_person,
    result2@simul_data$true_person
  )
})
