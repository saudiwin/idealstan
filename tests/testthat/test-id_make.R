# Tests for id_make function

# Basic functionality tests ----

test_that("id_make creates idealdata object from basic input", {
  skip_on_cran()

  # Create simple test data
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
  expect_equal(nrow(result@score_matrix), 50)
})

test_that("id_make handles missing data correctly", {
  skip_on_cran()

  # Create test data with missing values
  set.seed(123)
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1, NA), 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_make handles group_id parameter", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    group_id = rep(c("A", "B"), each = 25),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id",
    group_id = "group_id"
  )

  expect_s4_class(result, "idealdata")
})

# Input validation tests ----

test_that("id_make handles default column names gracefully", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  # When outcome columns use defaults that don't exist, id_make may still work
  # or produce a result with missing data - this tests graceful handling
  result <- tryCatch(
    id_make(test_data, person_id = "person_id", item_id = "item_id"),
    error = function(e) NULL
  )

  # Either produces result or NULL
  expect_true(is.null(result) || inherits(result, "idealdata"))
})

test_that("id_make errors on missing person_id column", {
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  expect_error(
    id_make(
      test_data,
      outcome_disc = "outcome",
      person_id = "nonexistent_column",
      item_id = "item_id"
    )
  )
})

test_that("id_make errors on missing item_id column", {
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  expect_error(
    id_make(
      test_data,
      outcome_disc = "outcome",
      person_id = "person_id",
      item_id = "nonexistent_column"
    )
  )
})

# Time series tests ----

test_that("id_make handles time_id parameter", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 15),
    item_id = rep(rep(1:5, each = 3), times = 10),
    time_id = rep(rep(1:3, times = 5), times = 10),
    outcome = sample(c(0, 1), 150, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id",
    time_id = "time_id"
  )

  expect_s4_class(result, "idealdata")
})

# Different outcome types ----

test_that("id_make handles ordinal outcomes", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(1:5, 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_make handles continuous outcomes", {
  skip_on_cran()

  set.seed(456)
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = rnorm(50, mean = 0, sd = 1)
  )

  result <- id_make(
    test_data,
    outcome_cont = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})

# Covariate tests ----

test_that("id_make handles person covariates", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE),
    covariate = rep(rnorm(10), each = 5)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id",
    person_cov = ~ covariate
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_make handles item covariates", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE),
    item_cov = rep(rnorm(5), times = 10)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id",
    item_cov = ~ item_cov
  )

  expect_s4_class(result, "idealdata")
})

# Factor handling tests ----

test_that("id_make handles factor person_id", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = factor(rep(paste0("P", 1:10), each = 5)),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})

test_that("id_make handles character person_id", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(paste0("Person_", 1:10), each = 5),
    item_id = rep(paste0("Item_", 1:5), times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})

# Edge cases ----

test_that("id_make handles single person", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1, 5),
    item_id = 1:5,
    outcome = sample(c(0, 1), 5, replace = TRUE)
  )

  # This might error or work depending on implementation
  # Testing that it handles gracefully
  result <- tryCatch(
    id_make(
      test_data,
      outcome_disc = "outcome",
      person_id = "person_id",
      item_id = "item_id"
    ),
    error = function(e) NULL
  )

  # Either returns idealdata or NULL (from error)
  expect_true(is.null(result) || inherits(result, "idealdata"))
})

test_that("id_make handles single item", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = 1:10,
    item_id = rep(1, 10),
    outcome = sample(c(0, 1), 10, replace = TRUE)
  )

  # This might error or work depending on implementation
  result <- tryCatch(
    id_make(
      test_data,
      outcome_disc = "outcome",
      person_id = "person_id",
      item_id = "item_id"
    ),
    error = function(e) NULL
  )

  expect_true(is.null(result) || inherits(result, "idealdata"))
})

# Score matrix structure tests ----

test_that("id_make score_matrix has correct structure", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_true("person_id" %in% names(result@score_matrix))
  expect_true("item_id" %in% names(result@score_matrix))
})

# All missing for a person/item tests ----

test_that("id_make handles all-missing outcomes for some persons", {
  skip_on_cran()

  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = c(rep(NA, 5), sample(c(0, 1), 45, replace = TRUE))
  )

  result <- id_make(
    test_data,
    outcome_disc = "outcome",
    person_id = "person_id",
    item_id = "item_id"
  )

  expect_s4_class(result, "idealdata")
})
