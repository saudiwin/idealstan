# Tests for id_make function

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

test_that("id_make validates input parameters", {
  test_data <- data.frame(
    person_id = rep(1:10, each = 5),
    item_id = rep(1:5, times = 10),
    outcome = sample(c(0, 1), 50, replace = TRUE)
  )

  # Missing outcome column - should error
  expect_error(
    id_make(test_data, person_id = "person_id", item_id = "item_id")
  )
})
