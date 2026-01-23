# Tests for S4 classes

test_that("idealdata class is properly defined", {
  expect_true(isClass("idealdata"))

  # Check expected slots exist
  slot_names <- slotNames("idealdata")
  expect_true("score_matrix" %in% slot_names)
  expect_true("person_data" %in% slot_names)
  expect_true("item_data" %in% slot_names)
})

test_that("idealstan class is properly defined", {
  expect_true(isClass("idealstan"))

  # Check expected slots exist
  slot_names <- slotNames("idealstan")
  expect_true("score_data" %in% slot_names)
  expect_true("model_type" %in% slot_names)
  expect_true("stan_samples" %in% slot_names)
})
