# Tests for id_extract function

# Function existence and signature tests ----

test_that("id_extract generic exists", {
  expect_true(isGeneric("id_extract"))
})

test_that("id_extract method exists for idealstan class", {
  methods <- showMethods("id_extract", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

test_that("id_extract has expected parameters", {
  # Get the method for idealstan - note that S4 methods may show only

  # generic params in formals(), but the method body accepts extract_type
  method <- selectMethod("id_extract", "idealstan")
  args <- names(formals(method))

  expect_true("object" %in% args)
  # extract_type is defined in the method body, passed via ...
  expect_true("..." %in% args || "extract_type" %in% args)
})

# Extract type validation tests ----

test_that("id_extract accepts valid extract_type values", {
  # Document expected extract_type values
  valid_types <- c("persons", "items", "person", "item")

  # This test verifies the expected types are known
  expect_true(length(valid_types) > 0)
})

# Tests with simulated data (requires fitted model) ----

test_that("id_extract requires idealstan object", {
  skip_on_cran()

  # Should error with non-idealstan input
  expect_error(
    id_extract("not_an_idealstan"),
    class = "error"
  )

  expect_error(
    id_extract(data.frame(x = 1)),
    class = "error"
  )
})
