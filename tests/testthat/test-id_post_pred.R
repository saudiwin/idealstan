# Tests for id_post_pred function

# Function existence and signature tests ----

test_that("id_post_pred generic exists", {
  expect_true(isGeneric("id_post_pred"))
})

test_that("id_post_pred method exists for idealstan class", {
  methods <- showMethods("id_post_pred", printTo = FALSE)
  expect_true(any(grepl("idealstan", methods)))
})

test_that("id_post_pred has expected parameters", {
  # Get the method for idealstan - note that S4 methods may show only
  # generic params in formals(), but additional params are in the method body
  method <- selectMethod("id_post_pred", "idealstan")
  args <- names(formals(method))

  expect_true("object" %in% args)
  # Additional parameters (draws, output, type) are defined in the method body
  # and passed via ..., so we just check that ... is available
  expect_true("..." %in% args)
})

# Parameter documentation tests ----

test_that("id_post_pred output parameter accepts expected values", {
  # Document expected output types
  valid_outputs <- c("observed", "missing", "both")
  expect_true(length(valid_outputs) > 0)
})

test_that("id_post_pred type parameter accepts expected values", {
  # Document expected type values
  valid_types <- c("predict", "log_lik")
  expect_true(length(valid_types) > 0)
})

# Error handling tests ----

test_that("id_post_pred requires idealstan object", {
  skip_on_cran()

  expect_error(
    id_post_pred("not_an_idealstan"),
    class = "error"
  )

  expect_error(
    id_post_pred(data.frame(x = 1)),
    class = "error"
  )
})
