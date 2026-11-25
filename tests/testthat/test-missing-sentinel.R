# Tests for the missing-value sentinels passed to Stan (GitHub issue #42).
# The Stan likelihood for inflated models counts a row as observed only when
# Y_int[n] < y_int_miss (or Y_cont[n] < y_cont_miss for continuous outcomes).

options(cmdstanr_warn_inits = FALSE)

skip_if_no_cmdstan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    skip("cmdstan not installed")
  }
}

sentinel_data <- function(seed = 42) {
  set.seed(seed)
  n_person <- 30
  n_item <- 6
  ideal <- rnorm(n_person)
  discrim <- c(runif(n_item - 1, 0.5, 1.5), -1)
  d <- expand.grid(person = 1:n_person, item = 1:n_item)
  lin <- discrim[d$item] * ideal[d$person]
  d$share <- plogis(lin + rnorm(nrow(d), 0, 0.5))
  d$norm <- lin + rnorm(nrow(d), 0, 0.5)
  d$pos <- exp(lin + rnorm(nrow(d), 0, 0.5))
  d$bin <- as.numeric(lin + rlogis(nrow(d)) > 0)
  d$miss <- runif(nrow(d)) < 0.2
  d
}

fit_sentinel <- function(d, ...) {
  dat <- id_make(d, person_id = "person", item_id = "item",
                 model_id = "model_id", ...)
  suppressMessages(id_estimate(dat, fixtype = "prefix", const_type = "items",
                               restrict_ind_high = "1", restrict_ind_low = "6",
                               use_method = "pathfinder", seed = 1))
}

# Rows of inflated models that the Stan likelihood treats as missing
n_stan_missing <- function(fit) {
  td <- fit@this_data
  cont <- td$mm %in% c(10, 12, 16)
  disc <- td$mm %in% c(2, 4, 6, 8, 14)
  n_cont <- if (any(cont)) sum(td$Y_cont[cont] >= td$y_cont_miss) else 0L
  n_disc <- if (any(disc)) sum(td$Y_int[disc] >= td$y_int_miss) else 0L
  c(disc = n_disc, cont = n_cont)
}

test_that("id_make keeps the continuous missing code exact with a discrete outcome", {
  skip_on_cran()

  d <- sentinel_data()
  d$model_id <- ifelse(d$item <= 3, 1, 9)
  d$bin[d$item > 3] <- NA
  d$norm[d$item <= 3] <- NA
  d$norm[d$item > 3 & d$miss] <- NA

  dat <- id_make(d, outcome_disc = "bin", outcome_cont = "norm",
                 person_id = "person", item_id = "item", model_id = "model_id")

  miss_code <- as.numeric(dat@miss_val[2])
  cont_rows <- dat@score_matrix$model_id == 9
  expect_equal(sum(dat@score_matrix$outcome_cont[cont_rows] == miss_code),
               sum(d$item > 3 & d$miss))
})

test_that(".warn_inflated_no_missing flags only inflated models without missing rows", {
  remove_list <- list(modelpoints = c(2, 2, 10, 10, 1),
                      Y_int = c(0, 1, 5, 5, 1), y_int_miss = 2,
                      Y_cont = c(9, 9, 0.5, 3, 9), y_cont_miss = 3)

  expect_warning(ids <- .warn_inflated_no_missing(remove_list),
                 "inflated model\\(s\\) 2 were chosen.*non-inflated model\\(s\\) 1")
  expect_equal(ids, 2L)

  remove_list$Y_int[1] <- 2
  expect_no_warning(.warn_inflated_no_missing(remove_list))
})

test_that("inflated models with no missing outcomes treat every row as observed", {
  skip_if_no_cmdstan()

  d <- sentinel_data()

  cases <- list(
    list(model = 16, outcome_cont = "share"),
    list(model = 10, outcome_cont = "norm"),
    list(model = 12, outcome_cont = "pos"),
    list(model = 2, outcome_disc = "bin")
  )

  for (cs in cases) {
    d$model_id <- cs$model
    expect_warning(
      fit <- do.call(fit_sentinel, c(list(d = d), cs[names(cs) != "model"])),
      "none of their outcomes are missing",
      info = paste("model", cs$model)
    )
    expect_equal(unname(n_stan_missing(fit)), c(0, 0),
                 info = paste("model", cs$model))
  }
})

test_that("inflated models with missing outcomes still flag exactly the missing rows", {
  skip_if_no_cmdstan()

  d <- sentinel_data()
  n_miss <- sum(d$miss)

  d$model_id <- 16
  d$share[d$miss] <- NA
  fit16 <- fit_sentinel(d, outcome_cont = "share")
  expect_equal(n_stan_missing(fit16)[["cont"]], n_miss)

  d$model_id <- 2
  d$bin[d$miss] <- NA
  fit2 <- fit_sentinel(d, outcome_disc = "bin")
  expect_equal(n_stan_missing(fit2)[["disc"]], n_miss)
})

test_that("mixed discrete and inflated continuous data flag the continuous missing rows", {
  skip_if_no_cmdstan()

  d <- sentinel_data()
  d$model_id <- ifelse(d$item <= 3, 1, 10)
  d$bin[d$item > 3] <- NA
  d$norm[d$item <= 3] <- NA
  d$norm[d$item > 3 & d$miss] <- NA

  fit <- fit_sentinel(d, outcome_disc = "bin", outcome_cont = "norm")

  expect_equal(n_stan_missing(fit)[["cont"]], sum(d$item > 3 & d$miss))
})

test_that("missing values of non-inflated models do not move the sentinel", {
  skip_if_no_cmdstan()

  d <- sentinel_data()
  # missing values only in items 1-3, which use the non-inflated model
  d$miss <- d$miss & d$item <= 3

  db <- d
  db$model_id <- ifelse(db$item <= 3, 1, 2)
  db$bin[db$miss] <- NA
  fit_bin <- suppressWarnings(fit_sentinel(db, outcome_disc = "bin"))
  expect_equal(fit_bin@this_data$N, nrow(d) - sum(d$miss))
  expect_equal(n_stan_missing(fit_bin)[["disc"]], 0)

  dc <- d
  dc$model_id <- ifelse(dc$item <= 3, 9, 10)
  dc$norm[dc$miss] <- NA
  # put the highest observed value in an inflated item
  dc$norm[dc$item == 4 & dc$person == 1] <- max(dc$norm, na.rm = TRUE) + 1
  fit_norm <- suppressWarnings(fit_sentinel(dc, outcome_cont = "norm"))
  expect_equal(fit_norm@this_data$N, nrow(d) - sum(d$miss))
  expect_equal(n_stan_missing(fit_norm)[["cont"]], 0)
})

test_that("id_make keeps ordered beta outcomes when mixed with discrete items", {
  skip_on_cran()

  d <- sentinel_data()
  d$model_id <- ifelse(d$item <= 3, 1, 15)
  d$bin[d$item > 3] <- NA
  d$share[d$item <= 3] <- NA

  dat <- id_make(d, outcome_disc = "bin", outcome_cont = "share",
                 person_id = "person", item_id = "item", model_id = "model_id")

  sm <- dat@score_matrix
  ob <- sm$model_id == 15
  expect_true(all(sm$outcome_disc[ob] == "Joint Posterior"))
  expect_equal(sort(sm$outcome_cont[ob]), sort(d$share[d$item > 3]))
})

test_that("mixed discrete, normal and ordered beta data keep every ordered beta row", {
  skip_if_no_cmdstan()

  d <- sentinel_data()
  d$model_id <- c(1, 1, 9, 9, 15, 15)[d$item]
  d$bin[d$item > 2] <- NA
  d$norm[d$item <= 2 | d$item >= 5] <- NA
  d$share[d$item <= 4] <- NA
  d$cont <- ifelse(d$item <= 4, d$norm, d$share)
  d$cont[d$item == 5 & d$person <= 3] <- NA   # missing ordered beta values

  fit <- fit_sentinel(d, outcome_disc = "bin", outcome_cont = "cont")
  td <- fit@this_data
  ob <- td$mm == 15

  expect_equal(sum(ob), sum(d$item >= 5) - 3)
  expect_true(all(td$Y_cont[ob] >= 0 & td$Y_cont[ob] <= 1))
})
