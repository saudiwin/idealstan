# Tests for binomial outcomes (model types 17 and 18) and for count models
# keeping the numeric value of the outcome.

options(cmdstanr_warn_inits = FALSE)

skip_if_no_cmdstan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    skip("cmdstan not installed")
  }
}

fit_binom <- function(dat, ...) {
  suppressMessages(id_estimate(dat, use_method = "pathfinder", seed = 1,
                               fixtype = "prefix", const_type = "items", ...))
}

binom_data <- function(seed = 7) {
  set.seed(seed)
  n_person <- 30
  ideal <- rnorm(n_person)
  d <- expand.grid(person = 1:n_person, item = 1:6)
  lin <- c(1, -1, 1, 1, -1, 1)[d$item] * ideal[d$person]
  d$n_trials <- sample(c(1, 5, 20), nrow(d), replace = TRUE)
  d$successes <- rbinom(nrow(d), d$n_trials, plogis(lin))
  d
}

test_that("id_make stores the number of trials and the numeric outcome", {
  skip_on_cran()

  d <- binom_data()
  d$model_id <- 17
  dat <- id_make(d, outcome_disc = "successes", outcome_trials = "n_trials",
                 person_id = "person", item_id = "item", model_id = "model_id")

  sm <- dat@score_matrix
  key <- paste(sm$person_id, sm$item_id)
  orig <- d[match(key, paste(d$person, d$item)), ]

  expect_equal(sm$outcome_trials, orig$n_trials)
  expect_equal(sm$outcome_disc_raw, orig$successes)
})

test_that(".count_data returns counts and trials and checks them", {
  skip_on_cran()

  d <- binom_data()
  d$model_id <- 17
  d$successes[1] <- NA
  dat <- id_make(d, outcome_disc = "successes", outcome_trials = "n_trials",
                 person_id = "person", item_id = "item", model_id = "model_id")
  sm <- dat@score_matrix

  out <- .count_data(sm$outcome_disc, sm$model_id, dat)
  observed <- !is.na(sm$outcome_disc_raw)

  expect_equal(out$count[observed], sm$outcome_disc_raw[observed])
  expect_true(all(is.na(out$count[!observed])))
  expect_equal(out$trials[observed], sm$outcome_trials[observed])
  expect_true(all(out$trials[!observed] == 0))

  # more successes than trials
  bad <- dat
  bad@score_matrix$outcome_trials[which(observed & sm$outcome_disc_raw > 0)[1]] <- 0
  expect_error(.count_data(sm$outcome_disc, sm$model_id, bad), "cannot be larger")

  # no trials column
  no_trials <- dat
  no_trials@score_matrix$outcome_trials <- NULL
  expect_error(.count_data(sm$outcome_disc, sm$model_id, no_trials), "outcome_trials")

  # not a count
  frac <- dat
  frac@score_matrix$outcome_disc_raw[which(observed)[1]] <- 0.5
  expect_error(.count_data(sm$outcome_disc, sm$model_id, frac), "whole number")
})

test_that("binomial model (17) passes successes and trials to Stan", {
  skip_if_no_cmdstan()

  set.seed(21)
  sim <- id_sim_gen(num_person = 30, num_items = 10, model_type = "binomial",
                    binom_trials = 8)
  fit <- fit_binom(sim, model_type = 17, restrict_ind_high = 1, restrict_ind_low = 2)
  td <- fit@this_data

  expect_equal(sort(td$Y_int), sort(sim@score_matrix$outcome_disc_raw))
  expect_true(all(td$trials == 8))

  # posterior predictions never exceed the number of trials
  pp <- suppressMessages(id_post_pred(fit, draws = 20))
  expect_true(all(unlist(pp[[1]]) <= 8))

  # log-likelihood is draws x observations
  ll <- suppressMessages(id_post_pred(fit, draws = 20, type = "log_lik"))
  expect_equal(ncol(ll), td$N)
  expect_true(all(is.finite(ll)))
})

test_that("inflated binomial model (18) flags exactly the missing outcomes", {
  skip_if_no_cmdstan()

  d <- binom_data()
  d$model_id <- 18
  miss <- runif(nrow(d)) < 0.2
  d$successes[miss] <- NA
  d$n_trials[miss] <- NA   # trials are not needed for missing outcomes

  dat <- id_make(d, outcome_disc = "successes", outcome_trials = "n_trials",
                 person_id = "person", item_id = "item", model_id = "model_id")
  fit <- fit_binom(dat, restrict_ind_high = "1", restrict_ind_low = "2")
  td <- fit@this_data

  expect_equal(td$N, nrow(d))
  expect_equal(sum(td$Y_int >= td$y_int_miss), sum(miss))
  expect_true(all(td$Y_int[td$Y_int < td$y_int_miss] <= td$trials[td$Y_int < td$y_int_miss]))
})

test_that("binomial items mix with binary and normal items", {
  skip_if_no_cmdstan()

  d <- binom_data()
  d$model_id <- c(1, 1, 17, 17, 9, 9)[d$item]
  d$y_disc <- ifelse(d$model_id == 1, as.numeric(d$successes > d$n_trials / 2),
                     ifelse(d$model_id == 17, d$successes, NA))
  d$y_cont <- ifelse(d$model_id == 9, d$successes / d$n_trials, NA)
  d$n_trials[d$model_id != 17] <- NA

  dat <- id_make(d, outcome_disc = "y_disc", outcome_cont = "y_cont",
                 outcome_trials = "n_trials", person_id = "person",
                 item_id = "item", model_id = "model_id")
  fit <- fit_binom(dat, restrict_ind_high = "3", restrict_ind_low = "4")
  td <- fit@this_data
  sm <- fit@score_data@score_matrix

  # every binomial row reaches Stan with its own successes and trials
  b <- td$mm == 17
  stan_rows <- paste(levels(sm$person_id)[td$ll[b]], levels(sm$item_id)[td$bb[b]])
  orig <- d[d$model_id == 17, ]
  orig <- orig[match(stan_rows, paste(orig$person, orig$item)), ]

  expect_equal(sum(b), sum(d$model_id == 17))
  expect_equal(as.numeric(td$Y_int[b]), orig$y_disc)
  expect_equal(as.numeric(td$trials[b]), orig$n_trials)
  expect_setequal(unique(td$Y_int[td$mm == 1]), c(0, 1))
})

test_that("Poisson models use the counts, not their ranks", {
  skip_if_no_cmdstan()

  set.seed(3)
  d <- data.frame(person = rep(1:20, each = 4), item = rep(1:4, 20), model_id = 7)
  d$y <- rpois(nrow(d), c(1, 5, 1, 5)[d$item]) + ifelse(d$item == 2, 100, 0)

  dat <- id_make(d, outcome_disc = "y", person_id = "person",
                 item_id = "item", model_id = "model_id")
  fit <- fit_binom(dat, restrict_ind_high = "1", restrict_ind_low = "2")

  expect_equal(sort(as.numeric(fit@this_data$Y_int)), sort(d$y))
})
