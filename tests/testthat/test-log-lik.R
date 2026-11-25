# Tests for the pointwise log-likelihood and expected values returned by
# id_post_pred(type = "log_lik") and id_post_pred(type = "epred"). The helpers
# take draws x observations matrices of Pr(outcome) and Pr(missing) for one item.

options(cmdstanr_warn_inits = FALSE)

set.seed(42)
S <- 6   # draws
n <- 4   # observations
pr_vote <- matrix(runif(S * n, 0.1, 0.9), S, n)
pr_absence <- matrix(runif(S * n, 0.1, 0.9), S, n)
miss_code <- 1000

# total probability of the possible outcomes y (plus Pr(missing) if inflated)
total_prob <- function(f, ys, inflate, ...) {
  tot <- Reduce(`+`, lapply(ys, function(y) {
    exp(f(pr_absence=pr_absence, pr_vote=pr_vote, inflate=inflate, type="log_lik",
          outcome=rep(y, n), miss_val=miss_code, N=n, ...))
  }))
  if(inflate) {
    tot <- tot + exp(f(pr_absence=pr_absence, pr_vote=pr_vote, inflate=TRUE, type="log_lik",
                       outcome=rep(miss_code, n), miss_val=miss_code, N=n, ...))
  }
  tot
}

test_that("log_lik is a draws x observations matrix that sums to one over outcomes", {
  skip_on_cran()

  cutpoints <- t(apply(matrix(rnorm(S * 3), S, 3), 1, sort))
  trials <- c(1, 3, 5, 10)

  for (inflate in c(FALSE, TRUE)) {
    ll <- .binary(pr_absence=pr_absence, pr_vote=pr_vote, inflate=inflate, type="log_lik",
                  outcome=c(0, 1, 1, 0), miss_val=miss_code, latent_space=FALSE)
    expect_equal(dim(ll), c(S, n))

    expect_equal(total_prob(.binary, 0:1, inflate, latent_space=FALSE),
                 matrix(1, S, n), tolerance=1e-8)
    expect_equal(total_prob(.poisson, 0:300, inflate), matrix(1, S, n), tolerance=1e-8)
    expect_equal(total_prob(.binomial, 0:10, inflate, trials=trials), matrix(1, S, n), tolerance=1e-8)
    expect_equal(total_prob(.ordinal_ratingscale, 1:4, inflate, cutpoints=cutpoints),
                 matrix(1, S, n), tolerance=1e-8)
  }
})

test_that("missing rows get log Pr(missing) and observed rows add log Pr(observed)", {
  skip_on_cran()

  y <- c(2, miss_code, 0, miss_code)
  ll_infl <- .poisson(pr_absence=pr_absence, pr_vote=pr_vote, inflate=TRUE, type="log_lik",
                      outcome=y, miss_val=miss_code)
  ll_obs <- dpois(matrix(c(2, 0, 0, 0), S, n, byrow=TRUE), exp(qlogis(pr_vote)), log=TRUE)

  expect_equal(ll_infl[, c(2, 4)], log(pr_absence[, c(2, 4)]))
  expect_equal(ll_infl[, c(1, 3)], log1p(-pr_absence[, c(1, 3)]) + ll_obs[, c(1, 3)])
})

test_that("continuous log_lik uses the item's own parameters", {
  skip_on_cran()

  sigma <- runif(S, 0.5, 2)
  y <- c(-1, 0.3, 2, 0.5)
  ll <- .normal(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="log_lik",
                outcome=y, miss_val=miss_code, sigma_sd=sigma)
  expect_equal(ll, dnorm(matrix(y, S, n, byrow=TRUE), qlogis(pr_vote), sigma, log=TRUE))

  ll <- .lognormal(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="log_lik",
                   outcome=abs(y), miss_val=miss_code, sigma_sd=sigma)
  expect_equal(ll, dlnorm(matrix(abs(y), S, n, byrow=TRUE), qlogis(pr_vote), sigma, log=TRUE))

  # ordered beta matches ordbetareg::dordbeta
  cuts <- t(apply(matrix(rnorm(S * 2), S, 2), 1, sort))
  phi <- runif(S, 1, 5)
  y <- c(0, 0.25, 1, 0.8)
  ll <- .ordbeta(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="log_lik",
                 outcome=y, miss_val=miss_code, cutpoints=cuts, phi=phi)
  expected <- sapply(1:n, function(j) sapply(1:S, function(s) {
    ordbetareg::dordbeta(y[j], mu=pr_vote[s, j], phi=phi[s], cutpoints=cuts[s, ], log=TRUE)
  }))
  expect_equal(ll, expected)
})

test_that("epred is the expected outcome, times Pr(observed) if inflated", {
  skip_on_cran()

  cutpoints <- t(apply(matrix(rnorm(S * 3), S, 3), 1, sort))
  sigma <- runif(S, 0.5, 2)

  # ordinal: expected category from the category probabilities
  probs <- lapply(1:4, function(k) exp(.ordinal_ratingscale(
    pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="log_lik",
    outcome=rep(k, n), miss_val=miss_code, cutpoints=cutpoints)))
  ep <- .ordinal_ratingscale(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE,
                             type="epred", cutpoints=cutpoints)
  expect_equal(unclass(ep)[, ], Reduce(`+`, Map(`*`, 1:4, probs)), ignore_attr=TRUE)

  ep <- .poisson(pr_absence=pr_absence, pr_vote=pr_vote, inflate=TRUE, type="epred")
  expect_equal(ep, (1 - pr_absence) * exp(qlogis(pr_vote)), ignore_attr=TRUE)

  ep <- .normal(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="epred",
                sigma_sd=sigma)
  expect_equal(ep, qlogis(pr_vote), ignore_attr=TRUE)

  ep <- .lognormal(pr_absence=pr_absence, pr_vote=pr_vote, inflate=FALSE, type="epred",
                   sigma_sd=sigma)
  expect_equal(ep, exp(qlogis(pr_vote) + sigma^2/2), ignore_attr=TRUE)

  ep <- .binomial(pr_absence=pr_absence, pr_vote=pr_vote, inflate=TRUE, type="epred",
                  trials=c(1, 3, 5, 10))
  expect_equal(ep, (1 - pr_absence) * pr_vote * matrix(c(1, 3, 5, 10), S, n, byrow=TRUE),
               ignore_attr=TRUE)
})

test_that("id_post_pred log_lik and epred work for mixed model types", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) skip("cmdstan not installed")

  set.seed(9)
  n_person <- 25
  ideal <- rnorm(n_person)
  d <- expand.grid(person = 1:n_person, item = 1:6)
  lin <- c(1, -1, 1, 1, -1, 1)[d$item] * ideal[d$person]
  d$model_id <- c(1, 1, 17, 17, 9, 9)[d$item]
  d$n_trials <- ifelse(d$model_id == 17, 5, NA)
  d$y_disc <- ifelse(d$model_id == 1, as.numeric(lin + rlogis(nrow(d)) > 0),
                     ifelse(d$model_id == 17, rbinom(nrow(d), 5, plogis(lin)), NA))
  d$y_cont <- ifelse(d$model_id == 9, lin + rnorm(nrow(d), 0, 0.5), NA)

  dat <- id_make(d, outcome_disc = "y_disc", outcome_cont = "y_cont",
                 outcome_trials = "n_trials", person_id = "person",
                 item_id = "item", model_id = "model_id")
  fit <- suppressMessages(id_estimate(dat, use_method = "pathfinder", seed = 1,
                                      fixtype = "prefix", const_type = "items",
                                      restrict_ind_high = "1", restrict_ind_low = "2"))

  ll <- suppressMessages(id_post_pred(fit, type = "log_lik"))
  expect_equal(ncol(ll), fit@this_data$N)
  expect_true(all(is.finite(ll)))

  ep <- suppressMessages(id_post_pred(fit, draws = 20, type = "epred"))
  ep_all <- do.call(cbind, unlist(ep, recursive = FALSE))
  expect_equal(dim(ep_all), c(20, fit@this_data$N))
  expect_true(all(is.finite(ep_all)))
})
