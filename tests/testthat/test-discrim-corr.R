# Tests for correlated observed and missingness discriminations
# (id_estimate(discrim_corr = TRUE)) and for positive discriminations.

options(cmdstanr_warn_inits = FALSE)

skip_if_no_cmdstan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    skip("cmdstan not installed")
  }
}

# normal score of a generalized beta draw, as in the Stan model
gb_score <- function(y, a = 2, b = 2, lb = -1, off = 2) qnorm(pbeta((y - lb) / off, a, b))

test_that("the copula covers the unpinned items of inflated models", {
  skip_on_cran()

  # items 1-4 use models 2 (inflated binary), 1, 4 (inflated rating scale) and
  # 14 (inflated latent space); item 3 is pinned high and item 2 pinned low
  td <- list(bb = rep(1:4, each = 2), mm = rep(c(2, 1, 4, 14), each = 2),
             const_type = 2, pos_discrim = 0, restrict_high = 3, restrict_low = 2)

  expect_equal(as.vector(.discrim_corr_items(td, TRUE)$corr_items), 1L)
  # pinned items keep their place when persons are pinned or discriminations are positive
  expect_equal(as.vector(.discrim_corr_items(modifyList(td, list(const_type = 1)), TRUE)$corr_items),
               c(1L, 3L))
  expect_equal(as.vector(.discrim_corr_items(modifyList(td, list(pos_discrim = 1)), TRUE)$corr_items),
               c(1L, 3L))
  expect_equal(.discrim_corr_items(td, FALSE)$num_corr, 0L)
})

test_that("id_sim_gen draws correlated discriminations with unchanged marginals", {
  skip_on_cran()

  set.seed(2)
  sim <- id_sim_gen(num_person = 2, num_items = 2000, inflate = TRUE, discrim_corr = 0.7)
  reg <- sim@simul_data$true_reg_discrim
  abs_d <- sim@simul_data$true_abs_discrim

  expect_equal(sim@simul_data$true_discrim_corr, 0.7)
  expect_equal(cor(gb_score(reg), gb_score(abs_d)), 0.7, tolerance = 0.05)
  expect_gt(ks.test((reg + 1) / 2, "pbeta", 2, 2)$p.value, 0.001)
  expect_gt(ks.test((abs_d + 1) / 2, "pbeta", 2, 2)$p.value, 0.001)

  expect_error(id_sim_gen(discrim_corr = 1), "between -1 and 1")
})

test_that("the copula keeps each discrimination's prior as its marginal", {
  skip_if_no_cmdstan()

  # sample from the prior: the discriminations should still have their
  # generalized beta priors, and their normal scores should have correlation rho
  set.seed(3)
  sim <- id_sim_gen(num_person = 10, num_items = 8, inflate = TRUE)
  fit <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 2, discrim_corr = TRUE, prior_only = TRUE,
                use_method = "mcmc", nchains = 1, niters = 2000, warmup = 500,
                seed = 3, fixtype = "prefix", restrict_ind_high = 1,
                restrict_ind_low = 2)))

  expect_equal(fit@this_data$num_corr, 8)

  rho <- as.numeric(fit@stan_samples$draws("discrim_rho"))
  reg <- posterior::as_draws_matrix(fit@stan_samples$draws("sigma_reg_free"))
  abs_d <- posterior::as_draws_matrix(fit@stan_samples$draws("sigma_abs_free"))

  # generalized beta(2, 2) on (-1, 1) has mean 0 and variance 0.2
  expect_equal(mean(reg), 0, tolerance = 0.05)
  expect_equal(mean(abs_d), 0, tolerance = 0.05)
  expect_equal(var(as.vector(reg)), 0.2, tolerance = 0.15)
  expect_equal(var(as.vector(abs_d)), 0.2, tolerance = 0.15)

  # E[z_reg * z_abs | rho] = rho
  zz <- as.vector(gb_score(unclass(reg)) * gb_score(unclass(abs_d)))
  slope <- coef(lm(zz ~ rep(rho, ncol(reg))))[2]
  expect_equal(unname(slope), 1, tolerance = 0.3)
})

test_that("discrim_corr recovers a strong correlation", {
  skip_if_no_cmdstan()

  # MCMC, because pathfinder collapses the posterior of the correlation to a point.
  # The correlation doesn't change if the scale is reflected, since both
  # discriminations of every item change sign together.
  set.seed(4)
  sim <- id_sim_gen(num_person = 100, num_items = 60, inflate = TRUE, discrim_corr = 0.9)
  fit <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 2, discrim_corr = TRUE, use_method = "mcmc",
                nchains = 2, ncores = 2, niters = 500, warmup = 500,
                seed = 4, fixtype = "prefix", const_type = "items",
                restrict_ind_high = which.max(sim@simul_data$true_reg_discrim),
                restrict_ind_low = which.min(sim@simul_data$true_reg_discrim))))

  expect_equal(fit@this_data$num_corr, 58)

  out <- summary(fit, pars = "discrim_corr")
  expect_equal(out$`Number of Items`, 58)
  expect_gt(out$`Low Posterior Interval`, 0.3)
  expect_gt(out$`High Posterior Interval`, 0.8)

  # without discrim_corr there is no correlation parameter
  fit0 <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 2, use_method = "pathfinder", seed = 4,
                fixtype = "prefix", const_type = "items",
                restrict_ind_high = which.max(sim@simul_data$true_reg_discrim),
                restrict_ind_low = which.min(sim@simul_data$true_reg_discrim))))
  expect_equal(fit0@this_data$num_corr, 0)
  expect_error(summary(fit0, pars = "discrim_corr"), "discrim_corr=TRUE")
})

test_that("discrim_corr warns when no items are inflated", {
  skip_if_no_cmdstan()

  set.seed(5)
  sim <- id_sim_gen(num_person = 20, num_items = 8, inflate = FALSE)
  expect_warning(
    suppressMessages(id_estimate(sim, model_type = 1, discrim_corr = TRUE,
                                 use_method = "pathfinder", seed = 5, fixtype = "prefix",
                                 restrict_ind_high = 1, restrict_ind_low = 2)),
    "discrim_corr=TRUE has no effect")
})

test_that("positive discriminations fit, with and without discrim_corr", {
  skip_if_no_cmdstan()

  set.seed(6)
  sim <- id_sim_gen(num_person = 40, num_items = 20, inflate = TRUE,
                    discrim_reg_lb = 0, discrim_corr = 0.5)

  # discrim_reg_lb = 0: generalized beta prior on (0, 1). The parameter used to be
  # declared on (-0.999, 0.999), so every negative value had a non-finite log density.
  out <- capture.output(fit <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 2, discrim_reg_lb = 0, discrim_corr = TRUE,
                use_method = "pathfinder", seed = 6, fixtype = "prefix",
                restrict_ind_high = 1, restrict_ind_low = 2))), type = "output")
  expect_false(any(grepl("Non-finite", out)))
  reg <- posterior::as_draws_matrix(fit@stan_samples$draws("sigma_reg_free"))
  expect_equal(fit@this_data$pos_discrim, 0)
  expect_true(all(reg > 0 & reg < 1))
  expect_true(all(is.finite(as.numeric(fit@stan_samples$draws("discrim_rho")))))

  # both bounds positive: exponential prior with no upper bound
  for (corr in c(FALSE, TRUE)) {
    fit <- suppressMessages(suppressWarnings(
      id_estimate(sim, model_type = 2, discrim_reg_lb = 0.01, discrim_corr = corr,
                  use_method = "pathfinder", seed = 6, fixtype = "prefix",
                  restrict_ind_high = 1, restrict_ind_low = 2)))
    reg <- posterior::as_draws_matrix(fit@stan_samples$draws("sigma_reg_free"))
    expect_equal(fit@this_data$pos_discrim, 1)
    expect_equal(fit@this_data$num_corr, if (corr) 20 else 0)
    expect_true(all(reg > 0))
  }
})
