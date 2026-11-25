# Tests for multidimensional ideal points (id_estimate(num_dim = )).

options(cmdstanr_warn_inits = FALSE)

skip_if_no_cmdstan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) skip("cmdstan not installed")
}

# Log density of every draw under the fitted model (CmdStan's log_prob method),
# optionally without the likelihood
stan_lp <- function(fit, prior_only = FALSE) {
  dat <- fit@stan_samples$data_file()
  if (prior_only) {
    d <- jsonlite::read_json(dat, simplifyVector = FALSE)
    d$prior_only <- 1L
    dat <- tempfile(fileext = ".json")
    jsonlite::write_json(d, dat, auto_unbox = TRUE, digits = NA)
  }
  out <- tempfile(fileext = ".csv")
  system2(fit@stan_samples$runset$exe_file(),
          c("log_prob", paste0("constrained_params=", fit@stan_samples$output_files()[1]),
            "jacobian=1", "data", paste0("file=", dat), "output", paste0("file=", out)),
          env = "STAN_NUM_THREADS=1", stdout = FALSE, stderr = FALSE)
  utils::read.csv(out, comment.char = "#")$lp__
}

# Stan's log-likelihood of each draw (the priors and Jacobian cancel)
stan_log_lik <- function(fit) stan_lp(fit) - stan_lp(fit, prior_only = TRUE)

fit_2d <- function(sim, ...) {
  suppressMessages(suppressWarnings(
    id_estimate(sim, num_dim = 2, use_method = "pathfinder", seed = 1, fixtype = "prefix",
                restrict_ind_high = 1, restrict_ind_low = 2, restrict_ind_dim = 3, ...)))
}

test_that("pinned items give a lower-triangular discrimination matrix", {
  skip_on_cran()

  # 3 dimensions: item 2 is pinned high on dimension 1, items 4 and 5 identify
  # dimensions 2 and 3
  td <- list(D = 3, num_bills = 6, const_type = 2, pos_discrim = 0,
             restrict_high = 2, restrict_dim = c(4, 5))
  m <- .dim_masks(td)
  expect_equal(which(m$zero, arr.ind = TRUE), cbind(row = c(2, 2, 4), col = c(1, 2, 2)))
  expect_equal(which(m$anchor, arr.ind = TRUE), cbind(row = c(4, 5), col = c(1, 2)))

  # positive discriminations need no pinned prior; persons pinned need no zeros
  expect_false(any(.dim_masks(modifyList(td, list(pos_discrim = 1)))$anchor))
  expect_false(any(unlist(.dim_masks(modifyList(td, list(const_type = 1))))))
  expect_equal(dim(.dim_masks(modifyList(td, list(D = 1)))$zero), c(6, 0))
})

test_that("the dimension data and restrict_ind_dim are checked", {
  skip_on_cran()

  expect_equal(.dim_data(1)$D, 1L)
  expect_length(.dim_data(1)$restrict_dim, 0)
  # placeholders after the dimension-1 pins until the model is identified
  expect_equal(as.vector(.dim_data(3)$restrict_dim), 3:4)
  expect_equal(as.vector(.dim_data(2, c(`2` = 7L))$fix_dim), 2)

  dim_prefix <- function(x, ...) .id_dim_prefix(NULL, x, num_dim = 3, const_type = "persons",
                                                use_groups = FALSE, restrict_ind_high = 1,
                                                restrict_ind_low = 2, fix_high = 2, ...)
  expect_error(dim_prefix(NULL), "restrict_ind_dim")
  expect_error(dim_prefix(5), "2 value")
  expect_error(dim_prefix(c(1, 5)), "should not repeat")
  expect_equal(dim_prefix(c(5, 6)), c(`2` = 5L, `2` = 6L))

  set.seed(1)
  sim <- id_sim_gen(num_person = 10, num_items = 6)
  expect_error(suppressMessages(id_estimate(sim, num_dim = 1.5)), "whole number")
})

test_that("id_sim_gen simulates extra dimensions and leaves one dimension unchanged", {
  skip_on_cran()

  set.seed(3)
  a <- id_sim_gen(num_person = 8, num_items = 12, time_points = 3, inflate = TRUE)
  set.seed(3)
  b <- id_sim_gen(num_person = 8, num_items = 12, time_points = 3, inflate = TRUE, num_dim = 1)
  # (the score matrix also stores the call, which differs)
  expect_identical(a@score_matrix$outcome_disc, b@score_matrix$outcome_disc)
  expect_identical(a@simul_data$true_person, b@simul_data$true_person)
  expect_identical(a@simul_data$true_reg_discrim, b@simul_data$true_reg_discrim)
  expect_null(a@simul_data$true_person_d)

  set.seed(3)
  s <- id_sim_gen(num_person = 8, num_items = 12, time_points = 3, inflate = TRUE, num_dim = 3)
  expect_equal(dim(s@simul_data$true_person_d), c(8, 3, 2))
  expect_equal(dim(s@simul_data$true_reg_discrim_d), c(12, 2))
  expect_equal(dim(s@simul_data$true_abs_discrim_d), c(12, 2))
  expect_true(all(abs(s@simul_data$true_reg_discrim_d) < 1))
  expect_equal(dim(.true_person(s@simul_data, 3)), c(8, 3))

  expect_error(id_sim_gen(num_dim = 0), "whole number")
})

test_that("the Stan and R log-likelihoods agree with two dimensions", {
  skip_if_no_cmdstan()

  check <- function(sim_args, ...) {
    set.seed(1)
    fit <- fit_2d(do.call(id_sim_gen, sim_args), ...)
    r_ll <- rowSums(unclass(suppressMessages(id_post_pred(fit, type = "log_lik"))))
    max(abs(stan_log_lik(fit) - r_ll))
  }

  b <- list(num_person = 20, num_items = 10, num_dim = 2)
  tv <- list(num_person = 20, num_items = 24, time_points = 6, num_dim = 2)

  # CSV draws are rounded to 6 significant digits, so the two agree to about 1e-5
  expect_lt(check(c(b, inflate = TRUE, person_cov = "continuous", person_cov_effect = 1,
                    item_cov = "continuous", item_discrim_cov_effect = 0.5), model_type = 2), 1e-3)
  expect_lt(check(c(b, inflate = TRUE, latent_space = TRUE), model_type = 14), 1e-3)
  expect_lt(check(c(b, inflate = TRUE, model_type = "ordinal_grm"), model_type = 6), 1e-3)
  expect_lt(check(c(b, inflate = TRUE, model_type = "ordbeta"), model_type = 16), 1e-3)
  expect_lt(check(c(b, inflate = TRUE), model_type = 2, map_over_id = "items",
                  discrim_corr = TRUE), 1e-3)
  expect_lt(check(tv, model_type = 1, vary_ideal_pts = "random_walk"), 1e-3)
  expect_lt(check(c(tv, time_process = "AR"), model_type = 1, vary_ideal_pts = "AR1"), 1e-3)
  expect_lt(check(c(tv, time_process = "GP"), model_type = 1, vary_ideal_pts = "GP"), 1e-3)
  expect_lt(check(c(tv, time_process = "splines"), model_type = 1, vary_ideal_pts = "splines"), 1e-3)
})

test_that("pinned items and persons identify the extra dimensions", {
  skip_if_no_cmdstan()

  set.seed(2)
  sim <- id_sim_gen(num_person = 30, num_items = 20, num_dim = 2)

  # items: the dimension-1 anchor has a discrimination of exactly 0 on dimension 2,
  # and the dimension-2 anchor a positive one
  fit <- fit_2d(sim, model_type = 1, const_type = "items")
  reg_d <- posterior::as_draws_matrix(fit@stan_samples$draws("sigma_reg_d"))
  expect_true(all(reg_d[, "sigma_reg_d[1,1]"] == 0))
  expect_gt(min(reg_d[, "sigma_reg_d[3,1]"]), 0)
  expect_equal(as.vector(fit@this_data$restrict_dim), 3)

  # persons: the dimension-2 anchor sits near (0, 2), the dimension-1 pins near 0 on dimension 2
  fit <- fit_2d(sim, model_type = 1)
  med <- function(v) median(posterior::as_draws_matrix(fit@stan_samples$draws(v)))
  expect_equal(med("L_full[3]"), 0, tolerance = 0.5)
  expect_equal(med("L_full_d[3,1]"), 2, tolerance = 0.5)
  expect_equal(med("L_full_d[1,1]"), 0, tolerance = 0.5)
  expect_equal(med("L_full_d[2,1]"), 0, tolerance = 0.5)

  # vb_full picks a different person for dimension 2
  fit <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 1, num_dim = 2, use_method = "pathfinder", seed = 1,
                fixtype = "vb_full")))
  expect_length(fit@this_data$restrict_dim, 1)
  expect_false(fit@this_data$restrict_dim %in% c(fit@this_data$restrict_high, fit@this_data$restrict_low))
})

test_that("summaries, predictions and plots handle two dimensions", {
  skip_if_no_cmdstan()

  set.seed(2)
  sim <- id_sim_gen(num_person = 30, num_items = 20, num_dim = 2, inflate = TRUE,
                    person_cov = "continuous", person_cov_effect = 1)
  fit <- fit_2d(sim, model_type = 2)

  s <- summary(fit)
  expect_equal(sort(unique(s$Dimension)), 1:2)
  expect_equal(nrow(s), 2 * 30)
  expect_true(all(grepl("^L_full_d\\[", s$`Parameter Name`[s$Dimension == 2])))

  items <- summary(fit, pars = "items")
  expect_equal(sum(items$Dimension == 2, na.rm = TRUE), 2 * 20)

  cov <- summary(fit, pars = "person_cov")
  expect_equal(cov$Dimension, 1:2)

  ep <- suppressMessages(id_post_pred(fit, draws = 10, type = "epred"))
  ep_all <- do.call(cbind, unlist(ep, recursive = FALSE))
  expect_equal(dim(ep_all), c(10, fit@this_data$N))
  expect_true(all(is.finite(ep_all)))

  me <- suppressMessages(id_me(fit, covariate = "person_cov", draws = 10))
  expect_true(all(is.finite(me$sum_ideal_effects$mean_est)))

  expect_s3_class(id_plot_persons(fit, dim = 2), "ggplot")
  expect_s3_class(id_plot_persons(fit, dim = 2, show_true = TRUE), "ggplot")
  expect_s3_class(id_plot_cov(fit, dim = 2), "ggplot")
  expect_error(id_plot_persons(fit, dim = 3), "only 2 dimension")
  expect_error(id_plot_persons(fit, dim = 2, item_plot = "1"), "dimension 1")

  ext <- id_extract(fit, extract_type = "persons")
  expect_equal(sum(grepl("^L_full_d", names(ext))), 30)

  # time-varying
  set.seed(3)
  sim <- id_sim_gen(num_person = 20, num_items = 24, time_points = 6, num_dim = 2)
  fit <- fit_2d(sim, model_type = 1, vary_ideal_pts = "random_walk")
  s <- summary(fit)
  expect_equal(nrow(s), 2 * 20 * 6)
  expect_s3_class(id_plot_persons_dyn(fit, dim = 2), "ggplot")
  expect_s3_class(id_plot_legis_var(fit, dim = 2), "ggplot")
})

test_that("two-dimensional ideal points are recovered", {
  skip_if_no_cmdstan()

  set.seed(11)
  sim <- id_sim_gen(num_person = 100, num_items = 60, num_dim = 2)
  truth <- cbind(sim@simul_data$true_person[, 1], sim@simul_data$true_person_d[, 1, 1])

  # pin persons close to (2, 0), (-2, 0) and (0, 2) in the true space
  fit <- suppressMessages(suppressWarnings(
    id_estimate(sim, model_type = 1, num_dim = 2, use_method = "mcmc", nchains = 2, ncores = 2,
                niters = 400, warmup = 400, seed = 11, fixtype = "prefix",
                restrict_ind_high = which.max(truth[, 1] - abs(truth[, 2])),
                restrict_ind_low = which.min(truth[, 1] + abs(truth[, 2])),
                restrict_ind_dim = which.max(truth[, 2] - abs(truth[, 1])))))

  s <- summary(fit)
  est <- sapply(1:2, function(d) {
    x <- s[s$Dimension == d, ]
    x$`Posterior Median`[order(as.integer(as.character(x$Person)))]
  })

  # each true dimension is a linear function of the estimated ones (the pins
  # don't match the true positions exactly, so allow a rotation and shift)
  r2 <- sapply(1:2, function(d) summary(lm(truth[, d] ~ est))$r.squared)
  expect_true(all(r2 > 0.6))
})

test_that("one-dimensional fits have no parameters for other dimensions", {
  skip_if_no_cmdstan()

  set.seed(1)
  fit <- suppressMessages(suppressWarnings(
    id_estimate(id_sim_gen(num_person = 20, num_items = 10), model_type = 1,
                use_method = "pathfinder", seed = 1, fixtype = "prefix",
                restrict_ind_high = 1, restrict_ind_low = 2)))
  expect_equal(.num_dim(fit), 1)
  expect_false(any(grepl("_d\\[", posterior::variables(fit@stan_samples$draws()))))
  expect_false("Dimension" %in% names(summary(fit)))
})
