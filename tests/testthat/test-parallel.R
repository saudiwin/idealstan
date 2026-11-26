# Tests for parallel processing through the future package (.id_lapply).

options(cmdstanr_warn_inits = FALSE)

skip_if_no_cmdstan <- function() {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) skip("cmdstan not installed")
}

quiet <- function(x) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(suppressWarnings(x))))
  out
}

test_that("more than one core uses a multisession plan for that call only", {
  skip_on_cran()

  expect_true(inherits(future::plan(), "sequential"))
  expect_equal(.id_lapply(1:4, sqrt, cores = 2), lapply(1:4, sqrt))
  # the default plan is back afterwards
  expect_true(inherits(future::plan(), "sequential"))
  # one core and no plan: plain lapply
  expect_equal(.id_lapply(1:4, sqrt), lapply(1:4, sqrt))
})

test_that("predictions and summaries are the same in parallel", {
  skip_if_no_cmdstan()
  # forked workers share this session's copy of idealstan, so this also works
  # with pkgload::load_all() (multisession workers load the installed package)
  skip_if_not(parallelly::supportsMulticore(), "forked workers aren't available here")

  set.seed(1)
  sim <- id_sim_gen(num_person = 20, num_items = 10, inflate = TRUE,
                    person_cov = "continuous", person_cov_effect = 1)
  fit <- quiet(id_estimate(sim, model_type = 2, use_method = "pathfinder", seed = 1,
                           fixtype = "prefix", restrict_ind_high = 1, restrict_ind_low = 2))

  epred <- quiet(id_post_pred(fit, draws = 1:20, type = "epred"))
  ll <- quiet(id_post_pred(fit, type = "log_lik"))
  items <- quiet(summary(fit, pars = "items"))

  # a plan set by the user is used whatever the number of cores, and left in place
  old <- future::plan(future::multicore, workers = 2)
  on.exit(future::plan(old), add = TRUE)

  expect_equal(quiet(id_post_pred(fit, draws = 1:20, type = "epred")), epred)
  expect_equal(quiet(id_post_pred(fit, type = "log_lik")), ll)
  expect_equal(quiet(summary(fit, pars = "items")), items)
  expect_equal(length(unlist(quiet(id_post_pred(fit, draws = 5)))), 5 * fit@this_data$N)
  expect_true(inherits(future::plan(), "multicore"))
})
