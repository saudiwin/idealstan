#' Generic function for drawing from the posterior predictive distribution
#' of an idealstan object.
#' Given a fitted idealstan object, this function will create posterior 
#' replications of the outcome variable (i.e. votes) for each posterior draw.
#' Note that the resulting file could become quite large if each posterior draw 
#' is used, this the default is set to only pull 100 replications, which is generally
#' enough for most uses.
#' @param object A fitted idealstan object
#' @param draws Number of draws to use from the total number of posterior draws (number of chains x number of iterations)
#' @param seed An optional seed to use for the replications to ensure reproducibility
#' @param sample_votes The number of votes to sample from the total number of votes. Note that with large IRT datasets, the number of votes grows exponentially as a function of legislators and bills
#' @return \code{posterior_predict} returns a matrix where the number of columns \eqn{K} is equal to the number of \code{draws}
#' and the number of rows \eqn{J} is equal to the number of votes (scores) for all bills (items) in the IRT model. The 
#' returned matrix will also have class "\code{ppd}" to indicate it contains draws from the posterior predictive distribution.
#'
#'
#' @examples
#' 
#' \donttest{
#' if (require("rstanarm")) {
#'   fit <- stan_glm(mpg ~ wt + am, data = mtcars)
#'   yrep <- posterior_predict(fit)
#'   all.equal(ncol(yrep), nobs(fit))
#'
#'   nd <- data.frame(wt = mean(mtcars$wt), am = c(0, 1))
#'   ytilde <- posterior_predict(fit, newdata = nd)
#'   all.equal(ncol(ytilde), nrow(nd))
#' }
#' }
#'
#' # Also see help("posterior_predict", package = "rstanarm")