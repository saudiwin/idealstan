#' \code{idealstan} package
#'
#' R Interface to Stan for Item-Response Theory Ideal Point Models
#'
#' See the README on
#' \href{https://github.com/saudiwin/idealstan/blob/master/README.md}{GitHub}
#'
#' @docType package
#' @name idealstan
#' @aliases idealstan-package
#' @useDynLib idealstan, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom rstan sampling
#' @references \enumerate{
#'    \item Kubinec, Robert. Generalized Ideal Point Models for Time-Varying and Missing-Data Inference. Working Paper.
#'    \item Clinton, J., Jackman, S., & Rivers, D. (2004). The Statistical Analysis of Roll Call Data. \emph{The American Political Science Review}, 98(2), 355-370. doi:10.1017/S0003055404001194
#'    \item Bafumi, J., Gelman, A., Park, D., & Kaplan, N. (2005). Practical Issues in Implementing and Understanding Bayesian Ideal Point Estimation. \emph{Political Analysis}, 13(2), 171-187. doi:10.1093/pan/mpi010
#' }
NULL

## quiets concerns of R CMD check with dplyr and NSE
## temporary fix, will use standard evaluation in future versions
if(getRversion() >= "2.15.1")  utils::globalVariables(c('party',
                                                        'iter',
                                                        'Person',
                                                        "time",
                                                        "parameter",
                                                        "y_shock",
                                                        "Type",
                                                        "estimate",
                                                        "marg_neg",
                                                        "marg_pos",
                                                        "mean_est",
                                                        "low_est",
                                                        "high_est",
                                                        "time_label",
                                                        'bill_cov',
                                                        'param',
                                                        'value',
                                                        'param_id',
                                                        'obs_type',
                                                        'legis_plot',
                                                        'low_pt',
                                                        'high_pt',
                                                        'median_pt',
                                                        'person.names',
                                                        'this_model',
                                                        'group',
                                                        'legis',
                                                        'ideal_pts',
                                                        'bills',
                                                        'rownum',
                                                        'ci_type',
                                                        'ci_value',
                                                        'high_bill',
                                                        'low_bill',
                                                        'bill_type',
                                                        'bill_vote',
                                                        'median_bill',
                                                        'true_vals',
                                                        'Rhats',
                                                        'avg',
                                                        'Params',
                                                        'high',
                                                        'low',
                                                        'vote_count',
                                                        'parameters',
                                                        '50%',
                                                        '2.5%',
                                                        '25%',
                                                        '75%',
                                                        '97.5%',
                                                        'par_type',
                                                        'posterior_mean',
                                                        'posterior_median',
                                                        'posterior_sd',
                                                        'Prob.025',
                                                        'Prob.25',
                                                        'Prob.75',
                                                        'Prob.975',
                                                        'true_vals',
                                                        'vote_countchecking',
                                                        'Rd',
                                                        'cross-references'))

#' Function that provides additional check questions for package release
release_questions <- function() {
  c(
    "Have you run package_test.RMD?"
  )
}
