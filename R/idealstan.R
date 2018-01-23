#' \code{idealstan} package
#'
#' R Interface to Stan for Item-Response Theory Ideal Point Models
#'
#' See the README on
#' \href{https://github.com/saudiwin/idealstan/blob/master/README.md}{GitHub}
#'
#' @docType package
#' @name idealstan
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))