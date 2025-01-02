#' Rollcall vote data for 114th Senate
#'
#' This rollcall vote object (see [pscl::rollcall()]) contains voting records for the 114th Senate in the US
#' Congress. Not all rollcalls are included, only those that had a 70-30 or closer split in the vote.
#' The data can be pre-processed via the [id_make()] function for estimation. 
#' See package vignette for details.
#'
#' @format A long data frame with one row for every vote cast by a Senator.
#' @source <http://www.voteview.com/>
"senate114"

#' Rollcall vote data for Delaware State Legislature
#'
#' This data frame contains the rollcall voting data for the Delaware state legislature from 1995 to present.
#' The data is in long format so that each row is one vote cast by a legislator. It includes a column, 
#' `group_id`, that lists a party for each legislator (D=Democrat, R=Republican,X=Independent).
#' 
#' The original data come from Boris Shor and Nolan McCarty (2002), "The Ideological Mapping of 
#' American Legislatures", American Political Science Review.
#'
#' @format A long data frame with one row for every vote cast by a legislator.
#' @source <https://www.cambridge.org/core/journals/american-political-science-review/article/ideological-mapping-of-american-legislatures/8E1192C22AA0B9F9B56167998A41CAB0>
"delaware"


