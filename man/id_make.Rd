% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Estimate.R
\name{id_make}
\alias{id_make}
\title{Create data to run IRT model}
\usage{
id_make(
  score_data = NULL,
  outcome_disc = "outcome_disc",
  outcome_cont = "outcome_cont",
  person_id = "person_id",
  item_id = "item_id",
  time_id = "time_id",
  group_id = "group_id",
  model_id = "model_id",
  ordered_id = "ordered_id",
  ignore_id = "ignore_id",
  simul_data = NULL,
  person_cov = NULL,
  item_cov = NULL,
  item_cov_miss = NULL,
  remove_cov_int = FALSE,
  unbounded = FALSE,
  exclude_level = NA,
  simulation = FALSE
)
}
\arguments{
\item{score_data}{A data frame in long form, i.e., one row in the data for each 
measured score or vote in the data or a \code{rollcall} data object from package \code{pscl}.}

\item{person_id}{Column name of the person/legislator ID index in \code{score_data}, 
default is \code{'person_id'}. Should be integer, character or factor.}

\item{item_id}{Column name of the item/bill ID index in \code{score_data}, 
default is \code{'item_id'}.  Should be integer, character or factor.}

\item{time_id}{Column name of the time values in \code{score_data}: 
optional, default is \code{'time_id'}. Should be a date or date-time class, but can be an integer
(i.e., years in whole numbers).}

\item{group_id}{Optional column name of a person/legislator group IDs (i.e., parties) in \code{score_data}. 
Optional, default is \code{'group_id'}. Should be integer, character or factor.}

\item{model_id}{Column name of the model/response types in the data.
Default is \code{"model_id"}. Only necessary if a model with multiple 
response types (i.e., binary + continuous outcomes). Must be a 
column with a series
of integers matching the model types in \code{\link{id_estimate}} 
showing which row of the data matches which outcome.}

\item{simul_data}{Optionally, data that has been generated by the \code{\link{id_sim_gen}} function.}

\item{person_cov}{A one-sided formula that specifies the covariates
in \code{score_data} that will be used to hierarchically model the person/legislator ideal points}

\item{item_cov}{A one-sided formula that specifies the covariates
in \code{score_data} that will be used to hierarchically model the 
item/bill discrimination parameters for the regular model}

\item{item_cov_miss}{A one-sided formula that specifies the covariates
in the dataset that will be used to hierarchically model the item/bill discrimination parameters for the
missing data model.}

\item{remove_cov_int}{Whether to remove constituent terms from hierarchical covariates that 
interact covariates with IDs like \code{person_id} or \code{item_id}. Set to \code{TRUE} if
including these constituent terms would cause multi-collinearity with other terms in the model
(such as running a group-level model with a group-level interaction or a person-level model
with a person-level interaction).}

\item{unbounded}{Whether or not the outcome/response is unbounded (i.e., continuous or
Poisson). If it is, \code{miss_val} 
is recoded as the maximum of the outcome + 1.}

\item{exclude_level}{A vector of any values that should be treated as \code{NA} in the response matrix. 
Unlike the \code{miss_val} parameter, these values will be dropped from the data before 
estimation rather than modeled explicitly.}

\item{simulation}{If \code{TRUE}, simulated values are saved in the \code{idealdata} object for 
later plotting with the \code{\link{id_plot_sims}} function}

\item{outcome}{Column name of the outcome in \code{score_data}, default is \code{"outcome"}}
}
\value{
A \code{idealdata} object that can then be used in the \code{\link{id_estimate}} function 
to fit a model.
}
\description{
To run an IRT model using \code{idealstan}, you must first process your data using the \code{id_make} 
function.
}
\details{
This function can accept either a \code{rollcall} data object from package
\code{pscl} or 
a long data frame where one row equals one item-person (bill-legislator)
observation with associated outcome. The preferred method is the long data frame 
as passing a long data frame permits
the inclusion of a wide range of covariates in the model, such as person-varying and item-varying 
(bill-varying) covariates. 
If a \code{rollcall} object is passed to the function, the \code{rollcall} data is converted
to a long data frame with data from the \code{vote.data} matrix used to determine dates for bills.
If passing a long data frame, you should specify the names of the 
columns containing the IDs for persons, items and 
groups (groups are IDs that may have multiple observations per ID, such as political parties or
classes) to the \code{id_make} function, along with the name of the response/outcome. 
The only required columns are the item/bill ID and the person/legislator ID along with an 
outcome column. 

The preferred format for the outcome column for discrete variables (binary or ordinal)
is to pass a factor variable with levels in the correct order, i.e., in ascending order.
For example, if using legislative data, the levels of the factor should be \code{c('No','Yes')}.
If a different kind of variable is passed, such as a character or numeric variable, 
you should consider specifying \code{low_val},\code{high_val} and \code{middle_val} to 
determine the correct order of the discrete outcome. Specifying \code{middle_val} is only
necessary if you are estimating an ordinal model.

If you do not specify a value for \code{miss_val}, then any \code{NA} are assumed to be 
missing. If you do specify \code{miss_val} and you also have \code{NA} in your data 
(assuming \code{miss_val} is not \code{NA}), then the function will treat the data
coded as \code{miss_val} as missing data that should be modeled and will treat the \code{NA}
data as ignorable missing data that will be removed (list-wise deletion) before estimating a
model.
}
\examples{
# You can either use a pscl rollcall object or a vote/score matrix 
# where persons/legislators are in the rows
# and items/bills are in the columns

library(dplyr)

# First, using a rollcall object with the 114th Senate's rollcall votes:

data('senate114')

to_idealstan <-   id_make(score_data = senate114,
               outcome = 'cast_code',
               person_id = 'bioname',
               item_id = 'rollnumber',
               group_id= 'party_code',
               time_id='date',
               high_val='Yes',
               low_val='No',
               miss_val='Absent')

}
