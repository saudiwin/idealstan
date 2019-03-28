#' Create data to run IRT model
#' 
#' To run an IRT model using \code{idealstan}, you must first process your data using the \code{id_make} 
#' function. 
#' 
#' @details This function can accept either a \code{rollcall} data object from package
#' \code{pscl} or 
#' a long data frame where one row equals one item-person (bill-legislator)
#' observation with associated outcome. The preferred method is the long data frame 
#' as passing a long data frame permits
#' the inclusion of a wide range of covariates in the model, such as person-varying and item-varying 
#' (bill-varying) covariates. 
#' If a \code{rollcall} object is passed to the function, the \code{rollcall} data is converted
#' to a long data frame with data from the \code{vote.data} matrix used to determine dates for bills.
#' If passing a long data frame, you should specify the names of the 
#' columns containing the IDs for persons, items and 
#' groups (groups are IDs that may have multiple observations per ID, such as political parties or
#' classes) to the \code{id_make} function, along with the name of the response/outcome. 
#' The only required columns are the item/bill ID and the person/legislator ID along with an 
#' outcome column. 
#' 
#' The preferred format for the outcome column for discrete variables (binary or ordinal)
#' is to pass a factor variable with levels in the correct order, i.e., in ascending order.
#' For example, if using legislative data, the levels of the factor should be \code{c('No','Yes')}.
#' If a different kind of variable is passed, such as a character or numeric variable, 
#' you should consider specifying \code{low_val},\code{high_val} and \code{middle_val} to 
#' determine the correct order of the discrete outcome. Specifying \code{middle_val} is only
#' necessary if you are estimating an ordinal model.
#' 
#' If you do not specify a value for \code{miss_val}, then any \code{NA} are assumed to be 
#' missing. If you do specify \code{miss_val} and you also have \code{NA} in your data 
#' (assuming \code{miss_val} is not \code{NA}), then the function will treat the data
#' coded as \code{miss_val} as missing data that should be modeled and will treat the \code{NA}
#' data as ignorable missing data that will be removed (list-wise deletion) before estimating a
#' model.
#' 
#' @section Time-Varying Models
#' 
#' To run a time-varying model, you need to include the name of a column with dates (or integers) that is passed 
#' to the \code{time_id} option.
#' 
#' @section Continuous Outcomes
#' 
#' If the outcome is unbounded i.e. a continuous or an unbounded 
#' discrete variable like Poisson, simply set \code{unbounded} to \code{TRUE}. You can ignore the
#' options that specify which values should be \code{high_val} or \code{low_val}. You can either specify
#' a particular value as missing using \code{miss_val}, or all 
#' missing values (\code{NA}) will be recoded to a specific value out of the range of the outcome to use
#' for modeling the missingness.
#' 
#' @section Hierarchical Covariates
#' 
#' Covariates can be fit on the person-level ideal point parameters as well as
#' item discrimination parameters for either the inflated (missing) or non-inflated (observed) 
#' models. These covariates must be columns that were included with the data fed to the 
#' \code{\link{id_make}} function. The covariate relationships are specified as 
#' one-sided formulas, i.e. \code{~cov1 + cov2 + cov1*cov2}. To interact covariates with the 
#' person-level ideal points you can use \code{~cov1 + person_id + cov1*person_id} and for
#' group-level ideal poins you can use \code{~cov1 + group_id + cov1*group_id} where
#' \code{group_id} or \code{person_id} is the same name as the name of the column 
#' for these options that you passed to \code{id_make} (i.e., the names of the columns
#' in the original data).
#' 
#' @param score_data A data frame in long form, i.e., one row in the data for each 
#' measured score or vote in the data or a \code{rollcall} data object from package \code{pscl}.
#' @param outcome Column name of the outcome in \code{score_data}, default is \code{"outcome"}
#' @param person_id Column name of the person/legislator ID index in \code{score_data}, 
#' default is \code{'person_id'}. Should be integer, character or factor.
#' @param item_id Column name of the item/bill ID index in \code{score_data}, 
#' default is \code{'item_id'}.  Should be integer, character or factor.
#' @param time_id Column name of the time values in \code{score_data}: 
#' optional, default is \code{'time_id'}. Should be a date or date-time class, but can be an integer
#' (i.e., years in whole numbers).
#' @param group_id Optional column name of a person/legislator group IDs (i.e., parties) in \code{score_data}. 
#' Optional, default is \code{'group_id'}. Should be integer, character or factor.
#' @param person_cov A one-sided formula that specifies the covariates
#' in \code{score_data} that will be used to hierarchically model the person/legislator ideal points
#' @param item_cov A one-sided formula that specifies the covariates
#' in \code{score_data} that will be used to hierarchically model the 
#' item/bill discrimination parameters for the regular model
#' @param item_cov_miss A one-sided formula that specifies the covariates
#' in the dataset that will be used to hierarchically model the item/bill discrimination parameters for the
#' missing data model.
#' @param remove_cov_int Whether to remove constituent terms from hierarchical covariates that 
#' interact covariates with IDs like \code{person_id} or \code{item_id}. Set to \code{TRUE} if
#' including these constituent terms would cause multi-collinearity with other terms in the model
#' (such as running a group-level model with a group-level interaction or a person-level model
#' with a person-level interaction).
#' @param simul_data Optionally, data that has been generated by the \code{\link{id_sim_gen}} function.
#' @param miss_val The value (numeric or character) that indicate missing data/absences in the data. 
#' If missing data is coded as \code{NA}, 
#'  simply leave this parameter at the default, \code{NA}.
#' @param high_val The value (numeric or character) that indicate the highest discrete outcome possible, 
#' such as yes in a vote dataset or correct in a test examination.
#' @param low_val The value (numeric or character) that indicates the lowest discrete outcome possible, 
#' such as no votes in a vote dataset or incorrect in a test examination.
#' @param middle_val The value (numeric or character) that indicate values 
#' between the lowest and highest categories, such as abstention in voting data or 
#' "Neither Agree nor Disagree" in Likert scales.
#'  If there are multiple possible values, 
#'  pass along a numeric or character vector of all such values in correct order (lower to higher values).
#'  If there are no middle values (binary outcome), leave empty.
#' @param unbounded Whether or not the outcome/response is unbounded (i.e., continuous or
#'  Poisson). If it is, \code{miss_val} 
#'  is recoded as the maximum of the outcome + 1. 
#' @param exclude_level A vector of any values that should be treated as \code{NA} in the response matrix. 
#' Unlike the \code{miss_val} parameter, these values will be dropped from the data before 
#' estimation rather than modeled explicitly.
#' @param simulation If \code{TRUE}, simulated values are saved in the \code{idealdata} object for 
#' later plotting with the \code{\link{id_plot_sims}} function
#' @return A \code{idealdata} object that can then be used in the \code{\link{id_estimate}} function 
#' to fit a model.
#' @export
#' @import rstan
#' @import dplyr
#' @importFrom tidyr gather spread
#' @import bayesplot
#' @import rstantools
#' @import Rcpp
#' @import methods
#' @importFrom stats dbinom median plogis quantile reorder rexp rlnorm runif sd step rnorm
#' @importFrom utils person
#' @useDynLib idealstan, .registration = TRUE
#' @examples 
#' # You can either use a pscl rollcall object or a vote/score matrix 
#' # where persons/legislators are in the rows
#' # and items/bills are in the columns
#' 
#' library(dplyr)
#' 
#' # First, using a rollcall object with the 114th Senate's rollcall votes:
#' 
#' data('senate114')
#' 
#' to_idealstan <-   id_make(score_data = senate114,
#'                outcome = 'cast_code',
#'                person_id = 'bioname',
#'                item_id = 'rollnumber',
#'                group_id= 'party_code',
#'                time_id='date',
#'                high_val='Yes',
#'                low_val='No',
#'                miss_val='Absent')
#' 
id_make <- function(score_data=NULL,
                    outcome='outcome',
                    person_id='person_id',
                    item_id='item_id',
                    time_id='time_id',
                    group_id='group_id',
                    simul_data=NULL,
                           person_cov=NULL,
                  item_cov=NULL,
                           item_cov_miss=NULL,
                  remove_cov_int=FALSE,
                           miss_val=NA,high_val=NULL,low_val=NULL,middle_val=NULL,
                    unbounded=FALSE,
                           exclude_level=NA,simulation=FALSE) {
  
  
  # make sure to ungroup it if it's a tidy data frame
  if('tbl' %in% class(score_data)) score_data <- ungroup(score_data)
  
  # if object is a rollcall, pre-process data into long form
  if('rollcall' %in% class(score_data)) {
    score_data <- .prepare_rollcall(score_data,item_id=item_id,
                                    time_id=time_id)
    
    time_id <- score_data$time_id
    item_id <- score_data$item_id
    score_data <- score_data$score_data
    
    miss_val <- 9
    low_val <- 6
    high_val <- 1
    exclude_level <- c(3,7)

  } 
  
    # data is already in long form
  
    # save original ID names as strings to filter later
  
    orig_id <- c(item_id,
                 group_id,
                 person_id)
  
    # test for input and quote
    outcome <- .check_quoted(outcome,quo(outcome))
    item_id <- .check_quoted(item_id,quo(item_id))
    time_id <- .check_quoted(time_id,quo(time_id))
    group_id <- .check_quoted(group_id,quo(group_id))
    person_id <- .check_quoted(person_id,quo(person_id))

  
  
  # rename data
  # IDs are made factors now so we can re-arrange indices when need be
  score_rename <- select(score_data,
                       outcome = !!outcome,
                       item_id = !!item_id,
                       person_id = !!person_id) %>% 
    mutate(item_id=factor(!! quo(item_id)),
           person_id=factor(!! quo(person_id)))
  
  # if time or group IDs don't exist, make dummies
  
  test_group <- try(factor(pull(score_data,!!group_id)),silent=TRUE)
  test_time <- try(pull(score_data,!!time_id),silent=TRUE)
  
  if(any('try-error' %in% class(test_group))) {
    score_rename$group_id <- factor("G")
  } else {
    score_rename$group_id <- test_group
  }
  if(any('try-error' %in% class(test_time))) {
    score_rename$time_id <- 1
  } else {
    score_rename$time_id <- test_time
  }
  
  # now we can make these all quosures again to use in NSE
  
  outcome <- quo(outcome)
  item_id <- quo(item_id)
  person_id <- quo(person_id)
  time_id <- quo(time_id)
  group_id <- quo(group_id)
  
  # see what the max time point is
  
  max_t <- max(as.numeric(factor(pull(score_rename,!!time_id))),na.rm=T)
  num_person <- max(as.numeric(factor(pull(score_rename,!!person_id))),na.rm=T)
  num_group <- try(max(as.numeric(factor(pull(score_rename,!!group_id)))))
  num_item <- max(as.numeric(factor(pull(score_rename,!!item_id))),na.rm=T)
  
  # create data frames for all hierachical parameters

  if(!is.null(person_cov)) {

    personm <- model.matrix(person_cov,data=score_data)
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(personm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(personm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(orig_id, function(c) {
      check_all <- grepl(x=colnames(personm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(personm),
                               pattern=":")
      check_all & !check_all_colon
    }) %>% apply(1,any)
    
    if(remove_cov_int) {
      personm <- personm[,!check_ids] 
    }

    score_rename <- bind_cols(score_rename,
                              as_data_frame(personm))
    person_cov <- dimnames(personm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$personcov0 <- 0
    person_cov <- 'personcov0'
  }
  
  if(!is.null(item_cov)) {
    
    itemm <- model.matrix(item_cov,data=score_data)
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(orig_id, function(c) {
      check_all <- grepl(x=colnames(itemm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(itemm),
                               pattern=":")
      check_all & !check_all_colon
    }) %>% apply(1,any)
    
    if(remove_cov_int) {
      itemm <- itemm[,!check_ids]
    }
    
    
    score_rename <- bind_cols(score_rename,
                              as_data_frame(itemm))
    item_cov <- dimnames(itemm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcov0 <- 0
    item_cov <- 'itemcov0'
  }
  
  if(!is.null(item_cov_miss)) {
    
    itemmissm <- model.matrix(item_cov_miss,data=score_data)
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemmissm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemmissm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(orig_id, function(c) {
      check_all <- grepl(x=colnames(itemmissm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(itemmissm),
                               pattern=":")
      check_all & !check_all_colon
    }) %>% apply(1,any)
    
    if(remove_cov_int) {
      itemmissm <- itemmissm[,!check_ids]
    }

    score_rename <- bind_cols(score_rename,
                              as_data_frame(itemmissm))
    item_cov_miss <- dimnames(itemmissm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcovmiss0 <- 0
    item_cov_miss <- 'itemcovmiss0'
  }
  
  # recode score/outcome
  if(!unbounded) {
    
    if(is.na(miss_val)) {
      score_rename$outcome[is.na(score_rename$outcome)] <- 'Missing'
      miss_val <- 'Missing'
    }
    
    if(!is.null(high_val) && !is.null(low_val) && !is.null(middle_val)) {

      score_rename$outcome <- factor(score_rename$outcome,
                                     levels=c(low_val,middle_val,high_val,miss_val),
                                     exclude = exclude_level)
    } else if(!is.null(high_val) && !is.null(low_val)) {
      score_rename$outcome <- factor(score_rename$outcome,
                                     levels=c(low_val,high_val,miss_val),
                                     exclude = exclude_level)
    } else {
      # variable does not need to be recoded, only move missing to the end
      score_rename$outcome <- factor(score_rename$outcome)
      score_rename$outcome <- fct_relevel(score_rename$outcome,as.character(miss_val),after=Inf)
    }
  } 


  if(unbounded) {
    max_val <- max(pull(score_rename,!!outcome))
    # make missing data the highest observed value
    if(is.na(miss_val)) {
      score_rename <- mutate(score_rename,!! quo_name(outcome) := coalesce(is.na(!!outcome),max_val+1L))
    } else {
      score_rename <- mutate(score_rename,!! quo_name(outcome) := ifelse(!!outcome==miss_val,max_val+1L,!!outcome))
    }
    miss_val <- max_val+1L
  } 
  

  outobj <- new('idealdata',
      score_matrix=score_rename,
      person_cov=person_cov,
      item_cov=item_cov,
      item_cov_miss=item_cov_miss,
      miss_val=miss_val)
  
  if(simulation==TRUE) {
    outobj@simul_data <- simul_data
    outobj@simulation <- simulation
  }
  return(outobj)
}

#' Estimate an \code{idealstan} model
#' 
#' This function will take a pre-processed \code{idealdata} vote/score dataframe and 
#' run one of the available IRT/latent space ideal point models on the data using
#' Stan's MCMC engine.
#' 
#' To run an IRT ideal point model, you must first pre-process your data using the \code{\link{id_make}} function. Be sure to specify the correct options for the
#' kind of model you are going to run: if you want to run an unbounded outcome (i.e. Poisson or continuous),
#' the data needs to be processed differently. Also any hierarchical covariates at the person or item level
#' need to be specified in \code{\link{id_make}}. If they are specified in \code{\link{id_make}}, than all 
#' subsequent models fit by this function will have these covariates.
#' 
#' \strong{Note that for static ideal point models, the covariates are only defined for those 
#' persons who are not being used as constraints.}
#' 
#' As of this version of \code{idealstan}, the following model types are available. Simply pass 
#' the number of the model in the list to the \code{model_type} option to fit the model.
#' 
#' \enumerate{
#'   \item IRT 2-PL (binary response) ideal point model, no missing-data inflation
#'   \item IRT 2-PL ideal point model (binary response) with missing- inflation
#'   \item Ordinal IRT (rating scale) ideal point model no missing-data inflation
#'   \item Ordinal IRT (rating scale) ideal point model with missing-data inflation
#'   \item Ordinal IRT (graded response) ideal point model no missing-data inflation
#'   \item Ordinal IRT (graded response) ideal point model with missing-data inflation
#'   \item Poisson IRT (Wordfish) ideal point model with no missing data inflation
#'   \item Poisson IRT (Wordfish) ideal point model with missing-data inflation
#'   \item unbounded (Gaussian) IRT ideal point model with no missing data
#'   \item unbounded (Gaussian) IRT ideal point model with missing-data inflation
#'   \item Positive-unbounded (Log-normal) IRT ideal point model with no missing data
#'   \item Positive-unbounded (Log-normal) IRT ideal point model with missing-data inflation
#'   \item Latent Space (binary response) ideal point model with no missing data
#'   \item Latent Space (binary response) ideal point model with missing-data inflation
#' }
#' 
#' @section Time-Varying Inferece
#' 
#' In addition, each of these models can have time-varying ideal point (person) parameters if
#' a column of dates is fed to the \code{\link{id_make}} function. If the option \code{vary_ideal_pts} is 
#' set to \code{'random_walk'}, \code{id_estimate} will estimate a random-walk ideal point model where ideal points 
#' move in a random direction. If \code{vary_ideal_pts} is set to \code{'AR1'}, a stationary ideal point model 
#' is estimated where ideal points fluctuate around long-term mean. If \code{vary_ideal_pts} 
#' is set to \code{'GP'}, then a semi-parametric Gaussian process time-series prior will be put
#' around the ideal points. Please see the package vignette and associated paper for more detail
#' about these time-varying models. Both the \code{'AR1'} and \code{'GP'} models can also 
#' accept time-varying covariates.
#' 
#' @section Missing Data
#' 
#' The inflation model used to account for missing data assumes that missingness is a 
#' function of the persons' (legislators')
#' ideal points. In other words,the model will take into account if people with high or low ideal points
#' tend to have more/less missing data on a specific item/bill. Missing data is whatever was 
#' passed as \code{miss_val} to the \code{\link{id_make}} function. 
#' If there isn't any relationship
#' between missing data and ideal points, then the model assumes that the missingness is ignorable 
#' conditional on each
#' item, but it will still adjust the results to reflect these ignorable (random) missing
#' values. The inflation is designed to be general enough to handle a wide array of potential
#' situations where strategic social choices make missing data important to take into account.
#' 
#' The missing data is assumed to be any possible value of the outcome. The well-known 
#' zero-inflated Poisson model is a special case where missing values are known to be all zeroes. 
#' To fit a zero-inflated Poisson model, change \code{inflate_zeroes} to \code{TRUE} and also 
#' make sure to set the value for zero as \code{miss_val} in the \code{\link{id_make}} function.
#' This will only work for outcomes that are distributed as Poisson variables (i.e., 
#' unbounded integers or counts).
#' 
#' To leave missing data out of the model, simply choose a version of the model in the list above
#' that is non-inflated.
#' 
#' Models can be either fit on the person/legislator IDs or on group-level IDs (as specified to the 
#' \code{id_make} function). If group-level parameters should be fit, set \code{use_groups} to \code{TRUE}.
#' 
#' @section Covariates
#' 
#' Covariates are included in the model if they were specified as options to the 
#' \code{\link{id_make}} function. The covariate plots can be accessed with 
#' \code{\link{id_plot_cov}} on a fitted \code{idealstan} model object.
#' 
#' @section Identification:
#' Identifying IRT models is challenging, and ideal point models are still more challenging 
#' because the discrimination parameters are not constrained.
#' As a result, more care must be taken to obtain estimates that are the same regardless of starting values. 
#' The parameter \code{fixtype} enables you to change the type of identification used. The default, 'vb_full', 
#' does not require any further
#' information from you in order for the model to be fit. In this version of identification, 
#' an unidentified model is run using
#' variational Bayesian inference (see \code{\link[rstan]{vb}}). The function will then select two 
#' persons/legislators that end up on either end of the ideal point spectrum, and pin their ideal points
#' to those specific values. This is sufficient to identify all of the static models and also the AR(1) 
#' time-varying models. For random-walk time-varying models and Gaussian process
#' models, identification is more difficult (see vignette).
#' Setting the option \code{restrict_mean} to \code{TRUE} will implement additional identification 
#' constraints on random-walk models, and is always implemented on Gaussian process models. This 
#' option fixes the distance between high and low points of the time series based on a 
#' variational inference run to prevent oscillation in the time series.
#' A particularly convenient option for \code{fixtype} is \code{'vb_partial'}. In this case, the user
#' should pass the IDs (as a character vector) of the persons to constrain high (\code{restrict_ind_high}) and
#' low (\code{restrict_ind_low}). A model will then be fit to find the likely positions of these parameters,
#' which will then be used to fit an identified model. In this way, the user can achieve a certain
#' shape of the ideal point distribution without needing to choose specific values ahead of time to pin
#' parameters to.
#' If a prior model has been estimated with the same data, the user can re-use those identification 
#' settings by passing the fitted \code{idealstan} object to the \code{prior_fit} option.
#' @param idealdata An object produced by the \code{\link{id_make}} containing a score/vote matrix for use for estimation & plotting
#' @param model_type An integer reflecting the kind of model to be estimated. See below.
#' @param inflate_zero If the outcome is distributed as Poisson (count/unbounded integer), 
#' setting this to 
#' \code{TRUE} will fit a traditional zero-inflated model. To use correctly, the value for 
#' zero must be passed as the \code{miss_val} option to \code{\link{id_make}} before
#' running a model so that zeroes are coded as missing data.
#' @param vary_ideal_pts Default \code{'none'}. If \code{'random_walk'}, \code{'AR1'} or 
#' \code{'GP'}, a 
#' time-varying ideal point model will be fit with either a random-walk process, an 
#' AR1 process or a Gaussian process. See documentation for more info.
#' @param use_subset Whether a subset of the legislators/persons should be used instead of the full response matrix
#' @param sample_it Whether or not to use a random subsample of the response matrix. Useful for testing.
#' @param subset_group If person/legislative data was included in the \code{\link{id_make}} function, then you can subset by
#' any value in the \code{$group} column of that data if \code{use_subset} is \code{TRUE}.
#' @param subset_person A list of character values of names of persons/legislators to use to subset if \code{use_subset} is 
#' \code{TRUE} and person/legislative data was included in the \code{\link{id_make}} function with the required \code{$person.names}
#' column
#' @param sample_size If \code{sample_it} is \code{TRUE}, this value reflects how many legislators/persons will be sampled from
#' the response matrix
#' @param nchains The number of chains to use in Stan's sampler. Minimum is one. See \code{\link[rstan]{stan}} for more info.
#' @param niters The number of iterations to run Stan's sampler. Shouldn't be set much lower than 500. See \code{\link[rstan]{stan}} for more info.
#' @param use_vb Whether or not to use Stan's variational Bayesian inference engine instead of full Bayesian inference. Pros: it's much faster.
#' Cons: it's not quite as accurate. See \code{\link[rstan]{vb}} for more info.
#' @param tol_rel_obj If \code{use_vb} is \code{TRUE}, this parameter sets the stopping rule for the \code{vb} algorithm. 
#' It's default is 0.0005. A stricter threshold will require the sampler to run longer but may yield a
#' better result in a difficult model with highly correlated parameters. Lowering the threshold should work fine for simpler
#' models.
#' @param warmup The number of iterations to use to calibrate Stan's sampler on a given model. Shouldn't be less than 100. 
#' See \code{\link[rstan]{stan}} for more info.
#' @param ncores The number of cores in your computer to use for parallel processing in the Stan engine. 
#' See \code{\link[rstan]{stan}} for more info.
#' @param fixtype Sets the particular kind of identification used on the model, could be one of 'vb_full' 
#' (identification provided exclusively by running a variational identification model with no prior info), 
#' 'vb_partial' (two indices of ideal points to fix are provided but the values to fix are determined by the
#' identification model), 
#' 'constrain' (two indices of ideal points to fix are provided--only sufficient for model if \code{restrict_var} is 
#' \code{FALSE}, 
#' and 'prior_fit' (a previous identified \code{idealstan} fit is passed to the \code{prior_fit} option and used
#' as the basis for identification).
#'  See details for more information.
#' @param prior_fit If a previous \code{idealstan} model was fit \emph{with the same} data, then the same
#' identification constraints can be recycled from the prior fit if the \code{idealstan} object is passed 
#' to this option. Note that means that all identification options, like \code{restrict_var}, will also 
#' be the same
#' @param id_diff The fixed difference between the high/low person/legislator ideal points used to identify the model. 
#' Set at 4 as a standard value but can be changed to any arbitrary number without affecting model results besides re-scaling.
#' @param id_diff_high The fixed intercept of the high ideal point used to constrain the model. 
#' @param id_refresh The number of times to report iterations from the variational run used to 
#' identify models. Default is 0 (nothing output to console).
#' @param sample_stationary If \code{TRUE}, the AR(1) coefficients in a time-varying model will be 
#' sampled from an unconstrained space and then mapped back to a stationary space. Leaving this \code{TRUE} is 
#' slower but will work better when there is limited information to identify a model. If used, the
#' \code{ar_sd} parameter should be increased to 5 to allow for wider sampling in the unconstrained space.
#' @param ar_sd If an AR(1) model is used, this defines the prior scale of the Normal distribution. A lower number 
#' can help 
#' identify the model when there are few time points.
#' @param use_groups If \code{TRUE}, group parameters from the person/legis data given in \code{\link{id_make}} will be 
#'  estimated instead of individual parameters. 
#' @param restrict_ind_high If \code{fixtype} is not "vb", the particular indices of legislators/persons or bills/items to constrain high
#' @param restrict_ind_low If \code{fixtype} is not "vb", the particular indices of legislators/persons or bills/items to constrain low. 
#' (Note: not used if values are pinned).
#' @param discrim_reg_sd Set the prior standard deviation of the bimodal prior for the discrimination parameters for the non-inflated model.
#' @param discrim_miss_sd Set the prior standard deviation of the bimodal prior for the discrimination parameters for the inflated model.
#' @param person_sd Set the prior standard deviation for the legislators (persons) parameters
#' @param time_sd The precision (inverse variance) of the over-time component of the person/legislator
#' parameters. A higher value will allow for less over-time variation (useful if estimates bounce too much). 
#' Default is 4.
#' @param diff_reg_sd Set the prior standard deviation for the bill (item) intercepts for the non-inflated model.
#' @param diff_miss_sd Set the prior standard deviation for the bill (item) intercepts for the inflated model.
#' @param restrict_sd Set the prior standard deviation for constrained parameters
#' @param restrict_var Whether to limit variance to no higher than 0.5 for random-walk time series models.
#' If left blank (the default), will be set to \code{TRUE} for random-walk models and \code{FALSE} for 
#' AR(1) models if identification is still a challenge (note: using this for AR(1) models is 
#' probably overkill).
#' @param restrict_var_high The upper limit for the variance parameter (if \code{restrict_var=TRUE} & 
#' model is a random-walk time-series). If left blank, either defaults to 0.1 or is set by 
#' identification model.
#' @param restrict_mean Whether or not to restrict the over-time mean of an ideal point 
#' (additional identification measure when standard fixes don't work). \code{TRUE} by 
#' default for random-walk models.
#' @param restrict_mean_val For random-walk models, the mean of a time-series ideal point to constrain.
#' Should not be set a priori (leave blank) unless you are absolutely sure. Otherwise it is set by 
#' the identification model.
#' @param restrict_mean_ind For random-walk models, the ID of the person/group whose over-time
#' mean to constrain. Should be left blank (will be set by identification model) unless you are 
#' really sure.
#' @param gp_sd_par The upper limit on allowed residual variation of the Gaussian process
#' prior. Increasing the limit will permit the GP to more closely follow the time points, 
#' resulting in much sharper bends in the function and potentially oscillation.
#' @param gp_num_diff The number of time points to use to calculate the length-scale prior
#' that determines the level of smoothness of the GP time process. Increasing this value
#' will result in greater smoothness/autocorrelation over time by selecting a greater number
#' of time points over which to calculate the length-scale prior.
#' @param gp_m_sd_par The upper limit of the marginal standard deviation of the GP time 
#' process. Decreasing this value will result in smoother fits.
#' @param gp_min_length The minimum value of the GP length-scale parameter. This is a hard
#' lower limit. Increasing this value will force a smoother GP fit. It should always be less than
#' \code{gp_num_diff}.
#' @param ... Additional parameters passed on to Stan's sampling engine. See \code{\link[rstan]{stan}} for more information.
#' @return A fitted \code{\link{idealstan}} object that contains posterior samples of all parameters either via full Bayesian inference
#' or a variational approximation if \code{use_vb} is set to \code{TRUE}. This object can then be passed to the plotting functions for further analysis.
#' @seealso \code{\link{id_make}} for pre-processing data,
#' \code{\link{id_plot_legis}} for plotting results,
#' \code{\link{summary}} for obtaining posterior quantiles,
#' \code{\link{posterior_predict}} for producing predictive replications.
#' @examples
#' # First we can simulate data for an IRT 2-PL model that is inflated for missing data
#' library(ggplot2)
#' library(dplyr)
#' 
#' # This code will take at least a few minutes to run 
#' \dontrun{
#' bin_irt_2pl_abs_sim <- id_sim_gen(model_type='binary',inflate=T)
#' 
#' # Now we can put that directly into the id_estimate function 
#' # to get full Bayesian posterior estimates
#' # We will constrain discrimination parameters 
#' # for identification purposes based on the true simulated values
#' 
#' bin_irt_2pl_abs_est <- id_estimate(bin_irt_2pl_abs_sim,
#'                        model_type=2,
#'                        restrict_ind_high = 
#'                        sort(bin_irt_2pl_abs_sim@simul_data$true_person,
#'                        decreasing=TRUE,
#'                        index=TRUE)$ix[1],
#'                        restrict_ind_low = 
#'                        sort(bin_irt_2pl_abs_sim@simul_data$true_person
#'                        decreasing=FALSE,
#'                        index=TRUE)$ix[1],
#'                        fixtype='vb_partial',
#'                        ncores=2,
#'                        nchains=2)
#'                                    
#' # We can now see how well the model recovered the true parameters
#' 
#' id_sim_coverage(bin_irt_2pl_abs_est) %>% 
#'          bind_rows(.id='Parameter') %>% 
#'          ggplot(aes(y=avg,x=Parameter)) +
#'            stat_summary(fun.args=list(mult=1.96)) + 
#'            theme_minimal()
#'  }
#' 
#' # In most cases, we will use pre-existing data 
#' # and we will need to use the id_make function first
#' # We will use the full rollcall voting data 
#' # from the 114th Senate as a rollcall object
#' 
#' data('senate114')
#' 
#' # Running this model will take at least a few minutes, even with 
#' # variational inference (use_vb=T) turned on
#' \dontrun{
#' 
#' to_idealstan <-   id_make(score_data = senate114,
#' outcome = 'cast_code',
#' person_id = 'bioname',
#' item_id = 'rollnumber',
#' group_id= 'party_code',
#' time_id='date',
#' high_val='Yes',
#' low_val='No',
#' miss_val='Absent')
#' 
#' sen_est <- id_estimate(senate_data,
#' model_type = 2,
#' use_vb = TRUE,
#' fixtype='vb_partial',
#' restrict_ind_high = "BARRASSO, John A.",
#' restrict_ind_low = "WARREN, Elizabeth")
#' 
#' # After running the model, we can plot 
#' # the results of the person/legislator ideal points
#' 
#' id_plot_legis(sen_est)
#' }
#' 
#' @references \enumerate{
#'    \item Clinton, J., Jackman, S., & Rivers, D. (2004). The Statistical Analysis of Roll Call Data. \emph{The American Political Science Review}, 98(2), 355-370. doi:10.1017/S0003055404001194
#'    \item Bafumi, J., Gelman, A., Park, D., & Kaplan, N. (2005). Practical Issues in Implementing and Understanding Bayesian Ideal Point Estimation. \emph{Political Analysis}, 13(2), 171-187. doi:10.1093/pan/mpi010
#'    \item Kubinec, R. "Generalized Ideal Point Models for Time-Varying and Missing-Data Inference". Working Paper.
#'    \item Betancourt, Michael. "Robust Gaussian Processes in Stan". (October 2017). Case Study.
#' }
#' @importFrom stats dnorm dpois model.matrix qlogis relevel rpois update
#' @importForm utils person
#' @export
id_estimate <- function(idealdata=NULL,model_type=2,
                        inflate_zero=FALSE,
                        vary_ideal_pts='none',
                        use_subset=FALSE,sample_it=FALSE,
                           subset_group=NULL,subset_person=NULL,sample_size=20,
                           nchains=4,niters=2000,use_vb=FALSE,
                           restrict_ind_high=NULL,
                          id_diff=4,
                        id_diff_high=2,
                           restrict_ind_low=NULL,
                           fixtype='vb_full',
                        id_refresh=0,
                        prior_fit=NULL,
                        warmup=floor(niters/2),ncores=4,
                        use_groups=FALSE,
                           discrim_reg_sd=2,
                           discrim_miss_sd=2,
                           person_sd=1,
                        time_sd=.1,
                        sample_stationary=FALSE,
                        ar_sd=2,
                           diff_reg_sd=1,
                           diff_miss_sd=1,
                           restrict_sd=0.01,
                        restrict_mean=NULL,
                        restrict_var=NULL,
                        restrict_mean_val=NULL,
                        restrict_mean_ind=NULL,
                        restrict_var_high=0.1,
                        tol_rel_obj=.0001,
                        gp_sd_par=.025,
                        gp_num_diff=c(3,0.01),
                        gp_m_sd_par=c(0.3,10),
                        gp_min_length=0,
                           ...) {


  
  if(use_subset==TRUE || sample_it==TRUE) {
    idealdata <- subset_ideal(idealdata,use_subset=use_subset,sample_it=sample_it,subset_group=subset_group,
                              subset_person=subset_person,sample_size=sample_size)
  }
  

    idealdata@stanmodel <- stanmodels[['irt_standard']]
    
  # currently fixed at one param, can change in future versions
    
  nfix <- 1
   
    #Using an un-identified model with variational inference, find those parameters that would be most useful for
    #constraining/pinning to have an identified model for full Bayesian inference
  
  # change time IDs if non time-varying model is being fit
  if(vary_ideal_pts=='none') {
    idealdata@score_matrix$time_id <- 1
  } 
  
  vary_ideal_pts <- switch(vary_ideal_pts,
                           none=1,
                           random_walk=2,
                           AR1=3,
                           GP=4)
  
  # set GP parameters
  
  if(vary_ideal_pts==4) {
    # convert multiplicity factor to total length of the data
    # use real time points instead of just counting number of points
    gp_num_diff[1] <- (max(as.numeric(idealdata@score_matrix$time_id))-
                         min(as.numeric(idealdata@score_matrix$time_id)))*gp_num_diff[1]
  }
    
  # use either row numbers for person/legislator IDs or use group IDs (static or time-varying)
      
  if(use_groups==T) {
    legispoints <- as.numeric(idealdata@score_matrix$group_id)
    num_legis <- max(legispoints)
  } else {
    legispoints <- as.numeric(idealdata@score_matrix$person_id)
    num_legis <- max(legispoints)
  }

  billpoints <- as.numeric(idealdata@score_matrix$item_id)
  timepoints <- as.numeric(factor(idealdata@score_matrix$time_id))
  # for gaussian processes, need actual time values
  time_ind <- switch(class(idealdata@score_matrix$time_id)[1],
                     factor=unique(as.numeric(idealdata@score_matrix$time_id)),
                     Date=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXct=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXlt=unique(as.numeric(idealdata@score_matrix$time_id)),
                     numeric=unique(idealdata@score_matrix$time_id),
                     integer=unique(idealdata@score_matrix$time_id))
  
  # now need to generate max/min values for empirical length-scale prior in GP
  if(gp_min_length>=gp_num_diff[1]) {
    stop('The parameter gp_min_length cannot be equal to or greater than gp_num_diff[1].')
  }
  
  max_t <- max(timepoints,na.rm=T)
  num_bills <- max(billpoints,na.rm=T)

  Y <- idealdata@score_matrix$outcome
  
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(model_type %in% c(1,3,5,7,9,11,13) & !is.na(idealdata@miss_val)) {
    Y <- .na_if(Y,idealdata@miss_val)
  }
  
  # check to see if more values than there should be for the bernoulli model
  
  if(model_type %in% c(1,13) && length(table(Y))>2) {
    stop('Too many values in score matrix for a binary model. Choose a different model_type.')
  } else if(model_type %in% c(2,14) && length(table(Y))>3) {
    stop("Too many values in score matrix for a binary model. Choose a different model_type.")
  }

  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
    remove_nas <- !is.na(Y) & !is.na(legispoints) & !is.na(billpoints) & !is.na(timepoints)
    Y <- Y[remove_nas]

    legispoints <- legispoints[remove_nas]
    billpoints <- billpoints[remove_nas]
    timepoints <- timepoints[remove_nas]
    
  if(model_type>8 && model_type < 13) {
    N_cont <- length(Y)
    N_int <- 1L
    Y_cont <- as.numeric(Y)
    Y_int <- array(1L)
  } else {
    N_cont <- 1L
    N_int <- length(Y)
    Y_cont <- array(1)
    Y_int <- as.integer(Y)
  }
    
  # set identification options
    
  if(length(idealdata@restrict_var)==0 && is.null(prior_fit) && is.null(restrict_var)) {
      if(vary_ideal_pts %in% c(1,3)) {
        idealdata@restrict_var <- FALSE
      } else {
        idealdata@restrict_var <- TRUE
      }
  } else if(length(idealdata@restrict_var)>0 && !is.null(prior_fit)) {
    # reset if a prior fit is being used for ID
    if(!is.null(restrict_var)) {
      idealdata@restrict_var <- restrict_var
    } else {
      if(vary_ideal_pts %in% c(1,3)) {
        idealdata@restrict_var <- FALSE
      } else {
        idealdata@restrict_var <- TRUE
      } 
    }
  } else if(length(idealdata@restrict_var)==0 && !is.null(restrict_var)) {
    idealdata@restrict_var <- restrict_var
  }

  # add in restrictions if they weren't identified by VB
    
  if(length(idealdata@restrict_var_high)==0 && !is.null(restrict_var_high)) {
      idealdata@restrict_var_high <- restrict_var_high
  } else if(length(idealdata@restrict_var_high)==0 && is.null(restrict_var_high)) {
      idealdata@restrict_var_high <- 1
  }
    
  if(length(idealdata@restrict_mean_val)==0 && !is.null(restrict_mean_val)) {
      idealdata@restrict_mean_val <- restrict_mean_val
  } else if(length(idealdata@restrict_mean_val)==0 && is.null(restrict_mean_val)) {
    idealdata@restrict_mean_val <- c(1,1)
  }
    
  if(length(idealdata@restrict_mean_ind)==0 && !is.null(restrict_mean_ind)) {
      idealdata@restrict_mean_ind <- restrict_mean_ind
  } else if(length(idealdata@restrict_mean_ind)==0 && is.null(restrict_mean_ind)) {
    idealdata@restrict_mean_ind <- rep(1,8)
  }
    
  if(length(idealdata@restrict_mean)>0 && vary_ideal_pts==3 && !is.null(prior_fit)) {
    
    # reset if a prior time-varying fit is being used
    if(is.null(restrict_mean)) {
      idealdata@restrict_mean <- FALSE
    } else {
      idealdata@restrict_mean <- restrict_mean
    }
    
  } else if(length(idealdata@restrict_mean)==0 && !is.null(restrict_mean)) {
    idealdata@restrict_mean <- restrict_mean
      
  } else if(length(idealdata@restrict_mean)==0 && is.null(restrict_mean)) {
    if(vary_ideal_pts %in% c(1,3)) {
      idealdata@restrict_mean <- FALSE
    } else {
      idealdata@restrict_mean <- TRUE
    }
  }
    
  this_data <- list(N=length(Y),
                    N_cont=N_cont,
                    N_int=N_int,
                    Y_int=Y_int,
                    Y_cont=Y_cont,
                    T=max_t,
                    num_legis=num_legis,
                    num_bills=num_bills,
                    ll=legispoints,
                    bb=billpoints,
                    num_fix_high=as.integer(1),
                    num_fix_low=as.integer(1),
                    LX=length(idealdata@person_cov),
                    SRX=length(idealdata@item_cov),
                    SAX=length(idealdata@item_cov_miss),
                    legis_pred=as.matrix(select(idealdata@score_matrix,
                                                idealdata@person_cov)),
                    srx_pred=as.matrix(select(idealdata@score_matrix,
                                              idealdata@item_cov)),
                    sax_pred=as.matrix(select(idealdata@score_matrix,
                                              idealdata@item_cov_miss)),
                    time=timepoints,
                    model_type=model_type,
                    discrim_reg_sd=discrim_reg_sd,
                    discrim_abs_sd=discrim_miss_sd,
                    diff_reg_sd=diff_reg_sd,
                    diff_abs_sd=diff_miss_sd,
                    legis_sd=1,
                    restrict_sd=restrict_sd,
                    time_proc=vary_ideal_pts,
                    diff=idealdata@diff,
                    diff_high=idealdata@diff_high,
                    time_sd=time_sd,
                    ar_sd=ar_sd,
                    restrict_var=idealdata@restrict_var,
                    restrict_var_high=idealdata@restrict_var_high,
                    restrict_mean=idealdata@restrict_mean,
                    restrict_mean_val=idealdata@restrict_mean_val,
                    restrict_mean_ind=idealdata@restrict_mean_ind,
                    zeroes=inflate_zero,
                    time_ind=as.array(time_ind),
                    time_proc=vary_ideal_pts,
                    gp_sd_par=gp_sd_par,
                    num_diff=gp_num_diff,
                    m_sd_par=gp_m_sd_par,
                    min_length=gp_min_length,
                    id_refresh=id_refresh)

  idealdata <- id_model(object=idealdata,fixtype=fixtype,model_type=model_type,this_data=this_data,
                        nfix=nfix,restrict_ind_high=restrict_ind_high,
                        restrict_ind_low=restrict_ind_low,
                        ncores=ncores,
                        tol_rel_obj=tol_rel_obj,
                        use_groups=use_groups,
                        prior_fit=prior_fit)

  # if diff hasn't been set yet, set it
  
  if(length(idealdata@diff_high)==0) {
      idealdata@diff <- id_diff
      idealdata@diff_high <- id_diff_high
  }
  
  # now run an identified run
  # repeat data formation as positions of rows/columns may have shifted
  if(use_groups==T) {
    legispoints <- as.numeric(idealdata@score_matrix$group_id)
    num_legis <- max(legispoints)
  } else {
    legispoints <- as.numeric(idealdata@score_matrix$person_id)
    num_legis <- max(legispoints)
  }
  
  billpoints <- as.numeric(idealdata@score_matrix$item_id)
  timepoints <- as.numeric(factor(idealdata@score_matrix$time_id))
  # for gaussian processes, need actual time values
  time_ind <- switch(class(idealdata@score_matrix$time_id)[1],
                     factor=unique(as.numeric(idealdata@score_matrix$time_id)),
                     Date=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXct=unique(as.numeric(idealdata@score_matrix$time_id)),
                     POSIXlt=unique(as.numeric(idealdata@score_matrix$time_id)),
                     numeric=unique(idealdata@score_matrix$time_id),
                     integer=unique(idealdata@score_matrix$time_id))
  max_t <- max(timepoints,na.rm=T)
  num_bills <- max(billpoints,na.rm=T)
  
  Y <- idealdata@score_matrix$outcome
  
  # check to see if we need to recode missing values from the data if the model_type doesn't handle missing data
  if(model_type %in% c(1,3,5,7,9,11,13) & !is.na(idealdata@miss_val)) {
    Y <- .na_if(Y,idealdata@miss_val)
  }
  
  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
  remove_nas <- !is.na(Y) & !is.na(legispoints) & !is.na(billpoints) & !is.na(timepoints)
  Y <- Y[remove_nas]
  legispoints <- legispoints[remove_nas]
  billpoints <- billpoints[remove_nas]
  timepoints <- timepoints[remove_nas]
  
  if(model_type>8 && model_type < 13) {
    N_cont <- length(Y)
    N_int <- 1
    Y_cont <- as.numeric(Y)
    Y_int <- array(1L)
  } else {
    N_cont <- 1
    N_int <- length(Y)
    Y_cont <- array(1L)
    Y_int <- as.integer(Y)
  }
  
  this_data <- list(N=length(Y),
                    N_cont=N_cont,
                    N_int=N_int,
                    Y_int=Y_int,
                    Y_cont=Y_cont,
                    T=max_t,
                    num_legis=num_legis,
                    num_bills=num_bills,
                    ll=legispoints,
                    bb=billpoints,
                    num_fix_high=as.integer(1),
                    num_fix_low=as.integer(1),
                    LX=length(idealdata@person_cov),
                    SRX=length(idealdata@item_cov),
                    SAX=length(idealdata@item_cov_miss),
                    legis_pred=as.matrix(select(idealdata@score_matrix,
                                                idealdata@person_cov)),
                    srx_pred=as.matrix(select(idealdata@score_matrix,
                                              idealdata@item_cov)),
                    sax_pred=as.matrix(select(idealdata@score_matrix,
                                              idealdata@item_cov_miss)),
                    time=timepoints,
                    time_proc=vary_ideal_pts,
                    model_type=model_type,
                    discrim_reg_sd=discrim_reg_sd,
                    discrim_abs_sd=discrim_miss_sd,
                    diff_reg_sd=diff_reg_sd,
                    diff_abs_sd=diff_miss_sd,
                    legis_sd=person_sd,
                    restrict_sd=restrict_sd,
                    diff=idealdata@diff,
                    diff_high=idealdata@diff_high,
                    time_sd=time_sd,
                    ar_sd=ar_sd,
                    restrict_var=idealdata@restrict_var,
                    restrict_var_high=idealdata@restrict_var_high,
                    restrict_mean_val=idealdata@restrict_mean_val,
                    restrict_mean_ind=idealdata@restrict_mean_ind,
                    restrict_mean=idealdata@restrict_mean,
                    zeroes=inflate_zero,
                    time_ind=as.array(time_ind),
                    time_proc=vary_ideal_pts,
                    gp_sd_par=gp_sd_par,
                    num_diff=gp_num_diff,
                    m_sd_par=gp_m_sd_par,
                    min_length=gp_min_length,
                    id_refresh=id_refresh)

  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=ncores,
                         this_data=this_data,use_vb=use_vb,
                         tol_rel_obj=tol_rel_obj,
                         ...)
  
  outobj@model_type <- model_type
  outobj@time_proc <- vary_ideal_pts
  outobj@use_groups <- use_groups
  return(outobj)
  
}
