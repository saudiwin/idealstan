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
#' @param model_id Column name of the model/response types in the data.
#' Default is \code{"model_id"}. Only necessary if a model with multiple 
#' response types (i.e., binary + continuous outcomes). Must be a 
#' column with a series
#' of integers matching the model types in \code{\link{id_estimate}} 
#' showing which row of the data matches which outcome.
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
#' @import dplyr
#' @importFrom tidyr gather spread
#' @import bayesplot
#' @import Rcpp
#' @import methods
#' @importFrom stats dbinom median plogis quantile reorder rexp rlnorm runif sd step rnorm
#' @importFrom utils person
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
                    outcome_disc='outcome_disc',
                    outcome_cont='outcome_cont',
                    person_id='person_id',
                    item_id='item_id',
                    time_id='time_id',
                    group_id='group_id',
                    model_id='model_id',
                    ordered_id="ordered_id",
                    ignore_id="ignore_id",
                    simul_data=NULL,
                    person_cov=NULL,
                    item_cov=NULL,
                    item_cov_miss=NULL,
                    remove_cov_int=FALSE,
                    unbounded=FALSE,
                    exclude_level=NA,simulation=FALSE) {
  
  # only allow  missing values as NA
  
  miss_val <- c(NA,NA)
  
  high_val <- NULL
  middle_val <- NULL
  low_val <- NULL
  
  
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
  outcome_disc <- .check_quoted(outcome_disc,quo(outcome_disc))
  outcome_cont <- .check_quoted(outcome_cont,quo(outcome_cont))
  ordered_id <- .check_quoted(ordered_id,quo(ordered_id))
  item_id <- .check_quoted(item_id,quo(item_id))
  time_id <- .check_quoted(time_id,quo(time_id))
  group_id <- .check_quoted(group_id,quo(group_id))
  person_id <- .check_quoted(person_id,quo(person_id))
  model_id <- .check_quoted(model_id,quo(model_id))
  
  
  # rename data
  # IDs are made factors now so we can re-arrange indices when need be
  score_rename <- select(score_data,
                         item_id = !!item_id,
                         person_id = !!person_id) %>% 
    mutate(item_id=factor(!! quo(item_id)),
           person_id=factor(!! quo(person_id)))
  
  # need to test model number one first
  
  test_model <- try(pull(score_data,!!model_id),silent=TRUE)
  
  if(any('try-error' %in% class(test_model))) {
    score_rename$model_id <- "missing"
  } else {
    score_rename$model_id <- test_model
  }
  
  # if time or group IDs don't exist, make dummies
  
  test_group <- try(factor(pull(score_data,!!group_id)),silent=TRUE)
  test_time <- try(pull(score_data,!!time_id),silent=TRUE)
  test_out_disc <- try(factor(pull(score_data,!!outcome_disc)),silent=TRUE)
  test_out_cont <- try(pull(score_data,!!outcome_cont),silent=TRUE)
  test_ordered <- try(pull(score_data,!!ordered_id),silent=TRUE)
  
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
  
  if(any('try-error' %in% class(test_out_disc))) {
    
  } else {
    score_rename$outcome_disc <- test_out_disc
  }
  
  if(any('try-error' %in% class(test_out_cont))) {
    
  } else {
    score_rename$outcome_cont <- test_out_cont
  }
  
  if(any("try_error" %in% class(test_ordered))) {
    
  } else {
    score_rename$ordered_id <- test_ordered
  }
  
  
  # now we can make these all quosures again to use in NSE
  
  outcome_disc <- quo(outcome_disc)
  outcome_cont <- quo(outcome_cont)
  item_id <- quo(item_id)
  person_id <- quo(person_id)
  time_id <- quo(time_id)
  group_id <- quo(group_id)
  model_id <- quo(model_id)
  ordered_id <- quo(ordered_id)
  
  # see what the max time point is
  
  max_t <- max(as.numeric(factor(pull(score_rename,!!time_id))),na.rm=T)
  num_person <- max(as.numeric(factor(pull(score_rename,!!person_id))),na.rm=T)
  num_group <- try(max(as.numeric(factor(pull(score_rename,!!group_id)))))
  num_item <- max(as.numeric(factor(pull(score_rename,!!item_id))),na.rm=T)
  
  # create data frames for all hierachical parameters
  
  if(!is.null(person_cov)) {
    
    # drop intercept
    
    personm <- model.matrix(person_cov,data=score_data)[,-1,drop=FALSE]
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(personm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(personm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(unique(score_rename$person_id), function(c) {
      check_all <- grepl(x=colnames(personm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(personm),
                               pattern=paste0(c,":","|",":",c))
      matrix(check_all & !check_all_colon,nrow=length(check_all))
    },simplify = F) %>% 
      do.call(rbind, .) %>% 
      apply(2,any)
    
    if(remove_cov_int) {
      personm <- personm[,!check_ids,drop=F] 
    }
    
    score_rename <- bind_cols(score_rename,
                              as_tibble(personm))
    person_cov <- dimnames(personm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$personcov0 <- 0
    person_cov <- 'personcov0'
  }
  
  if(!is.null(item_cov)) {
    
    itemm <- model.matrix(item_cov,data=score_data)[,-1,drop=F]
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(unique(score_rename$item_id), function(c) {
      check_all <- grepl(x=colnames(itemm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(itemm),
                               pattern=paste0(c,":","|",":",c))
      matrix(check_all & !check_all_colon,nrow=length(check_all))
    },simplify = F) %>% 
      do.call(rbind, .) %>% 
      apply(2,any)
    
    if(remove_cov_int) {
      itemm <- itemm[,!check_ids]
    }
    
    
    score_rename <- bind_cols(score_rename,
                              as_tibble(itemm))
    item_cov <- dimnames(itemm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcov0 <- 0
    item_cov <- 'itemcov0'
  }
  
  if(!is.null(item_cov_miss)) {
    
    itemmissm <- model.matrix(item_cov_miss,data=score_data)[,-1,drop=FALSE]
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemmissm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemmissm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(unique(score_rename$item_id), function(c) {
      check_all <- grepl(x=colnames(itemmissm),
                         pattern=c)
      check_all_colon <- grepl(x=colnames(itemmissm),
                               pattern=paste0(c,":","|",":",c))
      matrix(check_all & !check_all_colon,nrow=length(check_all))
    },simplify = F) %>% 
      do.call(rbind, .) %>% 
      apply(2,any)
    
    if(remove_cov_int) {
      itemmissm <- itemmissm[,!check_ids]
    }
    
    score_rename <- bind_cols(score_rename,
                              as_tibble(itemmissm))
    item_cov_miss <- dimnames(itemmissm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcovmiss0 <- 0
    item_cov_miss <- 'itemcovmiss0'
  }
  
  # recode score/outcome
  if("outcome_disc" %in% names(score_rename)) {
    
    if(is.na(miss_val[1])) {
      # make NA a level, then change it
      score_rename$outcome_disc <- addNA(score_rename$outcome_disc)
      levels(score_rename$outcome_disc)[length(levels(score_rename$outcome_disc))] <- "Missing"
      miss_val[1] <- 'Missing'
    }
    
    if(!is.null(high_val) && !is.null(low_val) && !is.null(middle_val)) {
      
      score_rename$outcome_disc <- factor(score_rename$outcome_disc,
                                          levels=c(low_val,middle_val,high_val,miss_val[1]),
                                          exclude = exclude_level)
    } else if(!is.null(high_val) && !is.null(low_val)) {
      score_rename$outcome_disc <- factor(score_rename$outcome_disc,
                                          levels=c(low_val,high_val,miss_val[1]),
                                          exclude = exclude_level)
    } else {
      # variable does not need to be recoded, only move missing to the end
      score_rename$outcome_disc <- factor(score_rename$outcome_disc)
      score_rename$outcome_disc <- fct_relevel(score_rename$outcome_disc,as.character(miss_val[1]),after=Inf)
    }
    
    # reconvert if continuous values present
    if("outcome_cont" %in% names(score_rename)) {
      
      levels(score_rename$outcome_disc) <- c(levels(score_rename$outcome_disc),
                                             "Joint Posterior")
      
      score_rename$outcome_disc[score_rename$model_id>8 & score_rename$model_id<13] <- "Joint Posterior"
      
    }
    
  } 
  
  
  
  
  if("outcome_cont" %in% names(score_rename)) {
    
    if("outcome_disc" %in% names(score_rename)) {
      max_val <- max(pull(score_rename,!!outcome_cont)[score_rename$outcome_disc=="Joint Posterior"],na.rm=T)
    } else {
      max_val <- max(pull(score_rename,!!outcome_cont),na.rm=T)
    }
    
    # make missing data the highest observed value
    if(is.na(miss_val[2])) {
      
      if("outcome_disc" %in% names(score_rename)) {
        # only truly missing if discrete outcome also missing
        score_rename$outcome_cont[score_rename$outcome_disc=="Joint Posterior" & !is.na(score_rename$outcome_disc)] <- coalesce(score_rename$outcome_cont[score_rename$outcome_disc=="Joint Posterior" & !is.na(score_rename$outcome_disc)],max_val+1L)
        
        # need to set another value for not truly missing values for appended datasets
        score_rename$outcome_cont[score_rename$outcome_disc!="Joint Posterior"] <- max_val + 2L
      } else {
        
        score_rename <- mutate(score_rename,!! quo_name(outcome_cont) := coalesce(!!outcome_cont,max_val+1L))
      }
    } else {
      if("outcome_disc" %in% names(score_rename)) {
        score_rename <- mutate(score_rename,!! quo_name(outcome_cont) := ifelse(!!outcome_cont==miss_val[2] & outcome_disc=="Joint Posterior" &
                                                                                  !is.na(outcome_disc),max_val+1L,!!outcome_cont))
        # need to set another value for not truly missing values for appended datasets
        score_rename$outcome_cont[score_rename$outcome_disc!="Joint Posterior"] <- max_val + 2L
      } else {
        score_rename <- mutate(score_rename,!! quo_name(outcome_cont) := ifelse(!!outcome_cont==miss_val[2],max_val+1L,!!outcome_cont))
      }
      
    }
    
    miss_val[2] <- max_val + 1L
  } 
  
  
  
  # need to sort by integer vs. continuous
  score_rename$discrete <- as.numeric(score_rename$model_id %in% c(1,2,3,4,5,6,7,8,13,14))
  score_rename <- arrange(score_rename,desc(discrete),person_id,item_id)
  
  
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
#' persons/legislators or items/bills that end up on either end of the ideal point spectrum, 
#' and pin their ideal points
#' to those specific values. 
#' To control whether persons/legislator or items/bills are constrained,
#' the \code{const_type} can be set to either \code{"persons"} or 
#' \code{"items"} respectively. 
#' In many situations, it is prudent to select those persons or items 
#' ahead of time to pin to specific values. This allows the analyst to 
#' be more specific about what type of latent dimension is to be 
#' estimated. To do so, the \code{fixtype} option should be set to 
#' \code{"prefix"}. The values of the persons/items to be pinned can be passed
#' as character values to \code{restrict_ind_high} and 
#' \code{restrict_ind_low} to pin the high/low ends of the latent 
#' scale respectively. Note that these should be the actual data values 
#' passed to the \code{id_make} function. If you don't pass any values, 
#' you will see a prompt asking you to select certain values of persons/items.
#' 
#' The pinned values for persons/items are set by default to +1/-1, though
#' this can be changed using the \code{fix_high} and 
#' \code{fix_low} options. This pinned range is sufficient to identify 
#' all of the models
#' implemented in idealstan, though fiddling with some parameters may be 
#' necessary in difficult cases. For time-series models, one of the 
#' person ideal point over-time variances is also fixed to .1, a value that
#' can be changed using the option \code{time_fix_sd}.
#' @param idealdata An object produced by the \code{\link{id_make}} 
#' containing a score/vote matrix for use for estimation & plotting
#' @param model_type An integer reflecting the kind of model to be estimated. 
#' See below.
#' @param inflate_zero If the outcome is distributed as Poisson (count/unbounded integer), 
#' setting this to 
#' \code{TRUE} will fit a traditional zero-inflated model. 
#' To use correctly, the value for 
#' zero must be passed as the \code{miss_val} option to \code{\link{id_make}} before
#' running a model so that zeroes are coded as missing data.
#' @param ignore_db If there are multiple time periods (particularly when there are 
#' very many time periods), you can pass in a data frame
#' (or tibble) with one row per person per time period and an indicator column 
#' \code{ignore} that is equal to 1 for periods that should be considered in sample
#' and 0 for periods for periods that should be considered out of sample. This is 
#' useful for excluding time periods from estimation for persons when they could not 
#' be present, i.e. such as before entrance into an organization or following death.
#' If \code{ignore} equals 0, the person's ideal point is estimated as a standard Normal
#' draw rather than an auto-correlated parameter, reducing computational 
#' burden substantially.
#' Note that there can only be one pre-sample period of 0s, one in-sample period of 1s,
#' and one post-sample period of 0s. Multiple in-sample periods cannot be interspersed
#' with out of sample periods. The columns must be labeled as \code{person_id}, 
#' \code{time_id} and \code{ignore} and must match the formatting of the columns
#' fed to the \code{id_make} function.
#' @param keep_param A list with logical values for different categories of paremeters which
#' should/should not be kept following estimation. Can be any/all of \code{person_int} for 
#' the person-level intercepts (static ideal points), 
#' \code{person_vary} for person-varying ideal points,
#' \code{item} for observed item parameters (discriminations/intercepts),
#' \code{item_miss} for missing item parameters (discriminations/intercepts),
#' and \code{extra} for other parameters (hierarchical covariates, ordinal intercepts, etc.).
#' Takes the form \code{list(person_int=TRUE,person_vary=TRUE,item=TRUE,item_miss=TRUE,extra=TRUE)}.
#' If any are missing in the list, it is assumed that those parameters will be excluded.
#' If \code{NULL} (default), will save all parameters in output.
#' @param grainsize The grainsize parameter for the \code{reduce_sum} 
#' function used for within-chain parallelization. The default is 1, 
#' which means 1 chunk (item or person) per core. Set to -1. to use
#' @param map_over_id This parameter identifies which ID variable to use to construct the 
#' shards for within-chain parallelization. It defaults to \code{"persons"} but can also take
#' a value of \code{"items"}. It is recommended to select whichever variable has more
#' distinct values to improve parallelization.
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
#' It's default is 0.001. A stricter threshold will require the sampler to run longer but may yield a
#' better result in a difficult model with highly correlated parameters. Lowering the threshold should work fine for simpler
#' models.
#' @param warmup The number of iterations to use to calibrate Stan's sampler on a given model. Shouldn't be less than 100. 
#' See \code{\link[rstan]{stan}} for more info.
#' @param ncores The number of cores in your computer to use for parallel processing in the Stan engine. 
#' See \code{\link[rstan]{stan}} for more info. If \code{within_chain} is set to
#' \code{"threads"}, this parameter will determine the number of threads 
#' (independent processes) used for within-chain parallelization.
#' @param fixtype Sets the particular kind of identification used on the model, could be either 'vb_full' 
#' (identification provided exclusively by running a variational identification model with no prior info), or
#' 'prefix' (two indices of ideal points or items to fix are provided to 
#' options \code{restrict_ind_high} and \code{restrict_ind_low}).
#'  See details for more information.
#' @param prior_fit If a previous \code{idealstan} model was fit \emph{with the same} data, then the same
#' identification constraints can be recycled from the prior fit if the \code{idealstan} object is passed 
#' to this option. Note that means that all identification options, like \code{restrict_var}, will also 
#' be the same
#' @param mpi_export If \code{within_chains="mpi"}, this parameter should refer to the 
#' directory where the necessary data and Stan code will be exported to. If missing, 
#' an interactive dialogue will prompt the user for a directory. 
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
#' @param const_type Whether \code{"persons"} are the parameters to be 
#' fixed for identification (the default) or \code{"items"}. Each of these
#' pinned parameters should be specified to \code{fix_high} and \code{fix_low}
#' if \code{fixtype} equals \code{"prefix"}, otherwise the model will
#' select the parameters to pin to fixed values.
#' @param restrict_ind_high If \code{fixtype} is not "vb_full", the character value or numerical index
#' of a legislator/person or bill/item to pin to a high value (default +1).
#' @param restrict_ind_low If \code{fixtype} is not "vb_full", the character value or numerical index of a 
#' legislator/person or bill/item to pin to a low value (default -1). 
#' @param fix_high The value of which the high fixed ideal point/item should be
#' fixed to. Default is +1.
#' @param fix_low The value of which the high fixed ideal point/item should be
#' fixed to. Default is -1.
#' @param discrim_reg_sd Set the prior standard deviation of the bimodal prior for the discrimination parameters for the non-inflated model.
#' @param discrim_miss_sd Set the prior standard deviation of the bimodal prior for the discrimination parameters for the inflated model.
#' @param person_sd Set the prior standard deviation for the legislators (persons) parameters
#' @param time_fix_sd The variance of the over-time component of the first person/legislator
#' is fixed to this value as a reference. 
#' Default is 0.1.
#' @param boundary_prior If your time series has very low variance (change over time),
#' you may want to use this option to put a boundary-avoiding inverse gamma prior on
#' the time series variance parameters if your model has a lot of divergent transitions. 
#' To do so, pass a list with a element called 
#' \code{beta} that signifies the rate parameter of the inverse-gamma distribution. 
#' For example, try \code{boundary_prior=list(beta=1)}. Increasing the value of \code{beta}
#' will increase the "push" away from zero. Setting it too high will result in 
#' time series that exhibit a lot of "wiggle" without much need.
#' @param time_center_cutoff The number of time points above which
#' the model will employ a centered time series approach for AR(1)
#' and random walk models. Below this number the model will employ a 
#' non-centered approach. The default is 50 time points, which is 
#' relatively arbitrary and higher values may be better if sampling
#' quality is poor above the threshold.
#' @param diff_reg_sd Set the prior standard deviation for the bill (item) intercepts for the non-inflated model.
#' @param diff_miss_sd Set the prior standard deviation for the bill (item) intercepts for the inflated model.
#' @param restrict_sd_high Set the prior standard deviation for pinned parameters. This has a default of 
#' 0.01, but could be set lower if the data is really large.
#' @param restrict_sd_low Set the prior standard deviation for pinned parameters. This has a default of 
#' 0.01, but could be set lower if the data is really large.
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
#' @param cmdstan_path_user Default is NULL, and so will default to whatever is set in
#' \code{cmdstanr} package. Specify a file path  here to use a different \code{cmdtstan}
#' installation.
#' @param gpu Whether a GPU is available to speed computation (primarily for GP 
#' time-varying models).
#' @param save_files The location to save CSV files with MCMC draws from \code{cmdstanr}. 
#' The default is \code{NULL}, which will use a folder in the package directory.
#' @param pos_discrim Whether all discrimination parameters should be constrained to be
#' positive. If so, the model reduces to a traditional IRT model in which all items 
#' positively predict ability. Setting this to \code{TRUE} will also eliminate the need
#' to use other parameters for identification, though the options should still be 
#' specified to prevent errors.
#' @param het_var Whether to use a separate variance parameter for each item if using
#' Normal or Log-Normal distributions that have variance parameters. Defaults to TRUE and
#' should be set to FALSE only if all items have a similar variance.
#' @param compile_optim Whether to use Stan compile optimization flags (turn off if there
#'   are any compile issues)
#' @param debug For debugging purposes, turns off threading to enable more informative
#'   error messages from Stan. Also recompiles model objects.
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
#'                        sort(bin_irt_2pl_abs_sim@simul_data$true_person,
#'                        decreasing=FALSE,
#'                        index=TRUE)$ix[1],
#'                        fixtype='prefix',
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
#' sen_est <- id_estimate(to_idealstan,
#' model_type = 2,
#' use_vb = TRUE,
#' fixtype='prefix',
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
#' @importFrom utils person
#' @import cmdstanr
#' @export
id_estimate <- function(idealdata=NULL,model_type=2,
                        inflate_zero=FALSE,
                        vary_ideal_pts='none',
                        keep_param=NULL,
                        grainsize=1,
                        mpi_export=NULL,
                        use_subset=FALSE,sample_it=FALSE,
                        subset_group=NULL,subset_person=NULL,sample_size=20,
                        nchains=4,niters=1000,use_vb=FALSE,
                        ignore_db=NULL,
                        restrict_ind_high=NULL,
                        fix_high=1,
                        fix_low=(-1),
                        restrict_ind_low=NULL,
                        fixtype='prefix',
                        const_type="persons",
                        id_refresh=0,
                        prior_fit=NULL,
                        warmup=1000,ncores=4,
                        use_groups=FALSE,
                        discrim_reg_sd=2,
                        discrim_miss_sd=2,
                        person_sd=3,
                        time_fix_sd=.1,
                        time_var=10,
                        ar1_up=1,
                        ar1_down=0,
                        boundary_prior=NULL,
                        time_center_cutoff=50,
                        restrict_var=FALSE,
                        sample_stationary=FALSE,
                        ar_sd=1,
                        diff_reg_sd=1,
                        diff_miss_sd=1,
                        restrict_sd_high=0.01,
                        restrict_sd_low=0.01,
                        tol_rel_obj=.001,
                        gp_sd_par=.025,
                        gp_num_diff=3,
                        gp_m_sd_par=0.3,
                        gp_min_length=0,
                        cmdstan_path_user=NULL,
                        #gpu=FALSE,
                        map_over_id="persons",
                        save_files=NULL,
                        pos_discrim=FALSE,
                        het_var=TRUE,
                        compile_optim=FALSE,
                        debug=FALSE,
                        ...) {
  
  
  # check to make sure cmdstanr is working
  
  if(is.null(cmdstanr::cmdstan_version())) {
    print("You need to install cmdstan with cmdstanr to compile models. Use the function install_cmdstan() in the cmdstanr package.")
  }
  
  
  
  if(use_subset==TRUE || sample_it==TRUE) {
    idealdata <- subset_ideal(idealdata,use_subset=use_subset,sample_it=sample_it,subset_group=subset_group,
                              subset_person=subset_person,sample_size=sample_size)
  }
  
  # set path if user specifies
  if(!is.null(cmdstan_path_user))
    set_cmdstan_path(cmdstan_path_user)
  
  if(!file.exists(system.file("stan_files","irt_standard_map",
                              package="idealstan"))) {
    print("Compiling model. Will take some time as this is the first time the package has been used.")
    print("Have you thought about donating to relief for victims of Yemen's famine?")
    print("Check out https://www.unicef.org/emergencies/yemen-crisis for more info.")
  }
  
  # stan_code <- system.file("stan_files","irt_standard.stan",
  #                          package="idealstan")
  
  stan_code_map <- system.file("stan_files","irt_standard_map.stan",
                               package="idealstan")
  
  # stan_code_gpu <- system.file("stan_files","irt_standard_gpu.stan",
  #                              package="idealstan")
  # 
  if(compile_optim) {
    
    idealdata@stanmodel_map <- stan_code_map %>%
      cmdstan_model(include_paths=dirname(stan_code_map),
                    cpp_options = list(stan_threads = !debug,
                                       STAN_CPP_OPTIMS=TRUE),
                    force_recompile=debug)
    
    # idealdata@stanmodel_gpu <- stan_code_gpu %>%
    #   cmdstan_model(include_paths=dirname(stan_code_map),
    #                 cpp_options = list(stan_threads = !debug,
    #                                    STAN_CPP_OPTIMS=TRUE,
    #                                    STAN_OPENCL=TRUE,
    #                                    opencl_platform_id = 0,
    #                                    opencl_device_id = 0),
    #                 force_recompile=debug)
    
  } else {
    
    idealdata@stanmodel_map <- stan_code_map %>%
      cmdstan_model(include_paths=dirname(stan_code_map),
                    cpp_options = list(stan_threads = !debug),
                    force_recompile=debug)
    
    # idealdata@stanmodel_gpu <- stan_code_gpu %>%
    #   cmdstan_model(include_paths=dirname(stan_code_map),
    #                 cpp_options = list(stan_threads = !debug,
    #                                    STAN_OPENCL=TRUE,
    #                                    opencl_platform_id = 0,
    #                                    opencl_device_id = 0),
    #                 force_recompile=debug)
    
    
  }
  
  
  
  
  #Using an un-identified model with variational inference, find those parameters that would be most useful for
  #constraining/pinning to have an identified model for full Bayesian inference
  
  # change time IDs if non time-varying model is being fit
  if(vary_ideal_pts=='none') {
    idealdata@score_matrix$time_id <- 1
    # make sure that the covariate arrays are only one time point
    #idealdata@person_cov <- idealdata@person_cov[1,,,drop=F]
    #idealdata@group_cov <- idealdata@group_cov[1,,,drop=F]
  } 
  
  vary_ideal_pts <- switch(vary_ideal_pts,
                           none=1,
                           random_walk=2,
                           AR1=3,
                           GP=4)
  
  # number of combined posterior models
  
  mod_count <- length(unique(idealdata@score_matrix$model_id))
  
  if(length(unique(idealdata@score_matrix$ordered_id))>1) {
    mod_count <- mod_count + length(unique(idealdata@score_matrix$ordered_id)) - 1
  }
  
  
  
  # set GP parameters
  
  # check if varying model IDs exist and replace with model type
  # if not
  
  if(idealdata@score_matrix$model_id[1]=="missing") {
    
    idealdata@score_matrix$model_id <- model_type
    idealdata@score_matrix$discrete <- as.numeric(model_type %in% c(1,2,3,4,5,6,7,8,13,14))
    
  } else {
    if(!is.numeric(idealdata@score_matrix$model_id)) {
      stop("Column model_id in the data is not a numeric series of 
           integers. Please pass a numeric value in the id_make function
           for model_id based on the available model types.")
    }
  }
  
  if((!(all(c(restrict_ind_high,restrict_ind_low) %in% unique(idealdata@score_matrix$person_id))) && !use_groups) && const_type=="persons") {
    
    stop("Your restricted persons/items are not in the data.")
    
  } else if(!(all(c(restrict_ind_high,restrict_ind_low) %in% unique(idealdata@score_matrix$item_id))) && const_type=="items") {
    
    stop("Your restricted persons/items are not in the data.")
    
  } else if((!(all(c(restrict_ind_high,restrict_ind_low) %in% unique(idealdata@score_matrix$group_id)))  && use_groups) && const_type=="persons") {
    
    stop("Your restricted persons/items are not in the data.")
    
  }
  
  # check if ordinal categories exist in the data if model_id>1
  
  if(any(c(3,4,5,6) %in% idealdata@score_matrix$model_id)) {
    if(is.null(idealdata@score_matrix$ordered_id) || !is.numeric(idealdata@score_matrix$ordered_id)) {
      stop("If you have an ordered categorical variable in a multi-distribution model, you must include a column in the data with the number of ordered categories for each row in the data.\n
             See id_make documentation for more info.")
    }
  } else {
    idealdata@score_matrix$ordered_id <- 0
  }
  
  # use either row numbers for person/legislator IDs or use group IDs (static or time-varying)
  
  if(use_groups==T) {
    legispoints <- as.numeric(idealdata@score_matrix$group_id)
  } else {
    legispoints <- as.numeric(idealdata@score_matrix$person_id)
  }
  
  billpoints <- as.numeric(idealdata@score_matrix$item_id)
  timepoints <- as.numeric(factor(as.numeric(idealdata@score_matrix$time_id)))
  modelpoints <- as.integer(idealdata@score_matrix$model_id)
  ordered_id <- as.integer(idealdata@score_matrix$ordered_id)
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
  
  if(("outcome_cont" %in% names(idealdata@score_matrix)) && ("outcome_disc" %in% names(idealdata@score_matrix))) {
    Y_int <- idealdata@score_matrix$outcome_disc
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else if ("outcome_cont" %in% names(idealdata@score_matrix)) {
    Y_int <- array(0)
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else {
    Y_cont <- array(0)
    Y_int <- idealdata@score_matrix$outcome_disc
  }
  
  
  #Remove NA values, which should have been coded correctly in the make_idealdata function
  
  # need to have a different way to remove missing values if multiple
  # posteriors used
  # set values for length of discrete/continuous outcomes  
  remove_list <- .remove_nas(Y_int,
                             Y_cont,
                             discrete=idealdata@score_matrix$discrete,
                             legispoints,
                             billpoints,
                             timepoints,
                             modelpoints,
                             ordered_id,
                             idealdata,
                             time_ind=as.array(time_ind),
                             time_proc=vary_ideal_pts,
                             ar_sd=ar_sd,
                             gp_sd_par=gp_sd_par,
                             num_diff=gp_num_diff,
                             m_sd_par=gp_m_sd_par,
                             min_length=gp_min_length,
                             const_type=switch(const_type,
                                               persons=1L,
                                               items=2L),
                             discrim_reg_sd=discrim_reg_sd,
                             discrim_miss_sd=discrim_miss_sd,
                             diff_reg_sd=diff_reg_sd,
                             diff_miss_sd=diff_miss_sd,
                             legis_sd=person_sd,
                             restrict_sd_high=restrict_sd_high,
                             restrict_sd_low=restrict_sd_low,
                             restrict_high=idealdata@restrict_ind_high,
                             restrict_low=idealdata@restrict_ind_low,
                             fix_high=idealdata@restrict_num_high,
                             fix_low=idealdata@restrict_num_low)
  
  # need to create new data if map_rect is in operation 
  # and we have missing values / ragged arrays
  
  out_list <- .make_sum_vals(idealdata@score_matrix,map_over_id,use_groups=use_groups,
                             remove_nas=remove_list$remove_nas)
  
  sum_vals <- out_list$sum_vals
  
  # need number of shards
  
  S <- nrow(sum_vals)
  
  # check for heterogenous variances
  
  if(het_var) {
    
    num_var <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(9,10,11,12)]))
    
    mod_items <- tibble(model_id=remove_list$modelpoints,
                        item_id=remove_list$billpoints) %>% 
      distinct
    
    mod_items <- mutate(mod_items,cont=model_id %in% c(9,10,11,12)) %>% 
      group_by(cont) %>% 
      mutate(num_var=1:n())
    
    type_het_var <- arrange(mod_items, item_id) %>% pull(num_var)
    
  } else {
    
    num_var <- 1
    
    type_het_var <- rep(num_var, length(unique(billpoints)))
    
  }
  
  # check for boundary priors
  
  if(!is.null(boundary_prior)) {
    
    if(is.null(boundary_prior$beta)) {
      
      stop("If you want to use a boundary-avoiding prior for time-series variance, please pass a list with an element named beta, i.e. list(beta=1).")
      
    }
    
    if(boundary_prior$beta <= 0) {
      
      stop("Boundary prior beta value must be strictly positive (i.e. greater than 0).")
      
    }
    
    inv_gamma_beta <- boundary_prior$beta
    
  } else {
    
    inv_gamma_beta <- 0
    
  }
  
  if(remove_list$N_cont>0) {
    
    Y_cont <- remove_list$Y_cont[out_list$this_data$orig_order]
    
  } else {
    
    Y_cont <- remove_list$Y_cont
    
  }
  
  if(remove_list$N_int>0) {
    
    Y_int <- remove_list$Y_int[out_list$this_data$orig_order]
    
  } else {
    
    Y_int <- remove_list$Y_int
    
  }
  
  if(!is.null(ignore_db)) {
    
    # check for correct columns
    
    if(!all(c("person_id",
             "time_id",
             "ignore") %in% names(ignore_db))) {
      stop("Ignore DB does not have the three necessary columns: time_id, person_id, and ignore (0 or 1).")
      
    }
    
    ignore_db <- mutate(ungroup(ignore_db),
                        person_id=as.numeric(person_id),
                        time_id=as.numeric(factor(as.numeric(ignore_db$time_id))))
    
    # filter out time_ids not in main data
    
    ignore_db <- filter(ignore_db, time_id %in% remove_list$timepoints,
                        person_id %in% remove_list$legispoints) %>% 
      select(person_id,time_id,ignore) %>% 
      arrange(person_id,time_id) %>% 
      as.matrix
    
  } else {
    
    ignore_db <- matrix(nrow=0,ncol=3)
    
  }
  
  this_data <- list(N=remove_list$N,
                    N_cont=remove_list$N_cont,
                    N_int=remove_list$N_int,
                    Y_int=Y_int,
                    Y_cont=Y_cont,
                    y_int_miss=remove_list$y_int_miss,
                    y_cont_miss=remove_list$y_cont_miss,
                    num_var=num_var,
                    type_het_var=type_het_var,
                    S=nrow(sum_vals),
                    S_type=as.numeric(map_over_id=="persons"),
                    T=remove_list$max_t,
                    num_legis=remove_list$num_legis,
                    num_bills=remove_list$num_bills,
                    num_ls=remove_list$num_ls,
                    #num_bills_grm=remove_list$num_bills_grm,
                    ll=remove_list$legispoints[out_list$this_data$orig_order],
                    bb=remove_list$billpoints[out_list$this_data$orig_order],
                    mm=remove_list$modelpoints[out_list$this_data$orig_order],
                    ignore=as.numeric(nrow(ignore_db)>0),
                    ignore_db=ignore_db,
                    mod_count=length(unique(remove_list$modelpoints)),
                    num_fix_high=as.integer(1),
                    num_fix_low=as.integer(1),
                    tot_cats=length(remove_list$n_cats_rat),
                    n_cats_rat=remove_list$n_cats_rat,
                    n_cats_grm=remove_list$n_cats_grm,
                    order_cats_rat=remove_list$order_cats_rat[out_list$this_data$orig_order],
                    order_cats_grm=remove_list$order_cats_grm[out_list$this_data$orig_order],
                    num_bills_grm=ifelse(any(remove_list$modelpoints %in% c(5,6)),
                                         remove_list$num_bills,0L),
                    LX=remove_list$LX,
                    SRX=remove_list$SRX,
                    SAX=remove_list$SAX,
                    legis_pred=remove_list$legis_pred[out_list$this_data$orig_order,,drop=FALSE],
                    srx_pred=remove_list$srx_pred[out_list$this_data$orig_order,,drop=FALSE],
                    sax_pred=remove_list$sax_pred[out_list$this_data$orig_order,,drop=FALSE],
                    time=remove_list$timepoints[out_list$this_data$orig_order],
                    time_proc=vary_ideal_pts,
                    discrim_reg_sd=discrim_reg_sd,
                    discrim_abs_sd=discrim_miss_sd,
                    diff_reg_sd=diff_reg_sd,
                    diff_abs_sd=diff_miss_sd,
                    legis_sd=person_sd,
                    restrict_sd_high=5,
                    restrict_sd_low=5,
                    time_sd=time_fix_sd,
                    time_var_sd=time_var,
                    ar1_up=ar1_up,
                    ar1_down=ar1_down,
                    inv_gamma_beta=inv_gamma_beta,
                    center_cutoff=as.integer(time_center_cutoff),
                    restrict_var=restrict_var,
                    ar_sd=ar_sd,
                    zeroes=as.numeric(inflate_zero),
                    time_ind=as.array(time_ind),
                    time_proc=vary_ideal_pts,
                    gp_sd_par=gp_sd_par,
                    m_sd_par=gp_m_sd_par,
                    min_length=gp_min_length,
                    id_refresh=id_refresh,
                    sum_vals=as.matrix(sum_vals),
                    const_type=switch(const_type,
                                      persons=1L,
                                      items=2L),
                    restrict_high=1,
                    restrict_low=2,
                    fix_high=0,
                    fix_low=0,
                    num_diff=gp_num_diff,
                    pos_discrim=as.integer(pos_discrim),
                    grainsize=grainsize)
  
  idealdata <- id_model(object=idealdata,fixtype=fixtype,this_data=this_data,
                        nfix=nfix,restrict_ind_high=restrict_ind_high,
                        restrict_ind_low=restrict_ind_low,
                        ncores=ncores,
                        tol_rel_obj=tol_rel_obj,
                        use_groups=use_groups,
                        prior_fit=prior_fit,
                        fix_high=fix_high,
                        fix_low=fix_low,
                        const_type=const_type)
  
  if(("outcome_cont" %in% names(idealdata@score_matrix)) && ("outcome_disc" %in% names(idealdata@score_matrix))) {
    Y_int <- idealdata@score_matrix$outcome_disc
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else if ("outcome_cont" %in% names(idealdata@score_matrix)) {
    Y_int <- array(0)
    Y_cont <- idealdata@score_matrix$outcome_cont
  } else {
    Y_cont <- array(0)
    Y_int <- idealdata@score_matrix$outcome_disc
  }
  
  
  # need to redo everything post-identification
  
  remove_list <- .remove_nas(Y_int,
                             Y_cont,
                             discrete=idealdata@score_matrix$discrete,
                             legispoints,
                             billpoints,
                             timepoints,
                             modelpoints,
                             ordered_id,
                             idealdata,
                             time_ind=as.array(time_ind),
                             time_proc=vary_ideal_pts,
                             ar_sd=ar_sd,
                             gp_sd_par=gp_sd_par,
                             m_sd_par=gp_m_sd_par,
                             min_length=gp_min_length,
                             const_type=switch(const_type,
                                               persons=1L,
                                               items=2L),
                             discrim_reg_sd=discrim_reg_sd,
                             discrim_miss_sd=discrim_miss_sd,
                             diff_reg_sd=diff_reg_sd,
                             diff_miss_sd=diff_miss_sd,
                             legis_sd=person_sd,
                             restrict_sd_high=restrict_sd_high,
                             restrict_sd_low=restrict_sd_low,
                             restrict_high=idealdata@restrict_ind_high,
                             restrict_low=idealdata@restrict_ind_low,
                             fix_high=idealdata@restrict_num_high,
                             fix_low=idealdata@restrict_num_low)
  
  idealdata <- remove_list$idealdata
  
  # need to create new data if map_rect is in operation 
  # and we have missing values / ragged arrays
  
  out_list <- .make_sum_vals(idealdata@score_matrix,map_over_id,use_groups=use_groups,
                             remove_nas=remove_list$remove_nas)
  
  sum_vals <- out_list$sum_vals
  
  # make sure data is re-sorted by ID
  
  idealdata@score_matrix <- out_list$this_data
  
  # need number of shards
  
  S <- nrow(sum_vals)
  
  # check for heterogenous variances
  
  if(het_var) {
    
    num_var <- length(unique(remove_list$billpoints[remove_list$modelpoints %in% c(9,10,11,12)]))
    
    mod_items <- tibble(model_id=this_data$mm,
                        item_id=this_data$bb) %>% 
      distinct
    
    mod_items <- mutate(mod_items,cont=model_id %in% c(9,10,11,12)) %>% 
      group_by(cont) %>% 
      mutate(num_var=1:n())
    
    type_het_var <- arrange(mod_items, item_id) %>% pull(num_var)
    
  } else {
    
    num_var <- 1
    
    type_het_var <- rep(num_var, length(unique(billpoints)))
    
  }
  
  if(remove_list$N_cont>0) {
    
    Y_cont <- remove_list$Y_cont[out_list$this_data$orig_order]
    
  } else {
    
    Y_cont <- remove_list$Y_cont
    
  }
  
  if(remove_list$N_int>0) {
    
    Y_int <- remove_list$Y_int[out_list$this_data$orig_order]
    
    order_cats_rat <- remove_list$order_cats_rat[out_list$this_data$orig_order]
    order_cats_grm <- remove_list$order_cats_grm[out_list$this_data$orig_order]
    
  } else {
    
    Y_int <- remove_list$Y_int
    order_cats_rat <- remove_list$order_cats_rat
    order_cats_grm <- remove_list$order_cats_grm
    
  }
  
  this_data <- list(N=remove_list$N,
                    N_cont=remove_list$N_cont,
                    N_int=remove_list$N_int,
                    Y_int=Y_int,
                    Y_cont=Y_cont,
                    y_int_miss=remove_list$y_int_miss,
                    y_cont_miss=remove_list$y_cont_miss,
                    num_var=num_var,
                    type_het_var=type_het_var,
                    S=nrow(sum_vals),
                    S_type=as.numeric(map_over_id=="persons"),
                    T=remove_list$max_t,
                    num_legis=remove_list$num_legis,
                    num_bills=remove_list$num_bills,
                    num_ls=remove_list$num_ls,
                    #num_bills_grm=remove_list$num_bills_grm,
                    ll=remove_list$legispoints[out_list$this_data$orig_order],
                    bb=remove_list$billpoints[out_list$this_data$orig_order],
                    mm=remove_list$modelpoints[out_list$this_data$orig_order],
                    ignore=as.numeric(nrow(ignore_db)>0),
                    ignore_db=ignore_db,
                    mod_count=length(unique(remove_list$modelpoints)),
                    num_fix_high=as.integer(1),
                    num_fix_low=as.integer(1),
                    tot_cats=length(remove_list$n_cats_rat),
                    n_cats_rat=remove_list$n_cats_rat,
                    n_cats_grm=remove_list$n_cats_grm,
                    order_cats_rat=order_cats_rat,
                    order_cats_grm=order_cats_grm,
                    num_bills_grm=ifelse(any(remove_list$modelpoints %in% c(5,6)),
                                         remove_list$num_bills,0L),
                    LX=remove_list$LX,
                    SRX=remove_list$SRX,
                    SAX=remove_list$SAX,
                    legis_pred=remove_list$legis_pred[out_list$this_data$orig_order,,drop=FALSE],
                    srx_pred=remove_list$srx_pred[out_list$this_data$orig_order,,drop=FALSE],
                    sax_pred=remove_list$sax_pred[out_list$this_data$orig_order,,drop=FALSE],
                    time=remove_list$timepoints[out_list$this_data$orig_order],
                    time_proc=vary_ideal_pts,
                    discrim_reg_sd=discrim_reg_sd,
                    discrim_abs_sd=discrim_miss_sd,
                    diff_reg_sd=diff_reg_sd,
                    diff_abs_sd=diff_miss_sd,
                    legis_sd=person_sd,
                    restrict_sd_high=restrict_sd_high,
                    restrict_sd_low=restrict_sd_low,
                    time_sd=time_fix_sd,
                    time_var_sd=time_var,
                    ar1_up=ar1_up,
                    ar1_down=ar1_down,
                    inv_gamma_beta=inv_gamma_beta,
                    center_cutoff=as.integer(time_center_cutoff),
                    restrict_var=restrict_var,
                    ar_sd=ar_sd,
                    zeroes=as.numeric(inflate_zero),
                    time_ind=as.array(time_ind),
                    gp_sd_par=gp_sd_par,
                    m_sd_par=gp_m_sd_par,
                    min_length=gp_min_length,
                    id_refresh=id_refresh,
                    sum_vals=as.matrix(sum_vals),
                    const_type=switch(const_type,
                                      persons=1L,
                                      items=2L),
                    restrict_high=idealdata@restrict_ind_high,
                    restrict_low=idealdata@restrict_ind_low,
                    fix_high=idealdata@restrict_num_high,
                    fix_low=idealdata@restrict_num_low,
                    num_diff=gp_num_diff,
                    pos_discrim=as.integer(pos_discrim),
                    grainsize=grainsize)
  
  # need to save n_cats
  
  idealdata@n_cats_rat <- remove_list$n_cats_rat
  idealdata@n_cats_grm <- remove_list$n_cats_grm
  idealdata@order_cats_rat <- remove_list$order_cats_rat
  idealdata@order_cats_grm <- remove_list$order_cats_grm
  
  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=ncores,
                         this_data=this_data,use_vb=use_vb,
                         #gpu=gpu,
                         save_files=save_files,
                         keep_param=keep_param,
                         tol_rel_obj=tol_rel_obj,within_chain=within_chain,
                         ...)
  
  outobj@model_type <- model_type
  outobj@time_proc <- vary_ideal_pts
  outobj@use_groups <- use_groups
  outobj@map_over_id <- map_over_id
  outobj@time_fix_sd <- time_fix_sd
  outobj@restrict_var <- restrict_var
  outobj@time_center_cutoff <- time_center_cutoff
  outobj@orig_order <- out_list$this_data$orig_order
  outobj@this_data <- this_data
  
  # need to recalculate legis points if time series used
  if(this_data$T>1 && ((!is.null(keep_param$person_vary) && keep_param$person_vary) || is.null(keep_param))) {
    outobj@time_varying <- try(.get_varying(outobj,
                                            legis_x=remove_list$legis_pred[out_list$this_data$orig_order,,drop=FALSE],
                                            person_id=this_data$ll,
                                            time_id=this_data$time))
  }
  
  return(outobj)
  
}

#' DEPRECATED: Reconstitute an idealstan object after an MPI/cluster run
#' 
#' This convenience function takes as input a file location storing the results of a 
#' MPI/cluster run. 
#' 
#' Given the CSV output from cmdstan and the original files exported 
#' from the \code{id_estimate} function, this function will create an \code{idealstan}
#' object which can be further analyzed with other \code{idealstan} package helper
#' functions.
#'  
#' @param object A fitted \code{idealstan} object (see \code{\link{id_estimate}})
#' @param file_loc A string with the location of the original files exported by 
#' \code{id_estimate}
#' @param csv_name A vector of character names of CSV files with posterior estimates from 
#' \code{cmdstan}. Should be located in the same place as \code{file_loc}.
#' @importFrom posterior summarize_draws
id_rebuild_mpi <- function(file_loc=NULL,
                           csv_name=NULL) {
  
  
  if(is.null(file_loc)) {
    file_loc <- rstudioapi::selectDirectory(caption="Choose the directory containing relevant files to rebuild object:")
  }
  
  all_csvs <- read_cmdstan_csv(paste0(file_loc,"/",csv_name))
  
  object <- readRDS(paste0(file_loc,"/","idealdata_object.rds"))
  
  extra_params <- readRDS(paste0(file_loc,"/",extra_params.rds))
  
  outobj <- new('idealstan',
                score_data=object,
                model_code=readLines(paste0(file_loc,"/","idealstan_stan_code.stan")),
                stan_samples=all_csvs,
                use_vb=extra_params$use_vb)
  
  # add safe summaries
  
  to_sum <- summarize_draws(outobj@stan_samples$post_warmup_draws)
  
  to_sum <- select(to_sum,variable,lower="q95",mean,median,upper="q5",rhat,ess_bulk,ess_tail)
  
  outobj@summary <- to_sum
  
  outobj@mpi <- T
  
  outobj@model_type <- extra_params$model_type
  outobj@time_proc <- extra_params$vary_ideal_pts
  outobj@use_groups <- extra_params$use_groups
  outobj@map_over_id <- extra_params$map_over_id
  outobj@time_fix_sd <- extra_params$time_fix_sd
  
  return(outobj)
  
  
}