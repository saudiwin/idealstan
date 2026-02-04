#' Create data to run IRT model
#' 
#' To run an IRT model using `idealstan`, you must first process your data using the `id_make` 
#' function. 
#' 
#' @details This function accepts a long data frame where one row equals one item-person (bill-legislator)
#' observation with associated continuous or discrete outcomes/responses.
#' You either need to include columns with specific names as required by the `id_make`
#' function such as `person_id` for person IDs and `item_id` for item IDs or
#'  specify the names of the 
#' columns containing the IDs to the `id_make` function for each column name (see examples).
#' The only required columns are the item/bill ID and the person/legislator ID along with an 
#' outcome column, `outcome_disc` for discrete variables and `outcome_cont` for
#' continuous variables. If both columns are included, then any value can be included for 
#' `outcome_disc` if there are values for `outcome_cont` and vice versa.
#' 
#' If items of multiple types are included, a column `model_id` must be included with
#' the model type (see `id_estimate` function documentation for list of model IDs)
#' for the response distribution, such as 
#' 1 for binary non-inflated, etc. If an ordinal outcome is included, an additional column
#' `ordered_id` must be included that has the total count of categories for that 
#' ordinal variable (i.e., 3 for 3 categories). 
#' 
#' For discrete data, it is recommended to include a numeric variable that starts at 0, such 
#' as values of 0 and 1 for binary data and 0,1,2 for ordinal/categorical data.
#' For continuous (unbounded) data, it is recommended to standardize the outcome to improve
#' model convergence and fit. 
#' 
#' Missing data should be passed as `NA` values in either
#' `outcome_disc` or `outcome_cont` and will be processed internally.
#' 
#' 
#' @section Time-Varying Models:
#' 
#' To run a time-varying model, you need to include the name of a column with dates (or integers) that is passed 
#' to the `time_id` option.
#' 
#' @section Continuous Outcomes:
#' 
#' If the outcome is continuous, you need to pass a dataframe with one column named
#' "outcome_disc" or pass the name of the column with the continuous data to the `outcome_disc`
#' argument. 
#' 
#' @section Hierarchical Covariates:
#' 
#' Covariates can be fit on the person-level ideal point parameters as well as
#' item discrimination parameters for either the inflated (missing) or non-inflated (observed) 
#' models. These covariates must be columns that were included with the data fed to the 
#' [id_make()] function. The covariate relationships are specified as 
#' one-sided formulas, i.e. `~cov1 + cov2 + cov1*cov2`. To interact covariates with the 
#' person-level ideal points you can use `~cov1 + person_id + cov1*person_id` and for
#' group-level ideal poins you can use `~cov1 + group_id + cov1*group_id` where
#' `group_id` or `person_id` is the same name as the name of the column 
#' for these options that you passed to `id_make` (i.e., the names of the columns
#' in the original data). If you are also going to model these intercepts--i.e. you are 
#' interacting the covariate with `person_id` and the model is estimating ideal points
#' at the person level--then set `remove_cov_int` to TRUE to avoid multicollinearity with the
#' ideal point intercepts.
#' 
#' @param score_data A data frame in long form, i.e., one row in the data for each 
#' measured score or vote in the data or a `rollcall` data object from package `pscl`.
#' @param outcome_disc Column name of the outcome with discrete values in `score_data`, default is `"outcome_disc"`
#' @param outcome_cont Column name of the outcome with discrete values in `score_data`, default is `"outcome_disc"`
#' @param ordered_id Column name of the variable showing the count of categories for 
#' ordinal/categorical items (must be at least 3 categories)
#' @param ignore_id Optional column for identifying observations that should not be 
#' modeled (i.e., not just treated as missing, rather removed during estimation). Should 
#' be a binary vector (0 for remove and 1 for include). Useful for time-varying models where
#' persons may not be present during particular periods and missing data is ignorable.
#' @param person_id Column name of the person/legislator ID index in `score_data`, 
#' default is `'person_id'`. Should be integer, character or factor.
#' @param item_id Column name of the item/bill ID index in `score_data`, 
#' default is `'item_id'`.  Should be integer, character or factor.
#' @param time_id Column name of the time values in `score_data`: 
#' optional, default is `'time_id'`. Should be a date or date-time class, but can be an integer
#' (i.e., years in whole numbers).
#' @param model_id Column name of the model/response types in the data.
#' Default is `"model_id"`. Only necessary if a model with multiple 
#' response types (i.e., binary + continuous outcomes). Must be a 
#' column with a series
#' of integers matching the model types in [id_estimate()] 
#' showing which row of the data matches which outcome.
#' @param group_id Optional column name of a person/legislator group IDs (i.e., parties) in `score_data`. 
#' Optional, default is `'group_id'`. Should be integer, character or factor.
#' @param person_cov A one-sided formula that specifies the covariates
#' in `score_data` that will be used to hierarchically model the person/legislator ideal points
#' @param item_cov A one-sided formula that specifies the covariates
#' in `score_data` that will be used to hierarchically model the 
#' item/bill discrimination parameters for the regular model
#' @param item_cov_miss A one-sided formula that specifies the covariates
#' in the dataset that will be used to hierarchically model the item/bill discrimination parameters for the
#' missing data model.
#' @param remove_cov_int Whether to remove constituent terms from hierarchical covariates that 
#' interact covariates with IDs like `person_id` or `item_id`. Set to `TRUE` if
#' including these constituent terms would cause multi-collinearity with other terms in the model
#' (such as running a group-level model with a group-level interaction or a person-level model
#' with a person-level interaction).
#' @param simul_data Optionally, data that has been generated by the [id_sim_gen()] function.
#' @param unbounded Whether or not the outcome/response is unbounded (i.e., continuous or
#'  Poisson). If it is, missing value 
#'  is recoded as the maximum of the outcome + 1. 
#' @param exclude_level A vector of any values that should be treated as `NA` in the response matrix. 
#' Unlike missing values, these values will be dropped from the data before 
#' estimation rather than modeled explicitly.
#' @param simulation If `TRUE`, simulated values are saved in the `idealdata` object for 
#' later plotting with the [id_show_trues()] function
#' @return A `idealdata` object that can then be used in the [id_estimate()] function 
#' to fit a model.
#' @export
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom forcats fct_relevel
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
#'                outcome_disc = 'cast_code',
#'                person_id = 'bioname',
#'                item_id = 'rollnumber',
#'                group_id= 'party_code',
#'                time_id='date')
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
  
  # first, save all arguments to the function call
  
  func_args <- match.call()
  
  # force evaluation of first argument in case it was piped
  
  func_args_list <- as.list(func_args)
  
  func_args_list[[1]] <- NULL

  func_args_list$score_data <- score_data
  
  # only allow  missing values as NA
  
  miss_val <- c(NA,NA)
  
  high_val <- NULL
  middle_val <- NULL
  low_val <- NULL
  
  
  # make sure to ungroup it if it's a tidy data frame
  if('tbl' %in% class(score_data)) score_data <- ungroup(score_data)
  
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
    score_rename$model_id <- as.integer(test_model)
  }
  
  # if time or group IDs don't exist, make dummies
  
  test_group <- try(factor(pull(score_data,!!group_id)),silent=TRUE)
  test_time <- try(pull(score_data,!!time_id),silent=TRUE)
  test_out_disc <- try(factor(pull(score_data,!!outcome_disc)),silent=TRUE)
  test_out_cont <- try(pull(score_data,!!outcome_cont),silent=TRUE)
  test_ordered <- try(pull(score_data,!!ordered_id),silent=TRUE)
  
  if(any('try-error' %in% class(test_group))) {
    score_rename$group_id <- 1L
  } else {
    score_rename$group_id <- as.integer(test_group)
  }
  if(any('try-error' %in% class(test_time))) {
    score_rename$time_id <- 1L
  } else {
    score_rename$time_id <- test_time
  }
  
  if(any('try-error' %in% class(test_out_disc))) {
    
  } else {
    score_rename$outcome_disc <- as.integer(test_out_disc)
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
    
    check_ids <- sapply(levels(score_rename$person_id), function(pers) {
      check_all <- grepl(x=colnames(personm),
                         pattern=as.character(pers))
      check_all_colon <- grepl(x=colnames(personm),
                               pattern=paste0(as.character(pers),":","|",":",as.character(pers)))
      matrix(check_all & !check_all_colon,nrow=length(check_all))
    },simplify = F) %>% 
      do.call(rbind, .) %>% 
      apply(2,any)
    
    if(remove_cov_int) {
      personm <- personm[,!check_ids,drop=F]
    }

    # Add prefix to person covariate names to avoid conflicts (issue #32)
    colnames(personm) <- paste0("person_", colnames(personm))

    score_rename <- bind_cols(score_rename,
                              as_tibble(personm, .name_repair = "minimal"))
    person_cov_names <- dimnames(personm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$personcov0 <- 0
    person_cov_names <- 'personcov0'
    person_cov <- formula()
    # need to avoid picking up stuff from environment
    environment(person_cov) <- emptyenv()
  }
  
  if(!is.null(item_cov)) {
    
    itemm <- model.matrix(item_cov,data=score_data)[,-1,drop=F]
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(levels(score_rename$item_id), function(c) {
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

    # Add prefix to item covariate names to avoid conflicts (issue #32)
    colnames(itemm) <- paste0("item_", colnames(itemm))

    score_rename <- bind_cols(score_rename,
                              as_tibble(itemm, .name_repair = "minimal"))
    item_cov_names <- dimnames(itemm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcov0 <- 0
    item_cov_names <- 'itemcov0'
    item_cov <- formula()
    # need to avoid picking up stuff from environment
    environment(item_cov) <- emptyenv()
  }
  
  if(!is.null(item_cov_miss)) {
    
    itemmissm <- model.matrix(item_cov_miss,data=score_data)[,-1,drop=FALSE]
    
    # need to check for missing data and remove any missing from IDs
    
    score_rename <- slice(score_rename,as.numeric(attr(itemmissm,'dimnames')[[1]]))
    
    if(nrow(score_rename)!=nrow(itemmissm)) stop('Covariate matrix and data matrix not the same size even after removing missing data.')
    
    # need to remove any constituent terms for IDs from the model matrix to avoid collinearity with 
    # model parameters
    
    check_ids <- sapply(levels(score_rename$item_id), function(c) {
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

    # Add prefix to item_miss covariate names to avoid conflicts (issue #32)
    colnames(itemmissm) <- paste0("item_miss_", colnames(itemmissm))

    score_rename <- bind_cols(score_rename,
                              as_tibble(itemmissm, .name_repair = "minimal"))
    item_cov_miss_names <- dimnames(itemmissm)[[2]]
  } else {
    # make a dummy column if no covariate data
    score_rename$itemcovmiss0 <- 0
    item_cov_miss_names <- 'itemcovmiss0'
    item_cov_miss <- formula()
    # need to avoid picking up stuff from environment
    environment(item_cov_miss) <- emptyenv()
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
      # Only relevel if the missing level actually exists in the data
      if(as.character(miss_val[1]) %in% levels(score_rename$outcome_disc)) {
        score_rename$outcome_disc <- fct_relevel(score_rename$outcome_disc,as.character(miss_val[1]),after=Inf)
      }
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
                person_cov=person_cov_names,
                person_cov_formula=person_cov,
                item_cov=item_cov_names,
                item_cov_miss=item_cov_miss_names,
                item_cov_formula=item_cov,
                item_cov_miss_formula=item_cov_miss,
                miss_val=miss_val,
                func_args=func_args_list)
  
  if(simulation) {
    outobj@simul_data <- simul_data
    outobj@simulation <- simulation
  }
  return(outobj)
}

#' Estimate an `idealstan` model
#' 
#' This function will take a pre-processed `idealdata` vote/score dataframe and 
#' run one of the available IRT/latent space ideal point models on the data using
#' Stan's MCMC engine.
#' 
#' To run an IRT ideal point model, you must first pre-process your data using the [id_make()] function. Be sure to specify the correct options for the
#' kind of model you are going to run: if you want to run an unbounded outcome (i.e. Poisson or continuous),
#' the data needs to be processed differently. Also any hierarchical covariates at the person or item level
#' need to be specified in [id_make()]. If they are specified in [id_make()], than all 
#' subsequent models fit by this function will have these covariates.
#' 
#' **Note that for static ideal point models, the covariates are only defined for those 
#' persons who are not being used as constraints.**
#' 
#' As of this version of `idealstan`, the following model types are available. Simply pass 
#' the number of the model in the list to the `model_type` option to fit the model.
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
#'   \item Ordered Beta (proportion/percentage) with no missing data
#'   \item Ordered Beta (proportion/percentage) with missing-data inflation
#' }
#' 
#' @section Time-Varying Inference:
#' 
#' In addition, each of these models can have time-varying ideal point (person) parameters if
#' a column of dates is fed to the [id_make()] function. If the option `vary_ideal_pts` is 
#' set to `'random_walk'`, `id_estimate` will estimate a random-walk ideal point model where ideal points 
#' move in a random direction. If `vary_ideal_pts` is set to `'AR1'`, a stationary ideal point model 
#' is estimated where ideal points fluctuate around long-term mean. If `vary_ideal_pts` 
#' is set to `'GP'`, then a semi-parametric Gaussian process time-series prior will be put
#' around the ideal points. If `vary_ideal_pts` 
#' is set to `'splines'`, then the ideal point trajectories will be a basis spline defined by the parameters `spline_knots` and `spline_degree`. 
#' Please see the package vignette and associated paper for more detail
#' about these time-varying models.
#' 
#' @section Missing Data:
#' 
#' The inflation model used to account for missing data assumes that missingness is a 
#' function of the persons' (legislators')
#' ideal points. In other words,the model will take into account if people with high or low ideal points
#' tend to have more/less missing data on a specific item/bill. Missing data should be coded
#' as `NA` when it is passed to the [id_make] function.
#' If there isn't any relationship
#' between missing data and ideal points, then the model assumes that the missingness is ignorable 
#' conditional on each
#' item, but it will still adjust the results to reflect these ignorable (random) missing
#' values. The inflation is designed to be general enough to handle a wide array of potential
#' situations where strategic social choices make missing data important to take into account.
#' 
#' To leave missing data out of the model, simply choose a version of the model in the list above
#' that is non-inflated.
#' 
#' Models can be either fit on the person/legislator IDs or on group-level IDs (as specified to the 
#' `id_make` function). If group-level parameters should be fit, set `use_groups` to `TRUE`.
#' 
#' @section Covariates:
#' 
#' Covariates are included in the model if they were specified as options to the 
#' [id_make()] function. The covariate plots can be accessed with 
#' [id_plot_cov()] on a fitted `idealstan` model object.
#' 
#' @section Identification:
#' Identifying IRT models is challenging, and ideal point models are still more challenging 
#' because the discrimination parameters are not constrained.
#' As a result, more care must be taken to obtain estimates that are the same regardless of starting values. 
#' The parameter `fixtype` enables you to change the type of identification used. The default, 'vb_full', 
#' does not require any further
#' information from you in order for the model to be fit. In this version of identification, 
#' an unidentified model is run using
#' variational Bayesian inference (see [rstan::vb()]). The function will then select two 
#' persons/legislators or items/bills that end up on either end of the ideal point spectrum, 
#' and pin their ideal points
#' to those specific values. 
#' To control whether persons/legislator or items/bills are constrained,
#' the `const_type` can be set to either `"persons"` or 
#' `"items"` respectively. 
#' In many situations, it is prudent to select those persons or items 
#' ahead of time to pin to specific values. This allows the analyst to 
#' be more specific about what type of latent dimension is to be 
#' estimated. To do so, the `fixtype` option should be set to 
#' `"prefix"`. The values of the persons/items to be pinned can be passed
#' as character values to `restrict_ind_high` and 
#' `restrict_ind_low` to pin the high/low ends of the latent 
#' scale respectively. Note that these should be the actual data values 
#' passed to the `id_make` function. If you don't pass any values, 
#' you will see a prompt asking you to select certain values of persons/items.
#' 
#' The pinned values for persons/items are set by default to +1/-1, though
#' this can be changed using the `fix_high` and 
#' `fix_low` options. This pinned range is sufficient to identify 
#' all of the models
#' implemented in idealstan, though fiddling with some parameters may be 
#' necessary in difficult cases. For time-series models, one of the 
#' person ideal point over-time variances is also fixed to .1, a value that
#' can be changed using the option `time_fix_sd`.
#' @param idealdata An object produced by the [id_make()] 
#' containing a score/vote matrix for use for estimation & plotting
#' @param model_type An integer reflecting the kind of model to be estimated. 
#' See below.
#' @param inflate_zero If the outcome is distributed as Poisson (count/unbounded integer), 
#' setting this to 
#' `TRUE` will fit a traditional zero-inflated model. 
#' @param ignore_db If there are multiple time periods (particularly when there are 
#' very many time periods), you can pass in a data frame
#' (or tibble) with one row per person per time period and an indicator column 
#' `ignore` that is equal to 1 for periods that should be considered in sample
#' and 0 for periods for periods that should be considered out of sample. This is 
#' useful for excluding time periods from estimation for persons when they could not 
#' be present, i.e. such as before entrance into an organization or following death.
#' If `ignore` equals 0, the person's ideal point is estimated as a standard Normal
#' draw rather than an auto-correlated parameter, reducing computational 
#' burden substantially.
#' Note that there can only be one pre-sample period of 0s, one in-sample period of 1s,
#' and one post-sample period of 0s. Multiple in-sample periods cannot be interspersed
#' with out of sample periods. The columns must be labeled as `person_id`, 
#' `time_id` and `ignore` and must match the formatting of the columns
#' fed to the `id_make` function.
#' @param keep_param A list with logical values for different categories of paremeters which
#' should/should not be kept following estimation. Can be any/all of `person_int` for 
#' the person-level intercepts (static ideal points), 
#' `person_vary` for person-varying ideal points,
#' `item` for observed item parameters (discriminations/intercepts),
#' `item_miss` for missing item parameters (discriminations/intercepts),
#' and `extra` for other parameters (hierarchical covariates, ordinal intercepts, etc.).
#' Takes the form `list(person_int=TRUE,person_vary=TRUE,item=TRUE,item_miss=TRUE,extra=TRUE)`.
#' If any are missing in the list, it is assumed that those parameters will be excluded.
#' If `NULL` (default), will save all parameters in output.
#' @param grainsize The grainsize parameter for the `reduce_sum` 
#' function used for within-chain parallelization. The default is 1, 
#' which means 1 chunk (item or person) per core. Set to -1. to use
#' @param map_over_id This parameter identifies which ID variable to use to construct the 
#' shards for within-chain parallelization. It defaults to `"persons"` but can also take
#' a value of `"items"`. It is recommended to select whichever variable has more
#' distinct values to improve parallelization.
#' @param vary_ideal_pts Default `'none'`. If `'random_walk'`, `'AR1'`, 
#' `'GP'`, or `'splines'`, a 
#' time-varying ideal point model will be fit with either a random-walk process, an 
#' AR1 process, a Gaussian process or a spline. 
#' Note that the spline is the easiest time-varying model to fit so long as the number
#' of knots (option `spline_knots`) is significantly less than 
#' the number of time points in the data. 
#' See documentation for more info.
#' @param seed The integer seed passed on to the estimation engine (including the 
#' algorithm used to obtain starting values)
#' @param use_subset Whether a subset of the legislators/persons should be used instead of the full response matrix
#' @param sample_it Whether or not to use a random subsample of the response matrix. Useful for testing.
#' @param subset_group If person/legislative data was included in the [id_make()] function, then you can subset by
#' any value in the `$group` column of that data if `use_subset` is `TRUE`.
#' @param subset_person A list of character values of names of persons/legislators to use to subset if `use_subset` is 
#' `TRUE` and person/legislative data was included in the [id_make()] function with the required `$person.names`
#' column
#' @param sample_size If `sample_it` is `TRUE`, this value reflects how many legislators/persons will be sampled from
#' the response matrix
#' @param nchains The number of chains to use in Stan's sampler. Minimum is one. See [rstan::stan()] for more info. If `use_method="pathfinder"`, this parameter
#' will determine the number of Pathfinder paths to estimate.
#' @param niters The number of iterations to run Stan's sampler. Shouldn't be set much lower than 500. See [rstan::stan()] for more info.
#' @param use_method The type of estimation to use to estimate the posterior 
#' distribution of the parameters. The default is `"mcmc"`, which uses Stan's 
#' Hamiltonian Markov Chain Monte Carlo inference. The other options, `"pathfinder"` and `"laplace"`, are variational algorithms that derive an easier/faster to compute
#" approximation. Pros: it's much faster but can be much less accurate. Note that Pathfinder is 
#' also used by default for finding initial starting values for full HMC sampling.
#' @param warmup The number of iterations to use to calibrate Stan's sampler on a given model. Shouldn't be less than 100. 
#' See [rstan::stan()] for more info.
#' @param ncores The number of cores in your computer to use for parallel processing in the Stan engine. 
#' See [rstan::stan()] for more info. If `within_chain` is set to
#' `"threads"`, this parameter will determine the number of threads 
#' (independent processes) used for within-chain parallelization.
#' @param fixtype Sets the particular kind of identification used on the model, could be either 'vb_full' 
#' (identification provided exclusively by running a variational identification model with no prior info), or
#' 'prefix' (two indices of ideal points or items to fix are provided to 
#' options `restrict_ind_high` and `restrict_ind_low`).
#'  See details for more information.
#' @param prior_only Whether to only sample from priors as opposed to the full model
#' with likelihood (the default). Useful for doing posterior predictive checks.
#' @param mpi_export If `within_chains="mpi"`, this parameter should refer to the 
#' directory where the necessary data and Stan code will be exported to. If missing, 
#' an interactive dialogue will prompt the user for a directory. 
#' @param id_refresh The number of times to report iterations from the variational run used to 
#' identify models. Default is 0 (nothing output to console).
#' @param ar_prior If an AR(1) model is used, this 2-length vector sets the 
#' shape (alpha) and scale (beta) of the Beta prior on the AR-1 adjustment 
#' parameters (often denoted phi). The first element of the vector is alpha
#' and the second is beta. See Stan documentation of the Beta distribution
#' for more info.
#' @param use_groups If `TRUE`, group parameters from the person/legis data given in [id_make()] will be 
#'  estimated instead of individual parameters. 
#' @param const_type Whether `"persons"` are the parameters to be 
#' fixed for identification (the default) or `"items"`. Each of these
#' pinned parameters should be specified to `fix_high` and `fix_low`
#' if `fixtype` equals `"prefix"`, otherwise the model will
#' select the parameters to pin to fixed values.
#' @param restrict_ind_high If `fixtype` is not "vb_full", a vector of character values or integer indices
#' of a legislator/person or bill/item to pin to a high value (default +1).
#' @param restrict_ind_low If `fixtype` is not "vb_full", a vector of character values or integer indices of a 
#' legislator/person or bill/item to pin to a low value (default -1). 
#' @param num_restrict_high If using variational inference for identification (`fixtype="vb_full"`),
#' how many parameters to constraint to positive values? Default is 1.
#' @param num_restrict_low If using variational inference for identification (`ixtype="vb_full"`),
#' how many parameters to constraint to positive negative values? Default is 1.
#' @param fix_high A vector of length `restrict_ind_high` with values 
#' that the high fixed person ideal point(s) should be
#' fixed to. Default is +2. Does not apply when `const_type="items"`; in that case,
#' use `restrict_sd`/`restrict_N` parameters (see below).
#' @param fix_low A vector of length `restrict_ind_low` with values 
#' that the high fixed person ideal point(s) should be
#' fixed to. Default is -2. Does not apply when `const_type="items"`; in that case,
#' use `restrict_sd`/`restrict_N` parameters (see below).
#' @param person_sd The standard deviation of the Normal distribution prior for 
#' persons (all non-constrained person ideal point parameters). Default is weakly informative (3)
#' on the logit scale.
#' @param discrim_reg_upb Upper bound of the rescaled Beta distribution for 
#' observed discrimination parameters (default is +1)
#' @param discrim_reg_lb Lower bound of the rescaled Beta distribution for 
#' observed discrimination parameters (default is -1). Set to 0 for 
#' conventional IRT.
#' @param discrim_miss_upb Upper bound of the rescaled Beta distribution for 
#' missing discrimination parameters (default is +1)
#' @param discrim_miss_lb Lower bound of the rescaled Beta distribution for 
#' missing discrimination parameters (default is -1). Set to 0 for 
#' conventional IRT.
#' @param discrim_reg_scale Set the scale parameter for the rescaled Beta distribution
#' of the discrimination parameters.
#' @param discrim_reg_shape Set the shape parameter for the rescaled Beta distribution
#' of the discrimination parameters.
#' @param discrim_miss_scale Set the scale parameter for the rescaled Beta distribution
#' of the missingness discrimination parameters.
#' @param discrim_miss_shape Set the shape parameter for the rescaled Beta distribution
#' of the missingness discrimination parameters.
#' @param restrict_var Whether to fix the variance parameter for the first person trajectory. Default
#' is FALSE (usually not necessary).
#' @param time_fix_sd The variance of the over-time component of the first person/legislator
#' is fixed to this value as a reference. 
#' Default is 0.1.
#' @param time_var The mean of the exponential distribution for over-time variances for
#' ideal point parameters. Default (10) is weakly informative on the logit scale.
#' @param ar1_up The upper bound of the AR(1) parameter, default is +1.
#' @param ar1_down The lower bound of the AR(1) parameter, default is 0. Set to -1
#' to allow for inverse responses to time shocks.
#' @param spline_knots Number of knots (essentially, number of points
#' at which to calculate time-varying ideal points given T time points). 
#' Default is NULL, which means that the spline is equivalent to 
#' polynomial time trend of degree `spline_degree`.
#' Note that the spline number (if not null) must be equal or less than 
#' the number of time points--and there is
#' no reason to have it equal to the number of time points as that will likely 
#' over-fit the data.
#' @param spline_degree The degree of the spline polynomial. The default is 2 which is a 
#' quadratic polynomial. A value of 1 will result in independent knots (essentially 
#' pooled across time points T). A higher value will result in wigglier time series. 
#' There is no "correct" value but lower values are likely more stable and easier to 
#' identify.
#' @param boundary_prior If your time series has very low variance (change over time),
#' you may want to use this option to put a boundary-avoiding inverse gamma prior on
#' the time series variance parameters if your model has a lot of divergent transitions. 
#' To do so, pass a list with a element called 
#' `beta` that signifies the rate parameter of the inverse-gamma distribution. 
#' For example, try `boundary_prior=list(beta=1)`. Increasing the value of `beta`
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
#' @param restrict_sd_high Set the level of tightness for high fixed parameters 
#' (top/positive end of scale).
#' If NULL, the default, will set to .1 if `const_type="persons"` and 
#' 10 if `const_type="items"`. For `const_type="persons"`, value is the 
#' SD of normal distribution centered around `fix_high`. For `const_type="items"`,
#' parameter is equal to the prior shape for high pinned parameters 
#' (divide by `restrict_N_high` + `restrict_sd_high`) to get expected value.
#' @param restrict_sd_low Set the level of tightness for low fixed parameters 
#' (low/negative end of scale).
#' If NULL, the default, will set to .1 if `const_type="persons"` and 
#' 10 if `const_type="items"`. For `const_type="persons"`, value is the 
#' SD of normal distribution centered around `fix_low`. For `const_type="items"`,
#' parameter is equal to the prior shape for high pinned parameters 
#' (divide by `restrict_N_low` + `restrict_sd_low`) to get expected value.
#' @param restrict_N_high Set the prior scale for high/positive pinned parameters. Default is 1000 
#' (equivalent to 1,000 observations of the pinned value). Higher values make the pin
#' stronger (for example if there is a lot of data).
#' @param restrict_N_low Set the prior shape for low/negative pinned parameters. Default is 1000 
#' (equivalent to 1,000 observations of the pinned value). Higher values make the pin stronger
#' (for example if there is a lot of data).
#' @param ordbeta_phi_mean The mean of the prior for phi, the dispersion parameter in the
#' ordered beta distribution. Value of this parameter (default is 1) is given as the 
#' mean of the exponential distribution for prior values of phi.
#' @param ordbeta_cut_alpha A length 2 vector of positive continuous values for alpha 
#' in the induced dirichlet distribution. This distribution is used for the cutpoints
#' of the ordered beta distribution. Default is c(1,1), which is uninformative.
#' @param ordbeta_cut_phi A value for the phi paremeter of the induced dirichlet distribution used for ordered beta cutpoint priors. Default is 0, which is weakly informative.
#' @param gp_nugget The nugget of the squared-exponential kernel (equals additional
#' variance of the GP-distributed ideal points)
#' @param gp_alpha The mean of the exponential prior of the alpha parameters of the 
#' GP squared-exponential kernel
#' @param gp_rho The mean of the exponential prior of the rho parameters of the GP
#' squared-exponential kernel
#' @param cmdstan_path_user Default is NULL, and so will default to whatever is set in
#' `cmdstanr` package. Specify a file path  here to use a different `cmdtstan`
#' installation.
#' @param save_files The location to save CSV files with MCMC draws from `cmdstanr`. 
#' The default is `NULL`, which will use a folder in the package directory.
#' @param compile_optim Whether to use Stan compile optimization flags (off by default)
#' @param debug For debugging purposes, turns off threading to enable more informative
#'   error messages from Stan. Also recompiles model objects.
#' @param init_pathfinder Whether to generate initial values from the Pathfinder 
#' algorithm (see Stan documentation). If FALSE, will generate random start values..
#' @param debug_mode Whether to debug code by printing values of log-probability
#' statements to the console. A level of 1 will print log-probability before
#' and after likelihood functions are calculated. A level of 2 will also 
#' print out the log probability contributions of priors. Default is 0.
#' @param ... Additional parameters passed on to Stan's sampling engine. See [cmdstanr::sample] for more information.
#' @return A fitted [idealstan()] object that contains posterior samples of all parameters either via full Bayesian inference
#' or a variational approximation if `use_method` is set to `"pathfinder"` or `"laplace"`. This object can then be passed to the plotting functions for further analysis.
#' @seealso [id_make()] for pre-processing data,
#' [id_plot_persons()] for plotting results,
#' [summary()] for obtaining posterior quantiles,
#' [id_post_pred()] for producing predictive replications.
#' @examples
#' 
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
#' \dontrun{
#' 
#' data('senate114')
#' 
#' # Running this model will take at least a few minutes, even with 
#' # variational inference (use_method="pathfinder") turned on
#' 
#' # first convert absences to NA values
#' 
#' senate114$cast_code[senate114$cast_code=="NA"] <- NA
#' 
#' # reset factor levels
#' 
#' senate114$cast_code <- factor(senate114$cast_code, 
#'                               levels=c("No","Yes"))
#' 
#' to_idealstan <-   id_make(score_data = senate114,
#' outcome_disc = 'cast_code',
#' person_id = 'bioname',
#' item_id = 'rollnumber',
#' group_id= 'party_code',
#' time_id='date')
#' 
#' sen_est <- id_estimate(to_idealstan,
#' model_type = 2,
#' use_method = "pathfinder",
#' fixtype='prefix',
#' restrict_ind_high = "BARRASSO, John A.",
#' restrict_ind_low = "WARREN, Elizabeth")
#' 
#' # After running the model, we can plot 
#' # the results of the person/legislator ideal points
#' 
#' id_plot_persons(sen_est)
#' }
#'
#' @references \enumerate{
#'    \item Clinton, J., Jackman, S., & Rivers, D. (2004). The Statistical Analysis of Roll Call Data. *The American Political Science Review*, 98(2), 355-370. doi:10.1017/S0003055404001194
#'    \item Bafumi, J., Gelman, A., Park, D., & Kaplan, N. (2005). Practical Issues in Implementing and Understanding Bayesian Ideal Point Estimation. *Political Analysis*, 13(2), 171-187. doi:10.1093/pan/mpi010
#'    \item Kubinec, R. "Generalized Ideal Point Models for Time-Varying and Missing-Data Inference". Working Paper.
#'    \item Betancourt, Michael. "Robust Gaussian Processes in Stan". (October 2017). Case Study.
#' }
#' @importFrom stats dnorm dpois model.matrix qlogis relevel rpois update aggregate dlnorm end formula start
#' @importFrom utils person packageDescription install.packages
#' @importFrom posterior as_draws_rvars ess_bulk ess_tail
#' @importFrom bayesplot mcmc_intervals
#' @importFrom rlang check_installed is_installed
#' @export
id_estimate <- function(idealdata=NULL,model_type=2,
                        inflate_zero=FALSE,
                        vary_ideal_pts='none',
                        seed=NULL,
                        keep_param=NULL,
                        grainsize=1,
                        mpi_export=NULL,
                        use_subset=FALSE,sample_it=FALSE,
                        subset_group=NULL,subset_person=NULL,sample_size=20,
                        nchains=4,niters=1000,use_method="mcmc",
                        ignore_db=NULL,
                        restrict_ind_high=NULL,
                        fix_high=2,
                        fix_low=(-2),
                        restrict_ind_low=NULL,
                        num_restrict_high=1,
                        num_restrict_low=1,
                        fixtype='prefix',
                        const_type="persons",
                        id_refresh=0,
                        prior_only=FALSE,
                        warmup=1000,ncores=4,
                        use_groups=FALSE,
                        discrim_reg_upb=1,
                        discrim_reg_lb=-1,
                        discrim_miss_upb=1,
                        discrim_miss_lb=-1,
                        discrim_reg_scale=2,
                        discrim_reg_shape=2,
                        discrim_miss_scale=2,
                        discrim_miss_shape=2,
                        person_sd=3,
                        time_fix_sd=.1,
                        time_var=10,
                        spline_knots=NULL,
                        spline_degree=2,
                        ar1_up=1,
                        ar1_down=-1,
                        boundary_prior=NULL,
                        time_center_cutoff=50,
                        restrict_var=FALSE,
                        ar_prior=c(2,2),
                        diff_reg_sd=3,
                        diff_miss_sd=3,
                        restrict_sd_high=NULL,
                        restrict_sd_low=NULL,
                        restrict_N_high=1000,
                        restrict_N_low=1000,
                        ordbeta_phi_mean=1,
                        ordbeta_cut_alpha=c(1,1,1),
                        ordbeta_cut_phi=0,
                        gp_nugget=.1,
                        gp_rho=.5,
                        gp_alpha=.5,
                        cmdstan_path_user=NULL,
                        map_over_id="persons",
                        save_files=NULL,
                        compile_optim=FALSE,
                        debug=FALSE,
                        init_pathfinder=TRUE,
                        debug_mode=0,
                        ...) {
  
  # check for correct use_method option

  if(!(use_method) %in% c("mcmc","pathfinder","laplace")) stop("You have entered a value for use_method that is not supported. At present the estimation routines are 'mcmc', 'pathfinder', or 'laplace'. Please see the associated working paper for more details at https://osf.io/preprints/osf/8j2bt_v3")

  # Check if cmdstanr is installed first (before attempting check_installed)
  # This prevents errors in non-interactive sessions

  if(!is_installed("cmdstanr")) {

    # In interactive sessions, offer to install
    if(interactive()) {

      install_cmdstanr <- function(pkg, ...) {
        install.packages(pkg, repos = c('https://stan-dev.r-universe.dev'))
      }

      check_installed("cmdstanr",
                      version="0.8.1",
                      reason="idealstan requires the package cmdstanr to run. This package is available from an external repository, https://stan-dev.r-universe.dev.",
                      action=install_cmdstanr)

      # Check again after potential installation
      if(!is_installed("cmdstanr")) {
        message("The R package cmdstanr is not installed so idealstan cannot estimate models. Please go to https://mc-stan.org/cmdstanr/ for installation instructions for the package cmdstanr.")
        return(NULL)
      }

    } else {
      # Non-interactive: just return NULL with message
      message("The R package cmdstanr is not installed so idealstan cannot estimate models. Please go to https://mc-stan.org/cmdstanr/ for installation instructions for the package cmdstanr.")
      return(NULL)
    }
  }
  
  # only allow estimation to proceed if cmdstan is also installed
  
  if(is.null(
    cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    
    message("R package cmdstanr is installed but cmdstan is not yet installed & set up. Please load the R package cmdstanr and use function install_cmdstan() to install cmdstan on your local machine. For more information, see https://mc-stan.org/cmdstanr/.")
    
    return(NULL)
    
  }
  
  # don't allow user to change this
  
  het_var <- TRUE
  
  
  # check to make sure cmdstanr is working
  
  if(is.null(cmdstanr::cmdstan_version())) {
    print("You need to install cmdstan with cmdstanr to compile models. Use the function install_cmdstan() in the cmdstanr package.")
  }
  
  # set path if user specifies
  if(!is.null(cmdstan_path_user))
    cmdstanr::set_cmdstan_path(cmdstan_path_user)
  
  if(!file.exists(system.file("stan_files","irt_standard_map",
                              package="idealstan"))) {
    print("Compiling model. Will take some time as this is the first time the package has been used.")
    print("Have you thought about donating to relief for victims of Yemen's famine?")
    print("Check out https://www.unicef.org/emergencies/yemen-crisis for more info.")
  }
  
  stan_code_map <- system.file("stan_files","irt_standard_map.stan",
                               package="idealstan")
  
  # stan_code_gpu <- system.file("stan_files","irt_standard_gpu.stan",
  #                              package="idealstan")
  # 
  if(compile_optim) {

    idealdata@stanmodel_map <- stan_code_map %>%
      cmdstanr::cmdstan_model(include_paths=dirname(stan_code_map),
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
      cmdstanr::cmdstan_model(include_paths=dirname(stan_code_map),
                    cpp_options = list(stan_threads = !debug),
                    force_recompile=debug)
    
    # for pathfinder
    
    # idealdata@stanmodel_gpu <- stan_code_gpu %>%
    #   cmdstan_model(include_paths=dirname(stan_code_map),
    #                 cpp_options = list(stan_threads = !debug,
    #                                    STAN_OPENCL=TRUE,
    #                                    opencl_platform_id = 0,
    #                                    opencl_device_id = 0),
    #                 force_recompile=debug)
    
    
  }
  
  # add in default arguments if missing
  
  if(is.null(restrict_sd_high)) {
    
    if(const_type=="persons") {
      
      restrict_sd_high <- .1
      
    } else {
      
      restrict_sd_high <- 10
      
    }
    
  }
  
  if(is.null(restrict_sd_low)) {
    
    if(const_type=="persons") {
      
      restrict_sd_low <- .1
      
    } else {
      
      restrict_sd_low <- 10
      
    }
    
  }
  
  
  # load and process data, return list to send to cmdstanr
  
  # Pass all arguments to the second function
  # make a list so we can save it for later
  
  eval_data_args <- list(idealdata=idealdata,
                         inflate_zero=inflate_zero,
                         model_type=model_type,
                         vary_ideal_pts=vary_ideal_pts,
                         keep_param=keep_param,
                         grainsize=grainsize,
                         use_subset=use_subset,sample_it=sample_it,
                         subset_group=subset_group,subset_person=subset_person,
                         sample_size=sample_size,
                         ignore_db=ignore_db,
                         restrict_ind_high=restrict_ind_high,
                         fix_high=fix_high,
                         fix_low=fix_low,
                         restrict_ind_low=restrict_ind_low,
                         fixtype=fixtype,
                         const_type=const_type,
                         id_refresh=id_refresh,
                         prior_only=prior_only,
                         use_groups=use_groups,
                         discrim_reg_upb=discrim_reg_upb,
                         discrim_reg_lb=discrim_reg_lb,
                         discrim_miss_upb=discrim_miss_upb,
                         discrim_miss_lb=discrim_miss_lb,
                         discrim_reg_scale=discrim_reg_scale,
                         discrim_reg_shape=discrim_reg_shape,
                         discrim_miss_scale=discrim_miss_scale,
                         discrim_miss_shape=discrim_miss_shape,
                         person_sd=person_sd,
                         time_fix_sd=time_fix_sd,
                         time_var=time_var,
                         spline_knots=spline_knots,
                         spline_degree=spline_degree,
                         ar1_up=ar1_up,
                         ncores=ncores,
                         ar1_down=ar1_down,
                         boundary_prior=boundary_prior,
                         time_center_cutoff=time_center_cutoff,
                         restrict_var=restrict_var,
                         ar_prior=ar_prior,
                         diff_reg_sd=diff_reg_sd,
                         diff_miss_sd=diff_miss_sd,
                         restrict_sd_high=restrict_sd_high,
                         restrict_sd_low=restrict_sd_low,
                         restrict_N_high=restrict_N_high,
                         restrict_N_low=restrict_N_low,
                         gp_nugget=gp_nugget,
                         gp_rho=gp_rho,
                         gp_alpha=gp_alpha,
                         map_over_id=map_over_id,
                         het_var=het_var, 
                         debug_mode=debug_mode,
                         num_restrict_high=num_restrict_high,
                         num_restrict_low=num_restrict_low,
                         ordbeta_phi_mean=ordbeta_phi_mean,
                         ordbeta_cut_alpha=ordbeta_cut_alpha,
                         ordbeta_cut_phi=ordbeta_cut_phi)
  
  all_data <- do.call(.make_stan_data,eval_data_args)
  
  this_data <- all_data$stan_data
  remove_list <- all_data$remove_list
  idealdata <- all_data$idealdata
  out_list <- all_data$out_list
  
  # need to save n_cats
  
  idealdata@n_cats_rat <- remove_list$n_cats_rat
  idealdata@n_cats_grm <- remove_list$n_cats_grm
  idealdata@order_cats_rat <- remove_list$order_cats_rat
  idealdata@order_cats_grm <- remove_list$order_cats_grm
  
  outobj <- sample_model(object=idealdata,nchains=nchains,niters=niters,warmup=warmup,ncores=ncores,
                         this_data=this_data,use_method=use_method,
                         save_files=save_files,
                         keep_param=keep_param,
                         within_chain=within_chain,
                         init_pathfinder=init_pathfinder,
                         seed=seed,
                         ...)
  
  outobj@model_type <- model_type
  outobj@time_proc <- this_data$time_proc
  outobj@use_groups <- use_groups
  outobj@map_over_id <- map_over_id
  outobj@time_fix_sd <- time_fix_sd
  outobj@restrict_var <- restrict_var
  outobj@time_center_cutoff <- time_center_cutoff
  outobj@orig_order <- out_list$this_data$orig_order
  outobj@this_data <- this_data
  outobj@remove_nas <- remove_list$remove_nas
  outobj@eval_data_args <- eval_data_args
  outobj@use_method <- use_method
  
  # need to recalculate legis points if time series used
  if(this_data$T>1 && ((!is.null(keep_param$person_vary) && keep_param$person_vary) || is.null(keep_param))) {
    outobj@time_varying <- try(.get_varying(outobj,
                                            person_id=this_data$ll,
                                            time_id=this_data$time))
  }
  
  return(outobj)
  
}
