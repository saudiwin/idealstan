#' Plot Legislator/Person and Bill/Item Ideal Points
#' 
#' This function can be used on a fitted \code{idealstan} object to plot the relative positions and 
#' uncertainties of legislator/persons and bills/items.
#' 
#' This plot shows the distribution of ideal points for the legislators/persons in the model. It will plot them as a vertical
#' dot plot with associated high-density posterior interval (can be changed 
#' with \code{high_limit} and \code{low_limit} options). In addition, if item/bill IDs
#' as a character vector is passed to the \code{item_plot} option, then an item/bill midpoint will be overlain
#' on the ideal point plot, showing the point at which legislators/persons are indifferent to voting/answering on the 
#' bill/item. Note that because this is an ideal point model, it is not possible to tell from the midpoint itself
#' which side will be voting which way. For that reason, the legislators/persons are colored by their votes/scores to
#' make it clear.
#' 
#' To compare across multiple \code{idealstan} models, pass a named list 
#' \code{list(model1=model1,model2=model2,etc)} to the \code{object} option. 
#' Note that these comparisons will done by individual persons/groups, so if there are a lot of 
#' persons/groups, consider using the \code{include} option to only compare a specific set
#' of persons/groups.
#' 
#' @param object A fitted \code{idealstan} object or a named list
#' of \code{idealstan} objects to compare across models
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param high_limit The quantile (number between 0 and 1) for the high end of posterior uncertainty to show in plot
#' @param low_limit The quantile (number between 0 and 1) for the low end of posterior uncertainty to show in plot
#' @param item_plot The IDs (character vector) of the bill/item midpoints to overlay on the plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param point_size If \code{person_labels} and \code{group_labels} are set to \code{FALSE}, controls the size of the points plotted.
#' @param hjust_length horizontal adjustment of the legislator labels
#' @param person_labels if \code{TRUE}, use the person_id column to plot labels for the person (legislator) ideal points
#' @param group_labels if \code{TRUE}, use the group column to plot text markers for the group (parties) from the person/legislator data
#' @param person_ci_alpha The transparency level of the dot plot and confidence bars for the person ideal points
#' @param item_plot_type Whether to show the \code{'non-inflated'} item/bill midpoints, 
#' the \code{'inflated'} item/bill midpoints, or produce plots for \code{'both'} kinds of models. 
#' Defaults to \code{'non-inflated'} and will only display an item/bill midpoint if one has been 
#' specified in \code{item_plot}.
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param group_color If \code{TRUE}, give each group/bloc a different color
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Leave NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param ... Other options passed on to plotting function, currently ignored
#' @import ggplot2
#' @import lazyeval
#' @importFrom rlang parse_quosure as_quosure
#' @import ggrepel
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' # First create data and run a model
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
id_plot_legis <- function(object,return_data=FALSE,
                          include=NULL,
                          high_limit=.95,
                          low_limit=.05,
                          item_plot=NULL,
                          item_plot_type='non-inflated',
                       text_size_label=2,text_size_group=2.5,
                       point_size=1,
                       hjust_length=-0.7,
                       person_labels=TRUE,
                       group_labels=F,
                       person_ci_alpha=0.2,
                       show_true=FALSE,group_color=TRUE,
                       hpd_limit=10,
                       sample_persons=NULL,...) {
  
  if(class(object)=='idealstan') {
    person_params <- .prepare_legis_data(object,
                                         high_limit=high_limit,
                                         low_limit=low_limit)
    model_wrap <- FALSE
    use_groups <- object@use_groups
  } else {
    # loop over lists
    person_params <- lapply(object,.prepare_legis_data,
                            high_limit=high_limit,
                            low_limit=low_limit) %>% 
      bind_rows(.id='Model')
    
    check_groups <- sapply(object,function(x) x@use_groups)
    if(all(check_groups)) {
      use_groups <- T
    } else {
      use_groups <- F
    }
    # now we can facet by persons/groups
    model_wrap <- TRUE
    group_color <- FALSE
    # assume all data is the same
    object <- object[[1]]
  }

  
  if(!is.null(include)) {
    person_params <- filter(person_params, person_id %in% include)
  }
  
  if(group_color && !model_wrap) {
    groupc <- ~group_id
  } else if(!group_color && !model_wrap) {
    groupc <- NA
  } else {
    # if multiple models, subset by models
    groupc <- ~Model
  }

  # sample for plot only
  
  if(!is.null(sample_persons)) {
    if(!is.numeric(sample_persons) && !(sample_persons>0 && sample_persons<1)) {
      stop('Please enter a fraction to sample from between 0 and 1 as a numeric value.')
    }
    to_sample <- sample(1:nrow(person_params),round(sample_persons*nrow(person_params)))
    person_params <- slice(person_params,to_sample)
  }
  
  
  # Rescale simulated values to ensure that they match estimated values in terms of scale multiplicativity

  if(show_true==TRUE) {
    
    true_vals <- data_frame(true_vals=object@score_data@simul_data$true_person[,1]) %>% 
      slice(as.numeric(levels(person_params$person_id))) %>% 
      mutate(id_num=1:n())
    
    person_params <- left_join(person_params,true_vals)
  }
  
  if(!is.null(hpd_limit)) {
    person_params <- filter(person_params,
                            abs(high_pt-low_pt)<hpd_limit)
  }
  
  # create item plot data
  
  if(!is.null(item_plot)) {
    
    # loop over the item IDs and calculate midpoints and HPDs
    if(object@model_type %in% c(1,2) || (object@model_type>6 && object@model_type<13)) {
      # binary models and continuous
      item_points <- lapply(item_plot,.item_plot_binary,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(3,4)) {
      # rating scale
      item_points <- lapply(item_plot,.item_plot_ord_rs,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(5,6)) {
      # grm
      item_points <- lapply(item_plot,.item_plot_ord_grm,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(13,14)) {
      # latent space
      item_points <- lapply(item_plot,.item_plot_ls,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    }
    
    # collect outcomes
    
    item_points <- left_join(item_points,object@score_data@score_matrix,by=c('item_name'='item_id')) %>% 
      gather(key='ci_type',value='ci_value',item_high,item_low) %>% 
      mutate(ci_type=factor(ci_type,levels=c('item_low',
                                     'item_high'),
                            labels=c(paste0(low_limit*100,'%'),
                                     paste0(high_limit*100,'%'))))
    person_params <- left_join(person_params,item_points,by=c('person_id'='person_id',
                                                              'group_id'='group_id'))
    # Choose a plot based on the user's options
    
    if(item_plot_type!='both') {
      
      item_plot_type <- switch(item_plot_type,
                               `non-inflated`="Non-Inflated\nDiscrimination",
                               inflated="Inflated\nDiscrimination")
      
      person_params <- filter(person_params,item_type==item_plot_type)
    } 
    
  }
  
  # Default plot: group names plotted as points

  if(is.null(item_plot)) {
    outplot <- person_params %>% ggplot() +
      geom_linerange(aes_(x=~reorder(person_id,median_pt),
                         ymin=~low_pt,ymax=~high_pt,color=groupc),
                     alpha=person_ci_alpha,
                     show.legend = FALSE) +
      geom_text(aes_(x=~reorder(person_id,median_pt),
                    y=~median_pt,label=~reorder(group_id,median_pt),
                    color=groupc),size=text_size_group,
                show.legend = FALSE,
                check_overlap = T) 
      
  } else {
    # if an item plot is being made, use the actual outcomes as points

    outplot <- person_params %>% ggplot() +
      geom_linerange(aes_(x=~reorder(person_id,median_pt),colour=groupc,
                         ymin=~low_pt,ymax=~high_pt),alpha=person_ci_alpha) +
      geom_text(aes_(x=~reorder(person_id,median_pt),y=~median_pt,
                    colour=groupc,
                    label=~reorder(outcome,median_pt)),size=text_size_group,
                check_overlap = T)
    
  }
    
    # determine if legislator names should be plotted

  if(person_labels==TRUE & group_color==TRUE) {
    outplot <- outplot + geom_text_repel(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt),color=group_id),
                                       nudge_x=hjust_length,size=text_size_label,show.legend = FALSE,
                                       segment.alpha=0,data=distinct(person_params,
                                                                     person_id,
                                                                     median_pt,
                                                                     group_id))
  } else if(person_labels==TRUE & group_color==FALSE) {
    outplot <- outplot + geom_text_repel(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt)),
                                   nudge_x=hjust_length,size=text_size_label,
                                   segment.alpha=0,data=distinct(person_params,
                                                                 person_id,
                                                                 median_pt,
                                                                 group_id))
  }
  
    
  if(!is.null(item_plot)) {
    
    # Add in bill vertical line (50% equiprobability voting line)
      outplot <- outplot + geom_hline(aes(yintercept=item_median),linetype=2,alpha=0.6) +
        geom_rug(aes(y=ci_value,linetype=ci_type)) +
        guides(linetype=guide_legend('Item\nMidpoint\nHPD'),
               colour=guide_legend('')) +
        theme(legend.position = 'bottom')
     
    #Whether or not to add a facet_wrap
    
    if(item_plot_type=='both' & length(item_plot)>1) {
      outplot <- outplot + facet_wrap(~item_name + item_type,dir='v')
    } else if(item_plot_type=='both') {
      outplot <- outplot + facet_wrap(~item_type,dir='v') 
    } 
    
    
  }
  
  # Add a dirty trick to enable the legend to show what group labels instead of just the letter 'a'
  
  if(group_color==TRUE) {
    
    outplot <- outplot + geom_point(aes(x=reorder(person_id,median_pt),y=median_pt,color=group_id),size=0,stroke=0) +
      guides(colour = guide_legend(title="",override.aes = list(size = 5)))
  }
  
  if(show_true==TRUE) {
    outplot <- outplot + geom_point(aes(x=reorder(person_id,median_pt),y=true_vals),color='black',shape=2)
    
  }
  
  # Add theme elements
  
  outplot <- outplot  +  ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank(),
          strip.background = element_blank(),
          panel.background = element_blank()) + coord_flip() 
  
  if(model_wrap) {
    # facet by groups/persons
    outplot <- outplot + facet_wrap(~group_id)
  }

  
  if(return_data==TRUE) {
    
    return_list <- list(outplot=outplot,plot_data=person_params)

    return(return_list)
    
  } else (
    return(outplot)
  )
  
}

#' Plot Legislator/Person Over-time Variances
#' 
#' This function can be used on a fitted \code{idealstan} object to plot the over-time variances 
#' (average rates of change in ideal points) for all the persons/legislators in the model.
#' 
#' This function will plot the person/legislator over-time variances as a vertical
#' dot plot with associated high-density posterior interval (can be changed 
#' with \code{high_limit} and \code{low_limit} options). 
#' 
#' @param object A fitted \code{idealstan} object
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param high_limit The quantile (number between 0 and 1) for the high end of posterior uncertainty to show in plot
#' @param low_limit The quantile (number between 0 and 1) for the low end of posterior uncertainty to show in plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param point_size If \code{person_labels} and \code{group_labels} are set to \code{FALSE}, controls the size of the points plotted.
#' @param hjust_length horizontal adjustment of the legislator labels
#' @param person_labels if \code{TRUE}, use the person_id column to plot labels for the person (legislator) ideal points
#' @param group_labels if \code{TRUE}, use the group column to plot text markers for the group (parties) from the person/legislator data
#' @param person_ci_alpha The transparency level of the dot plot and confidence bars for the person ideal points
#' @param group_color If \code{TRUE}, give each group/bloc a different color
#' @param ... Other options passed on to plotting function, currently ignored
#' @import ggplot2
#' @import lazyeval
#' @importFrom rlang parse_quosure as_quosure
#' @export
#' @examples 
#' 
#' # To demonstrate, we load the 114th Senate data and fit a time-varying model
#' 
#' data('senate114_fit')
#' 
#' \dontrun{
#' senate_data <- id_make(senate114,outcome = 'cast_code',
#' person_id = 'bioname',
#' item_id = 'rollnumber',
#' group_id= 'party_code',
#' time_id='date',
#' miss_val='Absent')
#' 
#'  senate114_time_fit <- id_estimate(senate_data,
#'  model_type = 2,
#'  use_vb = T,
#'  fixtype='vb_partial',
#'  vary_ideal_pts='random_walk',
#'  restrict_ind_high = "WARREN, Elizabeth",
#'  restrict_ind_low="BARRASSO, John A.",
#'  seed=84520)
#' # We plot the variances for all the Senators
#' 
#' id_plot_legis_var(senate114_fit)
#' }
id_plot_legis_var <- function(object,return_data=FALSE,
                              include=NULL,
                          high_limit=.95,
                          low_limit=.05,
                          text_size_label=2,text_size_group=2.5,
                          point_size=1,
                          hjust_length=-0.7,
                          person_labels=TRUE,
                          group_labels=F,
                          person_ci_alpha=0.1,group_color=TRUE,
                          ...) {
  
  person_params <- .prepare_legis_data(object,
                                       high_limit=high_limit,
                                       low_limit=low_limit,
                                       type='variance')
  
  if(object@use_groups) {
    person_params$person_id <- person_params$group_id 
    person_params <- person_params %>% distinct
  }
  
  if(!is.null(include)) {
    person_params <- filter(person_params, person_id %in% include)
  }
  

  
  # Default plot: group names plotted as points
  
  if(group_color==TRUE) {
    outplot <- person_params %>% ggplot() +
      geom_linerange(aes(x=reorder(person_id,median_pt),
                         ymin=low_pt,ymax=high_pt,color=group_id),
                     alpha=person_ci_alpha,
                     show.legend = FALSE) +
      geom_text(aes(x=reorder(person_id,median_pt),
                    y=median_pt,label=reorder(group_id,median_pt),
                    color=group_id),size=text_size_group,show.legend = FALSE) 
  } else if(group_color==FALSE) {
    outplot <- person_params %>% ggplot() +
      geom_linerange(aes(x=reorder(person_id,
                                   median_pt),
                         ymin=low_pt,ymax=high_pt),
                     alpha=person_ci_alpha) +
      geom_text(aes(x=reorder(person_id,median_pt),y=median_pt,
                          label=reorder(group_id,median_pt)),size=text_size_group)
    
  }
  
  # determine if legislator names should be plotted
  
  if(person_labels==TRUE & group_color==TRUE) {
    outplot <- outplot + geom_text_repel(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt),color=group_id),
                                   nudge_x=hjust_length,size=text_size_label,show.legend = FALSE,
                                   segment.alpha=0)
  } else if(person_labels==TRUE & group_color==FALSE) {
    outplot <- outplot + geom_text_repel(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt)),
                                   nudge_x=hjust_length,size=text_size_label,
                                   segment.alpha=0)
  }
  
  
  # Add a dirty trick to enable the legend to show what group labels instead of just the letter 'a'
  
  if(group_color==TRUE) {
    
    outplot <- outplot + geom_point(aes(x=reorder(person_id,median_pt),y=median_pt,color=group_id),size=0,stroke=0) +
      guides(colour = guide_legend(title="",override.aes = list(size = 5)))
  }
  
  # Add theme elements
  
  outplot <- outplot  + theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip() 
  
  
  if(return_data==TRUE) {
    
    return_list <- list(outplot=outplot,plot_data=person_params)
    return(return_list)
    
  } else (
    return(outplot)
  )
  
}

#' Function to plot dynamic ideal point models
#' 
#' This function can be used on a fitted \code{idealstan} object to plot the relative positions and 
#' uncertainties of legislator/persons and bills/items when the legislator/person ideal points
#' are allowed to vary over time.
#' 
#' This plot shows the distribution of ideal points for the legislators/persons in the model,
#' and also traces the path of these ideal points over time. It will plot them as a vertical
#' line with associated high-density posterior interval (10\% to 90\%). In addition, if the column index for a 
#' bill/item from the response matrix is passed to the \code{item_plot} option, then an item/bill midpoint will be overlain
#' on the ideal point plot, showing the point at which legislators/persons are indifferent to voting/answering on the 
#' bill/item. Note that because this is an ideal point model, it is not possible to tell from the midpoint itself
#' which side will be voting which way. For that reason, the legislators/persons are colored by their votes/scores to
#' make it clear.
#' 
#' @param object A fitted \code{idealstan} object or a named list of \code{idealstan}
#' objects if the plot is supposed to show a comparison of different fitted \code{idealstan}
#' models (see Time Series vignette)
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param item_plot The value of the item/bill for which to plot its midpoint (character value)
#' @param item_plot_type Whether to show the \code{'non-inflated'} item/bill midpoints, 
#' the \code{'inflated'} item/bill midpoints, or produce plots for \code{'both'} kinds of models. 
#' Defaults to \code{'non-inflated'} and will only display an item/bill midpoint if one has been 
#' specified in \code{item_plot}.
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param high_limit A number between 0 and 1 showing the upper limit to compute the 
#' posterior uncertainty interval (defaults to 0.95).
#' @param low_limit A number between 0 and 1 showing the lower limit to compute the 
#' posterior uncertainty interval (defaults to 0.05).
#' @param line_size Sets the size of the line of the time-varying ideal points.
#' @param group_color If \code{TRUE}, use the groups instead of individuals to plot colours
#' @param highlight A character referring to one of the persons in \code{person_labels} that the plot can highlight relative to other persons
#' @param person_ci_alpha The transparency level of ribbon confidence interval around the time-varying ideal points
#' @param person_line_alpha The transparency level of the time-varying ideal point line
#' @param plot_text If \code{TRUE}, will plot \code{person_labels} over the lines.
#' @param use_ci Whether or not high-posterior density intervals (credible intervals) should be
#' plotted over the estimates (turn off if the plot is too busy)
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Leave NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param plot_sim Whether to plot the true values of parameters if a simulation was used to generate data 
#' (see \code{\link{id_sim_gen}})
#' @param ... Other options passed on to plotting function, currently ignored
#' @importFrom gghighlight gghighlight
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' # First create data and run a model
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
#' vary_ideal_pts='random_walk',
#' fixtype='vb_partial',
#' restrict_ind_high = "BARRASSO, John A.",
#' restrict_ind_low = "WARREN, Elizabeth")
#' 
#' # After running the model, we can plot 
#' # the results of the person/legislator ideal points
#' 
#' id_plot_legis_dyn(sen_est)
#' }
id_plot_legis_dyn <- function(object,return_data=FALSE,
                              include=NULL,item_plot=NULL,
                              text_size_label=2,text_size_group=2.5,
                              high_limit=0.95,
                              low_limit=0.05,
                              line_size=1,
                              highlight=NULL,
                              plot_text=TRUE,
                              use_ci=TRUE,
                              person_line_alpha=0.3,
                              person_ci_alpha=0.8,
                              item_plot_type='non-inflated',show_true=FALSE,group_color=TRUE,
                              hpd_limit=10,
                              sample_persons=NULL,
                              plot_sim=FALSE,...) {
  
  # prepare data
  
  person_labels <- quo(person_id)
  group_labels <- quo(group_id)
  already_facet <- FALSE
  
  if(class(object)=='idealstan') {
    person_params <- .prepare_legis_data(object,
                                         high_limit=high_limit,
                                         low_limit=low_limit)
    model_wrap <- FALSE
    use_groups <- object@use_groups
  } else {
    # loop over lists
    person_params <- lapply(object,.prepare_legis_data,
                            high_limit=high_limit,
                            low_limit=low_limit) %>% 
      bind_rows(.id='Model')
    
    check_groups <- sapply(object,function(x) x@use_groups)
    if(all(check_groups)) {
      use_groups <- T
    } else {
      use_groups <- F
    }
    # now we can facet by persons/groups
    model_wrap <- TRUE
    # assume all data is the same
    object <- object[[1]]
  }
  
  # create item plot data
  
  if(!is.null(item_plot)) {
    
    # loop over the item IDs and calculate midpoints and HPDs
    if(object@model_type %in% c(1,2) || (object@model_type>6 && object@model_type<13)) {
      # binary models and continuous
      item_points <- lapply(item_plot,.item_plot_binary,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(3,4)) {
      # rating scale
      item_points <- lapply(item_plot,.item_plot_ord_rs,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(5,6)) {
      # grm
      item_points <- lapply(item_plot,.item_plot_ord_grm,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    } else if(object@model_type %in% c(13,14)) {
      # latent space
      item_points <- lapply(item_plot,.item_plot_ls,object=object,
                            low_limit=low_limit,
                            high_limit=high_limit) %>% bind_rows()
    }
    
    # collect outcomes
    
    item_points <- left_join(item_points,object@score_data@score_matrix,by=c('item_name'='item_id')) %>% 
      gather(key='ci_type',value='ci_value',item_high,item_low) %>% 
      mutate(ci_type=factor(ci_type,levels=c('item_low',
                                             'item_high'),
                            labels=c(paste0(low_limit*100,'%'),
                                     paste0(high_limit*100,'%'))))
    person_params <- left_join(person_params,item_points,by=c('person_id'='person_id',
                                                              'group_id'='group_id'))
    person_params$time_id <- person_params$time_id.x
    person_params$time_id.x <- NULL
    person_params$time_id.y <- NULL
    person_params <- spread(person_params,key=ci_type,value=ci_value)
    # Choose a plot based on the user's options
    
    if(item_plot_type!='both') {
      
      item_plot_type <- switch(item_plot_type,
                               `non-inflated`="Non-Inflated\nDiscrimination",
                               inflated="Inflated\nDiscrimination")
      
      person_params <- filter(person_params,item_type==item_plot_type)
    } 
    
  }
  
  if(!is.null(include)) {
    if(use_groups) {
      person_params <- filter(person_params, group_id %in% include)
    } else {
      person_params <- filter(person_params, person_id %in% include)
    }
    
  }
  
  if(use_groups && !model_wrap) {
    base_id <- ~group_id
  } else if(!use_groups && !model_wrap) {
    base_id <- ~person_id
  } else {
    # set base id to the model type
    base_id <- ~Model
    if(use_groups) {
      wrap_id <- ~group_id
    } else {
      wrap_id <- ~person_id
    }
  }
  
  # allow the option of plotting "true" ideal points instead of estimated ones as lines
  if(!is.null(object@score_data@simul_data) && plot_sim==T) {
    
    true_pts <- object@score_data@simul_data$true_person
    colnames(true_pts) <- c(as.character(1:ncol(true_pts)))
    true_pts <- as_data_frame(true_pts) %>% mutate(person_id=1:n()) %>% 
      gather(key = time_id,value=true_pt,-person_id) %>% 
      # need to flip for identification
      mutate(time_id=as.numeric(time_id),
             person_id=factor(person_id),
             person_id=fct_relevel(person_id,
                                   as.character(object@score_data@restrict_ind_low),
                                   as.character(object@score_data@restrict_ind_high),
                                   
                                   after=length(levels(person_id))))
    person_params <- left_join(person_params,true_pts,by=c("person_id","time_id"))
    
  }
  
  
  # plot CIs first for background
  
  if(use_ci==T) {
    outplot <- person_params %>% ggplot(aes_(x=~time_id)) + geom_ribbon(aes_(ymin=~low_pt,
                                          ymax=~high_pt,
                                          group=base_id),
                                     fill='grey80',
                                     colour=NA,
                                     alpha=person_ci_alpha)
  } else {
    outplot <- person_params %>% ggplot(aes_(x=~time_id))
  } 
  
  # add time-varying ideal points\
  if(!is.null(object@score_data@simul_data) && plot_sim==T) {
    
    outplot <- outplot + 
      geom_line(aes_(y=~true_pt,colour=base_id),
                alpha=person_ci_alpha,
                size=line_size)
    
  } else {
    if(group_color) {
      if(model_wrap) {
        outplot <- outplot + 
          geom_line(aes_(y=~median_pt,group=base_id,
                         colour=~Model),
                    alpha=person_ci_alpha,
                    size=line_size)
      } else {
        outplot <- outplot + 
          geom_line(aes_(y=~median_pt,group=base_id,
                         colour=~group_id),
                    alpha=person_ci_alpha,
                    size=line_size)
      }
      
    } else {
      
      outplot <- outplot + 
        geom_line(aes_(y=~median_pt,group=base_id),
                  alpha=person_ci_alpha,
                  size=line_size)
    }
  }
  
  # add item plot if it exists
  
  if(!is.null(item_plot)) {
    
    # Add in bill vertical line (50% equiprobability voting line)
    outplot <- outplot + geom_hline(aes(yintercept=item_median),linetype=2,alpha=0.6) +
      geom_ribbon(aes(ymin=`5%`,
                      ymax=`95%`),alpha=0.2,fill='pink') +
      guides(colour=guide_legend('')) +
      theme(legend.position = 'bottom')
    
    #Whether or not to add a facet_wrap
    # Note interferes with model_wrap
    if(item_plot_type=='both' && length(item_plot)>1) {
      if(model_wrap) {
        outplot <- outplot + facet_wrap(update(wrap_id,~. + item_name + item_type),dir='v',
                                        ncol=length(pull(person_params,Model)))
      } else {
        outplot <- outplot + facet_wrap(~item_name + item_type ,dir='v')
      }
      
    } else if(item_plot_type=='both') {
      if(model_wrap) {
        outplot <- outplot + facet_wrap(update(wrap_id,~ . + item_type),dir='v',
                                        ncol=length(pull(person_params,Model)))
      } else {
        outplot <- outplot + facet_wrap(~item_type,dir='v')
      }
       
    } 
    # record state
    already_facet <- TRUE
    
  }

  
  # plot random labels
  
  if(plot_text==TRUE) {
    
    # need new data that scatters names around the plot
    if(model_wrap) {
      sampled_data <- group_by(person_params,!!as_quosure(base_id),!!as_quosure(wrap_id)) %>% sample_n(1)
    } else {
      sampled_data <- group_by(person_params,!!as_quosure(base_id)) %>% sample_n(1)
    }
    
    
    if(!is.null(highlight)) {
      
      sampled_data <- filter(sampled_data,!(!!as_quosure(base_id) %in% highlight))
    }
    
    outplot <- outplot + 
      geom_text_repel(aes_(x=~time_id,y=~median_pt,label=base_id),data=sampled_data,
                size=text_size_label,segment.colour='grey50',segment.alpha = 0.5)
    
  }
  
  # select some special lines to highlight
  
  if(!is.null(highlight)) {
    # give some of the persons a special color and make bigger
    
    outplot <- outplot + 
      gghighlight(!!as_quosure(base_id) %in% highlight,use_group_by = F)
  }
  
  # only use a legend if groups are used or highlights
  
  if(group_color==F || (group_color==F && is.null(highlight))) {
    output <- outplot + 
      guides(color=FALSE,
             fill=FALSE)
  }
  
  outplot <- outplot +
    theme_minimal() + ylab("Ideal Point Scale") + xlab("") +
    theme(panel.grid= element_blank())
  
  # if not already facetted, facet
  
  if(!already_facet && model_wrap) {
    outplot <- outplot + facet_wrap(wrap_id,scales = 'free_y',
                                    ncol=length(pull(person_params,Model)))
  }
  
  return(outplot)
  
}

#' Function to compare two fitted idealstan models by plotting ideal points. Assumes that underlying data
#' is the same for both models.
#' @param scale_flip This parameter is set to true if you have two models that are reflected around the ideal point axis. This can happen as a result of identification and is harmless.
#' @param model1 The first model to compare
#' @param model2 The second model to compare
#' @param return_data Whether to return the underlying data
#' @param labels \code{TRUE} or \code{FALSE}, whether to use labels for points
#' @param hjust The horizontal adjustment of point labels
#' @param palette \code{colorbrewer} palette name
#' @param color_direction Whether to reverse the color scale
#' @param text_size_label Size of point labels
#' @param rescale Whether to rescale the estimates from two models so they will match regardless of arbitrary scale shifts in the 
#' ideal points
#' @export
id_plot_compare <- function(model1=NULL,model2=NULL,scale_flip=FALSE,return_data=FALSE,
                           labels=NULL,hjust=-.1,palette='Set1',color_direction=1,
                           text_size_label=2,
                           rescale=FALSE) {
  

  data1 <- legis_plot(model1,return_data=TRUE)$plot_data
  data2 <- legis_plot(model2,return_data=TRUE)$plot_data
  data1 <- mutate(data1,this_model='Model1')
  data2 <- mutate(data2,this_model='Model2')
  
  if(scale_flip==TRUE) {
    data1 <- mutate(data1,low_pt=low_pt*-1,
                    high_pt=high_pt*-1,
                    median_pt=median_pt*-1)
  }
  
  combined_data <- bind_rows(data1,data2)
  
  if(rescale==TRUE) {
    combined_data <- mutate(combined_data, median_pt=scale(median_pt)[,1])
  }
  
  outplot <- combined_data %>% ggplot(aes(y=reorder(person_id,median_pt),x=median_pt,color=this_model)) + 
    geom_point() + geom_text_repel(aes(label=reorder(person_id,median_pt)),size=text_size_label,
                             hjust=hjust) +
    geom_errorbarh(aes(xmin=low_pt,xmax=high_pt)) + theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank())
  
  if(!is.null(labels)) {
    outplot <- outplot + scale_colour_brewer(palette=palette,labels=labels,direction=color_direction,
                                             guide=guide_legend(title=''))
  }
  
  if(return_data==TRUE) {
    return(list(plot=outplot,plot_data=combined_data))
  } else {
    return(outplot)
  }
  
}

#' Density plots of Posterior Parameters
#' 
#' This function produces density plots of the different types of parameters in an \code{idealstan} model: item (bill) difficulty and discrimination
#'  parameters, and person (legislator) ideal points.
#'  
#' @param object A fitted \code{idealstan} object
#' @param params Select the type of parameter from the model to plot. \code{'person'} for person/legislator ideal points,
#'  \code{'miss_diff'} and \code{'miss_discrim'} for difficulty and discrimination parameters from the missing/inflated item/bill parameters,
#'  and \code{'regular_diff'} and \code{'regular_discrim'} for difficulty and discrimination parameters from the non-missing/non-inflated 
#'  item/bill parameters.
#' @param param_labels A vector of labels equal to the number of parameters. Primarily useful if \code{return_data} is \code{TRUE}.
#' @param dens_type Can be \code{'all'} for showing 90% HPD high, 10% HPD low and median posterior values. 
#'  Or to show one of those posterior estimates at a time, use \code{'high'} for 90% high HPD posterior estimate,
#'  \code{'low'} for 10% low HPD posterior estimate, and \code{'function'} for the whatever function is specificied
#'  in \code{func} (median by default).
#' @param return_data Whether or not to return the plot as a ggplot2 object and the data together in a list instead of
#'  plotting.
#' @param func The function to use if \code{'dens_type'} is set to \code{'function'}.
#' @param ... Other options passed on to the plotting function, currently ignored.
#' @export
id_plot_all_hist <- function(object,params='person',param_labels=NULL,dens_type='all',
                          return_data=FALSE,func=median,...) {
  
  stan_params <- switch(params,
                   miss_diff='A_int_full',
                   miss_discrim='sigma_abs_full',
                   regular_diff='B_int_full',
                   regular_discrim='sigma_reg_full',
                   person='L_full')
  
  estimates <- rstan::extract(object@stan_samples,pars=stan_params,permuted=T)[[1]]
  
  if(stan_params=='person') {
    #Need to collapse over time
    estimates <- estimates[,1:dim(estimates)[2],]
  }
  
  param_length <- ncol(estimates)
  iters <- nrow(estimates)
  estimates <- estimates %>% as_data_frame %>% 
    gather(param,value) 
  if(!is.null(param_labels)) {
    if(length(param_labels)==param_length) {
      estimates$param_id <- rep(param_labels,each=iters)  
    } else {
      warning('The length of the parameter labels is not the same as the actual number of parameters. Using default labels instead.')
      estimates$param_id <- rep(paste0(params,'_',1:param_length),each=iters)
    }
    
  } else {
    estimates$param_id <- rep(paste0(params,'_',1:param_length),each=iters)
  }
  
  estimates <- select(estimates,value,param_id) %>% group_by(param_id) %>%
    summarize(low_pt=quantile(value,0.1),high_pt=quantile(value,0.9),
                                                          median_pt=func(value))
  estimates <- gather(estimates,obs_type,value,-param_id) %>% 
    mutate(obs_type=recode(obs_type,`high_pt`='90% Posterior Estimates',
           `low_pt`='10% Posterior Estimates',
           `median_pt`="Median Posterior Estimates"))

  if(dens_type=='all') {
    outplot <- ggplot(estimates,aes(x=value)) + geom_density() + theme_minimal() + facet_wrap(~obs_type,
                                                                                              nrow=3,scales = "free")
  } else if(dens_type=='high') {
    outplot <- filter(estimates,obs_type=='high_pt') %>% 
    ggplot(aes(x=value)) + geom_density(size=1.5) + theme_minimal() 
  } else if(dens_type=='low') {
    outplot <- filter(estimates,obs_type=='low_pt') %>% 
      ggplot(aes(x=value)) + geom_density(size=1.5) + theme_minimal() 
  } else if(dens_type=='function') {
    outplot <- filter(estimates,obs_type=='median_pt') %>% 
      ggplot(aes(x=value)) + geom_density(size=1.5) + theme_minimal() 
  }
  outplot <- outplot + xlab('Parameter Posterior Values') + ylab('Density') + 
    theme(panel.grid=element_blank())
  if(return_data==TRUE) {
    return(list(plot=outplot,plot_data=estimates))
  } else {
    return(outplot)
  }
  
  
}
#' This function plots the results from a simulation generated by \code{\link{id_sim_gen}}.
#' 
#' @param sims A fitted \code{idealstan} object that has true data generated by \code{\link{id_sim_gen}}
#' @param type Type of analysis of true versus fitted values, can be \code{'RMSE'}, \code{'Residuals'} or \code{'Coverage'}
#' @export
id_plot_sims <- function(sims,type='RMSE') {

  stat_func <- switch(type,
                      RMSE=id_sim_rmse,
                      Residuals=id_sim_resid,
                      Coverage=id_sim_coverage)
  
  if(type=='Coverage') {
    
  } else {
    stat_func(sims) %>% 
      bind_rows(.id='ID') %>% 
      ggplot(aes(y=avg,x=Params,ymax=high,ymin=low)) + geom_pointrange(size=.3) +
      theme_minimal() + xlab("Parameters") + ylab(type) + facet_wrap(~ID,scales = "free",
                                                                     ncol=1) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank()) + 
      geom_hline(aes(yintercept=(mean(avg))),linetype=3) 
  }

  
}

#' Plotting Function to Display Rhat Distribution
#' 
#' This plotting function displays a histogram of the Rhat values of all parameters in an \code{idealstan} model.
#' 
#' @param obj A fitted \code{idealstan} object.
#' 
#' @export
id_plot_rhats <- function(obj) {
  # first get all summaries
  get_out <- rstan::summary(obj@stan_samples)$summary
  data_frame(Rhats=get_out[,'Rhat']) %>% 
    ggplot(aes(x=Rhats)) + theme_minimal() + geom_histogram() +
    ylab('Parameters') +theme(panel.grid=element_blank())
}

#' Marginal Effects Plot for Hierarchical Covariates
#' 
#' This function will calculate marginal effects, or the first derivative
#' of the IRT/ideal point model with respect to the hierarchical covariate,
#' separately for the two poles of the latent variable. These two marginal
#' effects permit the interpretation of the effect of the covariate on 
#' with respect to either end of the latent variable.
#' 
#' Because the marginal effects are always with respect to a given
#' outcome/response, the outcome to be predicted must be specified 
#' in \code{pred_outcome}. If it is not specified, the function
#' will prompt you to select one of the outcome's values in the data.
#' 
#' The ends of the latent variable can be specified via the 
#' \code{label_low} and \code{label_high} options, which will use those
#' labels in the ensuing plot.
#' 
#' To exclude parameters from the plot, use the \code{filter_cov} option. 
#' Note that the parameters must be specified using the underlying model 
#' syntax (however they are labeled in the plot). You can also change
#' the names of parameters using the \code{new_cov_names} option.
#' 
#' Note that the function produces a \code{ggplot2} object, which can 
#' be further modified with \code{ggplot2} functions.
#' 
#' @param object A fitted \code{idealstan} object
#' @param calc_varying Whether to marginalize covariate effects over 
#' discrimination parameters to calculate a meaningful quantity for the effect of
#' covariates on the latent scale (see vignette). Defaults to \code{TRUE}
#' @param label_high What label to use on the plot for the high end of the 
#' latent scale
#' @param label_low What label to use on the plot for the low end of the 
#' latent scale
#' @param pred_outcome For discrete models with more than 2 categories, 
#' or binary models with missing data, which outcome to predict. This should 
#' be a character value that matches what the outcome was coded as in the data
#' passed to \code{\link{id_make}}.
#' @param high_quantile The upper limit of the posterior density to use for 
#' calculating credible intervals
#' @param low_quantile The lower limit of the posterior density to use for
#' calculating credible intervals
#' @param new_cov_names A character vector of length equal to the number
#' of covariates (plus 1 for the intercept) to change the default labels.
#' To see the default labels, use the plot function with this option blank.
#' The character vector should be of th form used by 
#' @param cov_type Either \code{'person_cov'} for person or group-level hierarchical parameters,
#' \code{'discrim_reg_cov'} for bill/item discrimination parameters from regular (non-inflated) model, and 
#' \code{'discrim_infl_cov'} for bill/item discrimination parameters from inflated model.
#' @param filter_cov A character vector of coefficients from covariate plots to exclude from
#' plotting (should be the names of coefficients as they appear in the plots)
#' @param recalc_vals A character value of length three that can be used to create
#' a new variable that is a sum of two other variables. The first two values of the
#' character vector are the names of these parameters, while the third value is the name
#' of the new combined variable. Note that if the parameters are renamed, the new names
#' should be used in this option.
#' @return A \code{ggplot2} plot that can be further customized with \code{ggplot2} functions if need be.
#' @import svDialogs
#' @export
id_plot_cov <- function(object,
                        calc_varying=T,
                        label_high="Liberal",
                        label_low="Conservative",
                        cov_type='person_cov',
                        pred_outcome = NULL,
                        high_quantile=0.95,
                        low_quantile=0.05,
                        filter_cov=NULL,
                        new_cov_names=NULL,
                        recalc_vals=NULL) {
  
  # determine which outcome to predict
  
  if(is.null(pred_outcome)) {
    if(object@model_type %in% c(1,2,3,4,5,6,13,14)) {
      # ask user for predicted outcome
      pred_outcome <- svDialogs::dlg_list(levels(object@score_data@score_matrix$outcome),
                                          title="Select which level of the outcome to predict using covariates.")$res
    } else if(object@model_type %in% c(7,8)) {
      pred_outcome <- "Mean Count"
    } else if(object@model_type %in% c(9,10,11,12)) {
      pred_outcome <- "Mean"
    }
  }
  
  # adjust labels to match predicted outcome
  
  if(object@model_type %in% c(1,2,3,4,5,6,13,14)) {
    pred_outcome_high <- paste0("Pr(",pred_outcome,"|",label_high,")")
    pred_outcome_low <- paste0("Pr(",pred_outcome,"|",label_low,")")
    xlabel <- "Marginal Change in Probability"
  } else if(object@model_type %in% c(7,8)) {
    pred_outcome_high <- paste0("Mean Count|",label_high)
    pred_outcome_low <- paste0("Mean Count|",label_low)
    xlabel <- "Marginal Change in Mean Count"
  } else {
    pred_outcome_high <- paste0("Mean|",label_high)
    pred_outcome_low <- paste0("Mean|",label_low)
    xlabel <- "Marginal Change in Mean"
  }
  
  # pull hierarchical covariates
  
  param_name <- switch(cov_type,person_cov='legis_x',
                       discrim_reg_cov='sigma_reg_x',
                       discrim_infl_cov='sigma_abs_x')
  
  to_plot <- as.array(object@stan_samples,
                      pars=param_name)
  
  # reset names of parameters
  new_names <- switch(cov_type,person_cov=object@score_data@person_cov,
                      discrim_reg=object@score_data@item_cov,
                      discrim_abs=object@score_data@item_cov_miss)
  
  # recode these names if user supplies option
  
  if(!is.null(new_cov_names)) {
    new_names <- recode(new_names,!!!new_cov_names)
  }
  
  attributes(to_plot)$dimnames$parameters <- new_names
  
  # remove unwanted coefficients
  
  if(!is.null(filter_cov)) {
    to_plot <- to_plot[,,!(new_names %in% filter_cov),drop=F]
    new_names <- new_names[!(new_names %in% filter_cov)]
    new_cov_names <- new_cov_names[!(new_cov_names %in% filter_cov)]
  }
  
  # set up values to re-calculate
  
  if(!is.null(recalc_vals)) {
    if(length(recalc_vals)!=3) {
      stop("Option recalc_vals can only be a character vector of length 3 indicating which two variables to add together and their name.")
    }
    val1 <- which(attributes(to_plot)$dimnames$parameters==recalc_vals[1])
    val2 <- which(attributes(to_plot)$dimnames$parameters==recalc_vals[2])
    if(is.null(val1) || is.null(val2)) {
      stop("The parameter names you passed to re-calculate did not match existing parameters. Please be sure to use recoded parameter names not original parameter names.")
    }
  }
  
  if(calc_varying) {
    # get all sigmas
    sigma_all <- rstan::extract(object@stan_samples,"sigma_reg_free")
    
    # iterate over posterior draws and calculate effect conditional on pos/neg discrimination
    # for all params in to_plot
    
    neg_eff <- lapply(1:nrow(sigma_all[[1]]), function(i) {
      this_discrim <- sigma_all[[1]][i,]
      
      neg_discrim <- this_discrim[this_discrim<0]
      
      #calculate marginal changes in probability
      to_plot_neg <- apply(to_plot,3,function(c) {
        mean(plogis(c[i]*neg_discrim)-0.5)
      })
      tibble(estimate=to_plot_neg,
             parameter=names(to_plot_neg)) %>% 
        mutate(Type=pred_outcome_low)
    }) %>% bind_rows
    
    pos_eff <- lapply(1:nrow(sigma_all[[1]]), function(i) {
      this_discrim <- sigma_all[[1]][i,]
      pos_discrim <- this_discrim[this_discrim>0]
      
      
      #calculate marginal changes in probability
      to_plot_neg <- apply(to_plot,3,function(c) {
        mean(plogis(c[i]*pos_discrim)-0.5)
      })
      tibble(estimate=to_plot_neg,
             parameter=names(to_plot_neg)) %>% 
        mutate(Type=pred_outcome_high)
    }) %>% bind_rows
    
    if(!is.null(recalc_vals)) {
      # do the same for re-calculated values
      
      neg_eff_recalc <- lapply(1:nrow(sigma_all[[1]]), function(i) {
        this_discrim <- sigma_all[[1]][i,]
        
        neg_discrim <- this_discrim[this_discrim<0]
        
        #calculate marginal changes in probability
        to_plot_neg <- mean(plogis((to_plot[i,,val1]+to_plot[i,,val2])*neg_discrim)-0.5)
        
        tibble(estimate=to_plot_neg,
               parameter=recalc_vals[3]) %>% 
          mutate(Type=pred_outcome_low)
      }) %>% bind_rows
      
      pos_eff_recalc <- lapply(1:nrow(sigma_all[[1]]), function(i) {
        this_discrim <- sigma_all[[1]][i,]
        pos_discrim <- this_discrim[this_discrim>0]
        
        
        #calculate marginal changes in probability
        to_plot_neg <- mean(plogis((to_plot[i,,val1]+to_plot[i,,val2])*pos_discrim)-0.5)
        
        tibble(estimate=to_plot_neg,
               parameter=recalc_vals[3]) %>% 
          mutate(Type=pred_outcome_high)
      }) %>% bind_rows
    }
    
    if(!is.null(recalc_vals)) {
      to_plot <- bind_rows(neg_eff,
                           pos_eff,
                           neg_eff_recalc,
                           pos_eff_recalc)
    } else {
      to_plot <- bind_rows(neg_eff,
                           pos_eff)
    }
    
    
    
    sum_func <- function(this_data,high=high_quantile,
                         low=low_quantile) {
      tibble(y=median(this_data),
             ymin=quantile(this_data,low_quantile),
             ymax=quantile(this_data,high_quantile))
    }
    
    # if new levels exist, reorder
    
    if(!is.null(new_cov_names)) {
      if(is.null(recalc_vals)) {
        to_plot$parameter <- fct_relevel(factor(to_plot$parameter),rev(new_cov_names))
      } else {
        to_plot$parameter <- fct_relevel(factor(to_plot$parameter),c(rev(new_cov_names),recalc_vals[3]))
      }
      
    }
    
    outplot <- to_plot %>% 
      ggplot(aes(x=parameter,y=estimate)) +
      stat_summary(fun.data=sum_func) +
      coord_flip() +
      geom_hline(yintercept=0,linetype=2) +
      facet_wrap(~Type) +
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face="bold"),
            axis.ticks.y=element_blank()) +
      xlab("") + 
      ylab(xlabel)
    
    if(object@model_type %in% c(1,2,3,4,5,6,13,14)) {
      outplot <- outplot + scale_y_continuous(labels=scales::percent)
    }
    
    return(outplot)
    
  } else {
    
    mcmc_intervals(to_plot) + xlab("Ideal Point Score")
  }
  
}

#' Generate Impulse Response Functions for Covariates
#' 
#' This function will generate an impulse response function (IRF)
#' for a given covariate. The IRF shows the marginal impact of a 1-unit
#' change in the covariate on a person's ideal point over time. To use 
#' this function, the \code{vary_ideal_pts} option in 
#' \code{\link{id_estimate}} must have received the \code{'AR1'} option
#' as IRFs are only available for the AR(1) auto-regressive model.
#' 
#' @param object A fitted \code{idealstan} object
#' @param cov_name The name of the covariate to plot. Leave blank to select 
#' from a list of available covariates
#' @param calc_varying if \code{TRUE}, will calculate marginal effects of the
#' covariates on each end of the latent scale (see vignette for more information)
#' @param label_high The character label for the upper end of the latent scale
#' @param label_low The character label for the lower end of the latent scale
#' @param pred_outcome For discrete models with more than 2 categories, 
#' or binary models with missing data, which outcome to predict. This should 
#' be a character value that matches what the outcome was coded as in the data
#' passed to \code{\link{id_make}}.
#' @param high_quantile The upper limit of the posterior density to use for 
#' calculating credible intervals
#' @param low_quantile The lower limit of the posterior density to use for
#' calculating credible intervals
#' @param recalc_vals Whether to combine two variables into one through addition before
#' computing IRFs. If \code{TRUE}, two names of parameters should be passed to 
#' \code{cov_name} or selected from the dialog list
#' @param include A list of character names of person or group IDs for which to
#' calculate IRFs
#' @param time_calc The maximum number of time points over which to calculate the
#' IRF
#' @param time_label Character string specifying the type of time points (default is just
#' \code{"Time Points"})
#' @param line_type The line type of the IRF line (see \code{ggplot2} documentation)
#' @param line_width The line width of the IRF line (see \code{ggplot2} documentation)
#' @param line_alpha The line alpha (transparency) of the IRF line (see \code{ggplot2} documentation)
#' @param line_color The color of the IRF line (see \code{ggplot2} documentation)
#' @param ci_color The color of the IRF credible interval (see \code{ggplot2} documentation)
#' @param ci_alpha The alpha of the IRF credible interval (see \code{ggplot2} documentation)
#' @param use_ci Whether or not to plot a credible interval around the lines
#' @return a \code{ggplot2} object that can be further customized if necessary
#' @export
#' @import scales
id_plot_irf <- function(object,
                        cov_name=NULL,
                        label_high="Liberal",
                        label_low="Conservative",
                        pred_outcome=NULL,
                        recalc_vals=F,
                        include=NULL,
                        time_calc=10,
                        time_label="Time Points",
                        line_type=2,
                        line_width=1,
                        line_alpha=1,
                        line_color="red",
                        ci_color='black',
                        ci_alpha=0.5,
                        use_ci=TRUE,
                        high_quantile=0.95,
                        low_quantile=0.05,
                        calc_varying=T) {
  
  # figure out which covariate to iterate over
  
  # determine which outcome to predict
  
  if(is.null(pred_outcome)) {
    if(object@model_type %in% c(1,2,3,4,5,6,13,14)) {
      # ask user for predicted outcome
      pred_outcome <- svDialogs::dlg_list(levels(object@score_data@score_matrix$outcome),
                                          title="Select which level of the outcome to predict using covariates.")$res
    } else if(object@model_type %in% c(7,8)) {
      pred_outcome <- "Mean Count"
    } else if(object@model_type %in% c(9,10,11,12)) {
      pred_outcome <- "Mean"
    }
  }
  
  # adjust labels to match predicted outcome
  
  if(object@model_type %in% c(1,2,3,4,5,6,13,14)) {
    pred_outcome_high <- paste0("Pr(",pred_outcome,"|",label_high,")")
    pred_outcome_low <- paste0("Pr(",pred_outcome,"|",label_low,")")
    xlabel <- "Marginal Change in Probability"
  } else if(object@model_type %in% c(7,8)) {
    pred_outcome_high <- paste0("Mean Count|",label_high)
    pred_outcome_low <- paste0("Mean Count|",label_low)
    xlabel <- "Marginal Change in Mean Count"
  } else {
    pred_outcome_high <- paste0("Mean|",label_high)
    pred_outcome_low <- paste0("Mean|",label_low)
    xlabel <- "Marginal Change in Mean"
  }
  
  # pull hierarchical covariates
  
  param_name <- "legis_x"
  
  # figure out which prameter to use
  
  # reset names of parameters
  new_names <- object@score_data@person_cov
  
  if(is.null(cov_name)) {
    cov_name <- svDialogs::dlg_list(new_names,multiple=T,
                                    title="Select at least one variable to compute IRFs. If you want to combine two variables, select two variables (but not more than two).")$res
  }
  
  to_plot <- as.array(object@stan_samples,
                      pars=param_name)
  
  attributes(to_plot)$dimnames$parameters <- new_names
  
  to_plot <- to_plot[,,(new_names %in% cov_name),drop=F]
  
  # now need to loop over persons/item IDs to generate IRFs
  
  if(object@use_groups) {
    all_ids <- unique(object@score_data@score_matrix$group_id)
  } else {
    all_ids <- unique(object@score_data@score_matrix$person_id)
  }
  
  ar1 <- rstan::extract(object@stan_samples,"L_AR1")[[1]]
  
  # keep some if user specifies
  if(!is.null(include)) {
    ar1 <- ar1[,all_ids %in% include]
    all_ids <- all_ids[all_ids %in% include]
  }
  
  # calc IRF first then convert to marginal changes if wanted
  
  # get cov values
  
  cov1 <- to_plot[,,1]
  
  if(recalc_vals) {
    cov1 <- cov1 + to_plot[,,2]
  }
  
  # iterate over all the possible AR1 parameters
  
  all_irfs <- apply(ar1,2, function(arp) {
    this_irf <- .irf(total_t=time_calc,
                     adj_in=arp,
                     shock=cov1)  
    return(this_irf)
  })
  
  names(all_irfs) <- all_ids
  
  all_irfs <- bind_rows(all_irfs,.id="Person") 
  
  
  
  if(calc_varying) {
    
    # pull sigmas if we want to calculate marginal changes
    
    sigma_all <- rstan::extract(object@stan_samples,"sigma_reg_free")[[1]]
    
    # iterate over persons and time points
    
    
    all_eff <- lapply(1:nrow(sigma_all), function(i) {
      this_discrim <- sigma_all[i,]
      pos_discrim <- this_discrim[this_discrim>0]
      neg_discrim <- this_discrim[this_discrim<0]
      
      this_data <- filter(all_irfs,iter==i) %>% 
        group_by(Person,time) %>% 
        mutate(marg_neg=mean(plogis(y_shock*neg_discrim)-0.5),
               marg_pos=mean(plogis(y_shock*pos_discrim)-0.5))
    }) %>% bind_rows
    
    outplot <- all_eff %>% gather(key=Type,value=estimate,marg_neg,marg_pos) %>% 
      group_by(Person,time,Type) %>% 
      summarize(mean_est=mean(estimate),
                high_est=quantile(estimate,high_quantile),
                low_est=quantile(estimate,low_quantile)) %>% 
      ungroup %>% 
      mutate(time=factor(time),
             Type=recode(Type,marg_neg=pred_outcome_low,
                         marg_pos=pred_outcome_high)) %>% 
      ggplot(aes(y=mean_est,x=time,group=Person))
    
    if(use_ci) {
      outplot <- outplot + geom_ribbon(aes(ymin=low_est,
                                           ymax=high_est),
                                       alpha=ci_alpha,
                                       fill=ci_color)
    }
    
    outplot + geom_line(linetype=line_type,colour=line_color,size=line_width,
                        alpha=line_alpha) +
      geom_hline(yintercept=0,linetype=3) +
      facet_wrap(~Type) +
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face="bold"),
            axis.ticks.y=element_blank()) +
      scale_y_continuous(labels = scales::percent) +
      xlab(time_label) + 
      ylab(xlabel)
    
    
  } else {
    # don't need as many fancy options if we aren't calculating marginal changes
    
    outplot <- all_irfs %>% group_by(Person,time) %>% 
      summarize(mean_est=mean(y_shock),
                high_est=quantile(y_shock,high_quantile),
                low_est=quantile(y_shock,low_quantile)) %>% 
      ungroup %>% 
      mutate(time=factor(time)) %>% 
      ggplot(aes(y=mean_est,x=time,group=Person))
    
    if(use_ci) {
      outplot <- outplot + geom_ribbon(aes(ymin=low_est,
                                           ymax=high_est),
                                       alpha=ci_alpha,
                                       fill=ci_color)
    }
    
    outplot + geom_line(linetype=line_type,colour=line_color,size=line_width,
                        alpha=line_alpha) +
      geom_hline(yintercept=0,linetype=3) +
      theme(panel.grid=element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(face="bold"),
            axis.ticks.y=element_blank()) +
      xlab(time_label) + 
      ylab("Ideal Point Scale")
  }
  
  
  
  
}
