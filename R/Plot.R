#' Plot Legislator/Person and Bill/Item Ideal Points
#' 
#' This function can be used on a fitted `idealstan` object to plot the relative positions and 
#' uncertainties of legislator/persons and bills/items.
#' 
#' This plot shows the distribution of ideal points for the legislators/persons in the model. It will plot them as a vertical
#' dot plot with associated high-density posterior interval (can be changed 
#' with `high_limit` and `low_limit` options). In addition, if item/bill IDs
#' as a character vector is passed to the `item_plot` option, then an item/bill midpoint will be overlain
#' on the ideal point plot, showing the point at which legislators/persons are indifferent to voting/answering on the 
#' bill/item. Note that because this is an ideal point model, it is not possible to tell from the midpoint itself
#' which side will be voting which way. For that reason, the legislators/persons are colored by their votes/scores to
#' make it clear.
#' 
#' To compare across multiple `idealstan` models, pass a named list 
#' `list(model1=model1,model2=model2,etc)` to the `object` option. 
#' Note that these comparisons will done by individual persons/groups, so if there are a lot of 
#' persons/groups, consider using the `include` option to only compare a specific set
#' of persons/groups.
#' 
#' @param object A fitted `idealstan` object or a named list
#' of `idealstan` objects to compare across models
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param high_limit The quantile (number between 0 and 1) for the high end of posterior uncertainty to show in plot
#' @param low_limit The quantile (number between 0 and 1) for the low end of posterior uncertainty to show in plot
#' @param item_plot The IDs (character vector) of the bill/item midpoints to overlay on the plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param point_size If `person_labels` and `group_labels` are set to `FALSE`, controls the size of the points plotted.
#' @param hjust_length horizontal adjustment of the legislator labels
#' @param person_labels if `TRUE`, use the person_id column to plot labels for the person (legislator) ideal points
#' @param group_labels if `TRUE`, use the group column to plot text markers for the group (parties) from the person/legislator data
#' @param person_ci_alpha The transparency level of the dot plot and confidence bars for the person ideal points
#' @param item_plot_type Whether to show the `'non-inflated'` item/bill midpoints, 
#' the `'inflated'` item/bill midpoints, or produce plots for `'both'` kinds of models. 
#' Defaults to `'non-inflated'` and will only display an item/bill midpoint if one has been 
#' specified in `item_plot`.
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param group_color If `TRUE`, give each group/bloc a different color
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Default is NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param ... Other options passed on to plotting function, currently ignored
#' @import ggplot2
#' @import lazyeval
#' @importFrom rlang parse_quo as_quosure
#' @import ggrepel
#' @import ggthemes
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
#' use_method = "pathfinder",
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
                       hpd_limit=NULL,
                       sample_persons=NULL,...) {
  
  if(!is.null(include)) {
    if(use_groups) {
      include <- which(unique(object@score_data@score_matrix$group_id) %in% include)
    } else {
      include <- include <- which(unique(object@score_data@score_matrix$person_id) %in% include)
    }
    
  }
  
  if(!is.null(sample_persons)) {
    if(!is.numeric(sample_persons) && !(sample_persons>0 && sample_persons<1)) {
      stop('Please enter a fraction to sample from between 0 and 1 as a numeric value.')
    }
    
    if(use_groups) {
      
      to_sample <- sample(unique(object@score_data@score_matrix$group_id),
                          round(sample_persons*length(unique(object@score_data@score_matrix$group_id))))
      include <-  which(object@score_data@score_matrix$group_id %in% to_sample)
      
    } else {
      
      to_sample <- sample(unique(object@score_data@score_matrix$person_id),
                          round(sample_persons*length(unique(object@score_data@score_matrix$person_id))))
      include <-  which(object@score_data@score_matrix$person_id %in% to_sample)
      
    }
    
  }
  
  
  if(inherits(object,'idealstan')) {
    person_params <- .prepare_legis_data(object,
                                         high_limit=high_limit,
                                         low_limit=low_limit,include=include,
                                         aggregated=TRUE)
    model_wrap <- FALSE
    use_groups <- object@use_groups
  } else {
    # loop over lists
    person_params <- lapply(object,.prepare_legis_data,
                            high_limit=high_limit,
                            low_limit=low_limit,
                            include=include,
                            aggregated=TRUE) %>% 
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
  
  # if(!is.null(sample_persons)) {
  #   if(!is.numeric(sample_persons) && !(sample_persons>0 && sample_persons<1)) {
  #     stop('Please enter a fraction to sample from between 0 and 1 as a numeric value.')
  #   }
  #   to_sample <- sample(1:nrow(person_params),round(sample_persons*nrow(person_params)))
  #   person_params <- slice(person_params,to_sample)
  # }
  # 
  
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
                    label=~reorder(outcome_disc,median_pt)),size=text_size_group,
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
    theme_tufte() +
    theme(axis.text.y=element_blank()) + coord_flip() 
  
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
#' This function can be used on a fitted `idealstan` object to plot the over-time variances 
#' (average rates of change in ideal points) for all the persons/legislators in the model.
#' 
#' This function will plot the person/legislator over-time variances as a vertical
#' dot plot with associated high-density posterior interval (can be changed 
#' with `high_limit` and `low_limit` options). 
#' 
#' @param object A fitted `idealstan` object
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param high_limit The quantile (number between 0 and 1) for the high end of posterior uncertainty to show in plot
#' @param low_limit The quantile (number between 0 and 1) for the low end of posterior uncertainty to show in plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param point_size If `person_labels` and `group_labels` are set to `FALSE`, controls the size of the points plotted.
#' @param hjust_length horizontal adjustment of the legislator labels
#' @param person_labels if `TRUE`, use the person_id column to plot labels for the person (legislator) ideal points
#' @param group_labels if `TRUE`, use the group column to plot text markers for the group (parties) from the person/legislator data
#' @param person_ci_alpha The transparency level of the dot plot and confidence bars for the person ideal points
#' @param group_color If `TRUE`, give each group/bloc a different color
#' @param ... Other options passed on to plotting function, currently ignored
#' @import ggplot2
#' @import lazyeval
#' @importFrom rlang parse_quo as_quosure
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
#'  use_method="pathfinder",
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
                                       type='variance',
                                       aggregated=TRUE)
  
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
#' This function can be used on a fitted `idealstan` object to plot the relative positions and 
#' uncertainties of legislator/persons and bills/items when the legislator/person ideal points
#' are allowed to vary over time.
#' 
#' This plot shows the distribution of ideal points for the legislators/persons in the model,
#' and also traces the path of these ideal points over time. It will plot them as a vertical
#' line with associated high-density posterior interval (10\% to 90\%). In addition, if the column index for a 
#' bill/item from the response matrix is passed to the `item_plot` option, then an item/bill midpoint will be overlain
#' on the ideal point plot, showing the point at which legislators/persons are indifferent to voting/answering on the 
#' bill/item. Note that because this is an ideal point model, it is not possible to tell from the midpoint itself
#' which side will be voting which way. For that reason, the legislators/persons are colored by their votes/scores to
#' make it clear.
#' 
#' @param object A fitted `idealstan` object or a named list of `idealstan`
#' objects if the plot is supposed to show a comparison of different fitted `idealstan`
#' models (see Time Series vignette)
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param include Specify a list of person/legislator IDs to include in the plot (all others excluded)
#' @param item_plot The value of the item/bill for which to plot its midpoint (character value)
#' @param item_plot_type Whether to show the `'non-inflated'` item/bill midpoints, 
#' the `'inflated'` item/bill midpoints, or produce plots for `'both'` kinds of models. 
#' Defaults to `'non-inflated'` and will only display an item/bill midpoint if one has been 
#' specified in `item_plot`.
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param high_limit A number between 0 and 1 showing the upper limit to compute the 
#' posterior uncertainty interval (defaults to 0.95).
#' @param low_limit A number between 0 and 1 showing the lower limit to compute the 
#' posterior uncertainty interval (defaults to 0.05).
#' @param line_size Sets the size of the line of the time-varying ideal points.
#' @param group_color If `TRUE`, use the groups instead of individuals to plot colours
#' @param highlight A character referring to one of the persons in `person_labels` that the plot can highlight relative to other persons
#' @param person_ci_alpha The transparency level of ribbon confidence interval around the time-varying ideal points
#' @param person_line_alpha The transparency level of the time-varying ideal point line
#' @param plot_text If `TRUE`, will plot `person_labels` over the lines.
#' @param use_ci Whether or not high-posterior density intervals (credible intervals) should be
#' plotted over the estimates (turn off if the plot is too busy)
#' @param plot_lines The number of lines of actual draws of time-varying ideal points
#' to draw on the plot. Note that these are grouped by persons. Specific draws selected at random
#' from total number of draws of the estimation. Default is 0.
#' @param draw_line_alpha The opacity of lines plotted over the distribution (should be 
#' between 0 and 1, default is 0.5).
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Leave NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param plot_sim Whether to plot the true values of parameters if a simulation was used to generate data 
#' (see [id_sim_gen()])
#' @param add_cov Whether to add values of hierarchical person-level covariates to the
#' time trends (defaults to TRUE).
#' @param use_chain ID of MCMC chain to use rather than combining all chains. 
#' Default is NULL which will use all chains and is recommended.
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
#' use_method = "pathfinder",
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
                              plot_lines=0,
                              draw_line_alpha=0.5,
                              person_line_alpha=0.3,
                              person_ci_alpha=0.8,
                              item_plot_type='non-inflated',show_true=FALSE,group_color=TRUE,
                              hpd_limit=10,
                              sample_persons=NULL,
                              plot_sim=FALSE,
                              use_chain=NULL,
                              add_cov=TRUE,...) {
  
  
  # prepare data
  
  person_labels <- quo(person_id)
  group_labels <- quo(group_id)
  already_facet <- FALSE
  
  if(!is.null(include)) {
    if(object@use_groups) {
      include <- which(unique(object@score_data@score_matrix$group_id) %in% include)
    } else {
      include <- which(unique(object@score_data@score_matrix$person_id) %in% include)
    }
    
    if(length(include)==0)
      stop("You specified persons or groups that were not in the data. Please the labels as you passed them to the id_make function.")
    
  }
  
    if(!is.null(sample_persons)) {
      if(!is.numeric(sample_persons) && !(sample_persons>0 && sample_persons<1)) {
        stop('Please enter a fraction to sample from between 0 and 1 as a numeric value.')
      }
      
      if(object@use_groups) {
        
        to_sample <- sample(unique(object@score_data@score_matrix$group_id),
                            round(sample_persons*length(unique(object@score_data@score_matrix$group_id))))
        include <-  which(object@score_data@score_matrix$group_id %in% to_sample)
        
      } else {
        
        to_sample <- sample(unique(object@score_data@score_matrix$person_id),
                            round(sample_persons*length(unique(object@score_data@score_matrix$person_id))))
        include <-  which(object@score_data@score_matrix$person_id %in% to_sample)
        
      }

    }
  
  if(inherits(object,'idealstan')) {
    person_params <- .prepare_legis_data(object,
                                         high_limit=high_limit,
                                         low_limit=low_limit,
                                         sample_draws=plot_lines,
                                         include=include,
                                         add_cov=add_cov,
                                         use_chain=use_chain,aggregated=TRUE)
    model_wrap <- FALSE
    use_groups <- object@use_groups
  } else {
    # loop over lists
    person_params <- lapply(object,.prepare_legis_data,
                            high_limit=high_limit,
                            low_limit=low_limit,
                            sample_draws=plot_lines,
                            include=include,aggregated=TRUE) %>% 
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
  
  if(plot_lines>0) {
    
    person_lines <- dplyr::filter(ungroup(attr(person_params,"draws")), 
                           .draw %in% sample(unique(.draw),
                                                             plot_lines)) %>% 
      mutate(line_group1=paste0(person_id, .draw),
             line_group2=paste0(group_id, .draw))
    
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
  
  # check for very large numbers
  
  if(any(person_params$high_pt>1e6)) {
    warnings("Very large person ideal points detected. Removing from plot, likely a sign of model misfit.")
    person_params <- filter(person_params, high_pt<1e6,low_pt>(-1e6))
  }
  
  # fix missing labels
  
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
                alpha=person_line_alpha,
                size=line_size)
    
  } else {
    if(group_color) {
      if(model_wrap) {
        outplot <- outplot + 
          geom_line(aes_(y=~median_pt,group=base_id,
                         colour=~Model),
                    alpha=person_line_alpha,
                    size=line_size)
      } else {
        outplot <- outplot + 
          geom_line(aes_(y=~median_pt,group=base_id,
                         colour=~group_id),
                    alpha=person_line_alpha,
                    size=line_size)
      }
      
    } else {
      
      outplot <- outplot + 
        geom_line(aes_(y=~median_pt,group=base_id),
                  alpha=person_line_alpha,
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
      guides(color="none",
             fill="none")
  }
  
  # add individual draws as lines
  
  if(plot_lines>0 && object@use_groups) {
    
    outplot <- outplot + geom_line(data=person_lines, aes(y=ideal_pts,x=time_id,group=line_group2),
                                   alpha=draw_line_alpha)
    
  } else if(plot_lines>0 && !object@use_groups) {
    
    outplot <- outplot + geom_line(data=person_lines, aes(y=ideal_pts,x=time_id,group=line_group1),
                                   alpha=draw_line_alpha)
    
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
#' @param labels `TRUE` or `FALSE`, whether to use labels for points
#' @param hjust The horizontal adjustment of point labels
#' @param palette `colorbrewer` palette name
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
#' This function produces density plots of the different types of parameters in an `idealstan` model: item (bill) difficulty and discrimination
#'  parameters, and person (legislator) ideal points.
#'  
#' @param object A fitted `idealstan` object
#' @param params Select the type of parameter from the model to plot. `'person'` for person/legislator ideal points,
#'  `'miss_diff'` and `'miss_discrim'` for difficulty and discrimination parameters from the missing/inflated item/bill parameters,
#'  and `'obs_diff'` and `'obs_discrim'` for difficulty and discrimination parameters from the non-missing/non-inflated 
#'  item/bill parameters.
#' @param param_labels A vector of labels equal to the number of parameters. Primarily useful if `return_data` is `TRUE`.
#' @param dens_type Can be `'all'` for showing 90% HPD high, 10% HPD low and median posterior values. 
#'  Or to show one of those posterior estimates at a time, use `'high'` for 90% high HPD posterior estimate,
#'  `'low'` for 10% low HPD posterior estimate, and `'function'` for the whatever function is specificied
#'  in `func` (median by default).
#' @param return_data Whether or not to return the plot as a ggplot2 object and the data together in a list instead of
#'  plotting.
#' @param func The function to use if `'dens_type'` is set to `'function'`.
#' @param ... Other options passed on to the plotting function, currently ignored.
#' @export
id_plot_all_hist <- function(object,params='person',param_labels=NULL,dens_type='all',
                          return_data=FALSE,func=median,...) {
  
  stan_params <- switch(params,
                   miss_diff='A_int_free',
                   miss_discrim='sigma_abs_free',
                   obs_diff='B_int_free',
                   obs_discrim='sigma_reg_free',
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
#' This function plots the results from a simulation generated by [id_sim_gen()].
#' 
#' @param sims A fitted `idealstan` object that has true data generated by [id_sim_gen()]
#' @param type Type of analysis of true versus fitted values, can be `'RMSE'`, `'Residuals'` or `'Coverage'`
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
#' This plotting function displays a histogram of the Rhat values of all parameters in an `idealstan` model.
#' 
#' @param obj A fitted `idealstan` object.
#' @importFrom ggthemes theme_tufte
#' @export
id_plot_rhats <- function(obj) {
  # first get all summaries
  obj@summary %>% 
    ggplot(aes(x=rhat)) + theme_tufte() + geom_histogram() +
    ylab('Parameters') +
    xlab("Rhat") +
    labs(caption=stringr::str_wrap("Plot shows a histogram of model parameter Rhat values. Values below 1.1 generally indicate convergence across multiple chains. Values close to 1.0 are preferred.",
                                   width=60)) +
    ggtitle("Distribution of Rhat Values from Fitted Idealstan Model")
}

#' Marginal Effects Plot for Hierarchical Covariates
#' 
#' This function will calculate and plot the ideal point marginal effects, or the first derivative
#' of the IRT/ideal point model with respect to the hierarchical covariate,
#' for each item in the model. The function [id_me()] is used
#' to first calculate the ideal point marginal effects.
#' 
#' The ends of the latent variable can be specified via the 
#' `label_low` and `label_high` options, which will use those
#' labels for item discrimination.
#' 
#' Note that the function produces a `ggplot2` object, which can 
#' be further modified with `ggplot2` functions.
#' 
#' @param object A fitted `idealstan` object
#' @param calc_param Whether to calculate ideal point marginal effects for
#' a given covariate. If NULL, the default, the function will instead produce
#' a plot of the raw coefficients from the ideal point model. If passing the
#' name of a covariate, should be a character value of a column in the data 
#' passed to the
#' `id_make` function.
#' @param group_effects Character value for name of column in data by which to 
#' subset the data. Must be a column passed to the [id_make] function
#' @param label_high What label to use on the plot for the high end of the 
#' latent scale
#' @param label_low What label to use on the plot for the low end of the 
#' latent scale
#' @param pred_outcome For discrete models with more than 2 categories, 
#' or binary models with missing data, which outcome to predict. This should 
#' be the value that matches what the outcome was coded as in the data
#' passed to [id_make()].
#' @param plot_model_id The integer of the model ID to plot. If NULL and there
#' are multiple model types, `facet_wrap` will be used to produce multiple
#' plots with one for each model type.
#' @param upb The upper limit of the posterior density to use for 
#' calculating credible intervals
#' @param lb The lower limit of the posterior density to use for
#' calculating credible intervals
#' @param facet_ncol If facetting by multiple models or grouped factors, sets the 
#' number of columns in the multiple plots
#' @param cov_type Either `'person_cov'` for person or group-level hierarchical parameters,
#' `'discrim_reg_cov'` for bill/item discrimination parameters from regular (non-inflated) model, and 
#' `'discrim_infl_cov'` for bill/item discrimination parameters from inflated model.
#' @return A `ggplot2` plot that can be further customized with `ggplot2` functions if need be.
#' @param ... Additional argument passed on to `id_me`
#' @import svDialogs
#' @importFrom stats as.formula
#' @export
id_plot_cov <- function(object,
                        calc_param=NULL,
                        label_high="High",
                        label_low="Low",
                        group_effects=NULL,
                        plot_model_id=NULL,
                        pred_outcome = NULL,
                        lb=0.05,
                        upb=0.95,
                        facet_ncol=2,
                        cov_type='person_cov',
                        ...) {
  
  # helper function for scale labels
  
  custom_labels <- function(limits,scale_ends) {
    breaks <- seq(from=min(limits),to= max(limits),length.out=5)
    labels <- c(as.character(scale_ends[1]), round(breaks[2:4] ,2),as.character(scale_ends[2]))
    list(breaks = breaks, labels = labels)
  }
  
  scale_ends <- c(label_low,
                  label_high)
  
  # either calculate marginal effects or just plot the raw coefficients
  
  if(!is.null(calc_param)) {
    
    # first calculate the effects
    
    all_effs <- id_me(object,
                      pred_outcome=pred_outcome,
                      lb=lb,upb=upb,
                      covariate=calc_param,
                      group_effects=group_effects,
                      ...)
    
    all_effs$sum_ideal_effects <- mutate(all_effs$sum_ideal_effects,
                                         model_id_label=recode(model_id,
                                                         `1`="Binary Outcome",
                                                         `2`="Binary Outcome",
                                                         `3`="Rating-Scale Ordinal Outcome",
                                                         `4`="Rating-Scale Ordinal Outcome",
                                                         `5`="Graded-Response Ordinal Outcome",
                                                         `6`="Graded-Response Ordinal Outcome",
                                                         `7`="Poisson Outcome",
                                                         `8`="Poisson Outcome",
                                                         `9`="Gaussian Outcome",
                                                         `10`="Gaussian Outcome",
                                                         `11`="Log-Normal Outcome",
                                                         `12`="Log-Normal Outcome",
                                                         `13`="Binary Outcome Latent Space",
                                                         `14`="Binary Outcome Latent Space"))
    
    max_scale <- max(all_effs$sum_ideal_effects$item_discrimination)
    min_scale <- min(all_effs$sum_ideal_effects$item_discrimination)
    
    if(length(unique(all_effs$sum_ideal_effects$model_id))==1 || !is.null(plot_model_id)) {
      
      if(!is.null(plot_model_id)) {
        
        message(paste0("Filtering estimates to model ID ",plot_model_id))
        
        all_effs$sum_ideal_effects <- all_effs$sum_ideal_effects %>% 
          filter(model_id==plot_model_id)
        
      }
      
      # only a single model type to plot
      outplot <- all_effs$sum_ideal_effects %>% 
                ggplot(aes(y=mean_est,
                           x=reorder(item_id,mean_est))) +
                  geom_linerange(aes(ymin=low_est,
                                     ymax=high_est,
                                     colour=item_discrimination)) +
                  ggthemes::theme_tufte() + 
                  scale_colour_viridis_c(name="Item\nDiscrimination",
                                         limits = c(min_scale,
                                                    max_scale),
                                         breaks = custom_labels(c(min_scale,
                                                                  max_scale),
                                                                scale_ends)$breaks,
                                         labels = custom_labels(c(min_scale, 
                                                                  max_scale),
                                                                scale_ends)$labels) +
                  coord_flip() +
                  labs(y=paste0("Marginal Change in Probability of ",unique(all_effs$sum_ideal_effects$model_id_label)),
                       x="Items",
                       caption=paste0("Marginal effect of ",calc_param," on ", unique(all_effs$sum_ideal_effects$model_id_label)), " for a specific item.") +
                  geom_hline(yintercept=0,linetype=2) +
                  ggthemes::theme_clean() +
                  theme(axis.text.y=element_blank(),
                        axis.ticks.y=element_blank()) +
                  theme(legend.position = "right")
      
      if(!is.null(group_effects)) outplot <- outplot + facet_wrap(as.formula(paste0("~ ", group_effects)),
                                                                  scales="free_x",ncol = facet_ncol)
      
    } else {
      
      if(!is.null(group_effects)) message("Can't subset by groups due to multiple models. Please pass a value for plot_model_id to plot a specific model type and facet the plot by group.")
      
      # multiple model types to plot
      
      outplot <- all_effs$sum_ideal_effects %>% 
        ggplot(aes(y=mean_est,
                   x=reorder(item_id,mean_est))) +
        geom_linerange(aes(ymin=low_est,
                           ymax=high_est,
                           colour=item_discrimination)) +
        ggthemes::theme_tufte() + 
        scale_colour_viridis_c(name="Item\nDiscrimination",
                               limits = c(min_scale,
                                                                     max_scale),
                               breaks = custom_labels(c(min_scale,
                                                        max_scale),
                                                      scale_ends)$breaks,
                               labels = custom_labels(c(min_scale, 
                                                        max_scale),
                                                      scale_ends)$labels) +
        coord_flip() +
        facet_wrap(~model_id_label,scales="free_x",ncol = facet_ncol) +
        labs(y=paste0("Marginal Change in Probability of ",unique(all_effs$sum_ideal_effects$model_id_label)),
             x="Items",
             caption=paste0("Marginal effect of ",calc_param," on a ", tolower(unique(all_effs$sum_ideal_effects$model_id_label))), "\nfor each item/indicator in the model.") +
        geom_hline(yintercept=0,linetype=2) +
        ggthemes::theme_clean() +
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        theme(legend.position = "right")
      
      
      
    }
    
    # if(unique(object@score_data@score_matrix$model_id)>1) {
    #   
    #   outplot <- outplot + facet_wrap(~Type + model_id,scales="free_x")
    #   
    # } else {
    #   if(unique(object@score_data@score_matrix$model_id) %in% c(1,2,3,4,5,6,7,8,13,14)) {
    #     outplot <- outplot + scale_y_continuous(labels=scales::percent)
    #   }
    #   outplot <- outplot + facet_wrap(~Type)
    # }
    # 
    
  } else {
    
    param_name <- switch(cov_type,person_cov='legis_x',
                         discrim_reg_cov='sigma_reg_x',
                         discrim_infl_cov='sigma_abs_x')
    
    to_plot <- object@stan_samples$draws(variables = param_name)
    
    attr(to_plot,"dimnames")$variable <- switch(cov_type,
                                                person_cov=object@score_data@person_cov,
                                                discrim_reg_cov=object@score_data@item_cov,
                                                discrim_infl_cov=object@score_data@item_cov_miss)
    
    outplot <- mcmc_intervals(to_plot) + xlab("Ideal Point Score")
    
  }
  
  return(outplot)
  
}

#' NB: Deprecated Generate Impulse Response Functions for Covariates
#' 
#' This function will generate an impulse response function (IRF)
#' for a given covariate. The IRF shows the marginal impact of a 1-unit
#' change in the covariate on a person's ideal point over time. To use 
#' this function, the `vary_ideal_pts` option in 
#' [id_estimate()] must have received the `'AR1'` option
#' as IRFs are only available for the AR(1) auto-regressive model.
#' 
#' @param object A fitted `idealstan` object
#' @param cov_name The name of the covariate to plot. Leave blank to select 
#' from a list of available covariates
#' @param calc_varying if `TRUE`, will calculate marginal effects of the
#' covariates on each end of the latent scale (see vignette for more information)
#' @param label_high The character label for the upper end of the latent scale
#' @param label_low The character label for the lower end of the latent scale
#' @param pred_outcome For discrete models with more than 2 categories, 
#' or binary models with missing data, which outcome to predict. This should 
#' be a character value that matches what the outcome was coded as in the data
#' passed to [id_make()].
#' @param high_quantile The upper limit of the posterior density to use for 
#' calculating credible intervals
#' @param low_quantile The lower limit of the posterior density to use for
#' calculating credible intervals
#' @param recalc_vals Whether to combine two variables into one through addition before
#' computing IRFs. If `TRUE`, two names of parameters should be passed to 
#' `cov_name` or selected from the dialog list
#' @param include A list of character names of person or group IDs for which to
#' calculate IRFs
#' @param time_calc The maximum number of time points over which to calculate the
#' IRF
#' @param time_label Character string specifying the type of time points (default is just
#' `"Time Points"`)
#' @param line_type The line type of the IRF line (see `ggplot2` documentation)
#' @param line_width The line width of the IRF line (see `ggplot2` documentation)
#' @param line_alpha The line alpha (transparency) of the IRF line (see `ggplot2` documentation)
#' @param line_color The color of the IRF line (see `ggplot2` documentation)
#' @param ci_color The color of the IRF credible interval (see `ggplot2` documentation)
#' @param ci_alpha The alpha of the IRF credible interval (see `ggplot2` documentation)
#' @param use_ci Whether or not to plot a credible interval around the lines
#' @return a `ggplot2` object that can be further customized if necessary
#' @noRd
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
    if(m %in% c(1,2,3,4,5,6,13,14)) {
      # ask user for predicted outcome
      pred_outcome <- svDialogs::dlg_list(levels(object@score_data@score_matrix$outcome),
                                          title="Select which level of the outcome to predict using covariates.")$res
    } else if(m %in% c(7,8)) {
      pred_outcome <- "Mean Count"
    } else if(m %in% c(9,10,11,12)) {
      pred_outcome <- "Mean"
    }
  }
  
  # adjust labels to match predicted outcome
  
  if(m %in% c(1,2,3,4,5,6,13,14)) {
    pred_outcome_high <- paste0("Pr(",pred_outcome,"|",label_high,")")
    pred_outcome_low <- paste0("Pr(",pred_outcome,"|",label_low,")")
    xlabel <- "Marginal Change in Probability"
  } else if(m %in% c(7,8)) {
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
  
  to_plot <- object@stan_samples$draws(variables=param_name)
  
  attributes(to_plot)$dimnames$variable <- new_names
  
  to_plot <- to_plot[,,(new_names %in% cov_name),drop=F]
  
  # now need to loop over persons/item IDs to generate IRFs
  
  if(object@use_groups) {
    all_ids <- unique(object@score_data@score_matrix$group_id)
  } else {
    all_ids <- unique(object@score_data@score_matrix$person_id)
  }
  
  ar1 <- object@stan_samples$draws(variables="L_AR1") %>% as_draws_matrix
  
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
    
    sigma_all <- object@stan_samples$draws(variables="sigma_reg_free") %>% 
                    as_draws_matrix
    
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

#' Launch the Generalized Beta Distribution Explorer Shiny App
#'
#' This function starts an interactive Shiny application to visualize a generalized Beta distribution
#' on a symmetrical interval [–scale, +scale] in order to calculate the distribution paramters α (the `restrict_sd_high`/`restrict_N_low` parameter to `id_estimate`) and β (the `restrict_N_high`/`restrict_sd_low` parameter to `id_estimate`). This function is useful for understanding what values to use to pin item discrimination parameters in `id_estimate` given a specific prior mean `initial_y` and a relative level of precision `prior_sample_size` (also known as `phi`). Higher values of `prior_sample_size` will imply a tighter prior around the pinned discrimination parameter.
#'
#' The function is also useful for calculating the prior for all non-constrained discrimination parameters as well (`discrim_reg_shape` and `discrim_reg_scale`). These parameters are denoted in the Shiny app output for easy cut and paste to the `id_estimate` function call.
#' @param initial_y             Initial observed variate `y` (default: 0)
#' @param prior_sample_size     Relative level of precision/tightness around `y` (default: 200). Can be thought of as the relative sample size used to estimate `y`.
#' @param limits                Limits of prior. Default is `[-1,1]`. Set to `[0,1]` for a conventional IRT with all positive discrimination parameters (i.e. as in a test-taking scenario).
#' @export
id_plot_gbeta_prior <- function(
    initial_y = 0,
    prior_sample_size = 200,
    limits = c(-1,1)
) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Explore Prior Distributions for Discrimination Parameters (Generalized Beta)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(
          inputId = "limits", label = "Limits of Prior\n(set to 0 and 1 for standard IRT/all positive constraint):",
          min = -1, max = 1, value = limits, step = 0.1
        ),
        shiny::sliderInput(
          inputId = "y", label = "Value to fix prior at:",
          min = -1, max = 1,
          value = initial_y, step = 0.001
        ),
        shiny::numericInput(
          inputId = "y_numeric", label = "Manual input prior value:",
          value = initial_y
        ),
        shiny::sliderInput(
          inputId = "prior_sample_size", label = "Prior sample size (set to 2 for uninformative and 3 for weakly informative):",
          min = 1, max = 10000,
          value = prior_sample_size, step = 1
        ),
        shiny::numericInput(
          inputId = "prior_sample_size_numeric", label = "Manual input prior sample size:",
          value = prior_sample_size, min = 1
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput("distPlot"),
        shiny::verbatimTextOutput("shapeVals")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Sync 'y' limits with 'limits'
    shiny::observeEvent(input$limits, {
      shiny::updateSliderInput(
        session, "y",
        min = input$limits[1], max = input$limits[2],
        value = pmin(pmax(input$y, input$limits[1]), input$limits[2])
      )
    })
    
    output$shapeVals <- shiny::renderPrint({
      a     <- input$limits[1]
      b     <-  input$limits[2]
      x     <- (input$y - a) / (b - a)
      alpha <- input$prior_sample_size * x
      beta  <- input$prior_sample_size * (1 - x)
      
      if(input$y != a && input$y != b) {
        
        cat(sprintf("To fix a discrimination parameter to this value/shape, use these function arguments:\n\n Constrain high:\n.  restrict_N_high = %.2f\n.  restrict_sd_high  = %.2f\n. Constrain low:\n.   restrict_sd_low = %.2f\n.   restrict_N_low  = %.2f\n Default discrimination parameters:\n.  discrim_reg_scale = %.2f\n.  discrim_miss_scale = %.2f\n.   discrim_reg_shape  = %.2f\n.  discrim_miss_shape  = %.2f", alpha, beta, alpha, beta, alpha, alpha, beta, beta))
        
      } else {
        
        
        cat("WARNING: Prior is undefined because the prior mean is equal to the prior scale limit (high or low).")
        
      }
      
      
    })
    
    # Sync slider and numeric inputs for prior_sample_size
    shiny::observeEvent(input$prior_sample_size_numeric, {
      if(!is.na(input$prior_sample_size_numeric)) shiny::updateSliderInput(
        session, "prior_sample_size",
        value = input$prior_sample_size_numeric
      )
    })
    
    # Sync slider and numeric inputs for y
    shiny::observeEvent(input$y_numeric, {
      if(!is.na(input$y_numeric)) shiny::updateSliderInput(
        session, "y",
        value = input$y_numeric
      )
    })
    
    output$distPlot <- shiny::renderPlot({
      a     <- input$limits[1]
      b     <-  input$limits[2]
      xGrid <- seq(a, b, length.out = 400)
      x0    <- (xGrid - a) / (b - a)
      alpha <- input$prior_sample_size * ((input$y - a) / (b - a))
      beta  <- input$prior_sample_size * (1 - ((input$y - a) / (b - a)))
      dens  <- stats::dbeta(x0, shape1 = alpha, shape2 = beta) / (b - a)
      graphics::plot(
        xGrid, dens, type = "l", lwd = 2,
        xlab = "Expected value (average) of prior on discrimination", ylab = "Density",
        main = sprintf(
          "Generalized Beta on [%.2f, %.2f]\nPrecision φ=%d",
          a, b, input$prior_sample_size
        )
      )
    })
  }
  
  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}


