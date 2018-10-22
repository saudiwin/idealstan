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
#' @param object A fitted \code{idealstan} object
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
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
#' @param group_overlap Whether to prevent the text from overlapping itself (ggplot2 option)
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Leave NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param ... Other options passed on to plotting function, currently ignored
#' @import ggplot2
#' @import lazyeval
#' @importFrom rlang parse_quosure as_quosure
#' @export
#' @examples 
#' 
#' # To demonstrate, we load a fitted idealstan object based on the 114th Senate
#' 
#' data('senate114_fit')
#' 
#' id_plot_legis(senate114_fit)
#' 
#' # We can overlap the bill/item midpoints to show 
#' # where the persons/legislators are indifferent to responding positively
#' 
#' id_plot_legis(senate114_fit,item_plot=5)
#' 
id_plot_legis <- function(object,return_data=FALSE,
                          high_limit=.95,
                          low_limit=.05,
                          item_plot=NULL,
                          item_plot_type='non-inflated',
                       text_size_label=2,text_size_group=2.5,
                       point_size=1,
                       hjust_length=-0.7,
                       person_labels=TRUE,
                       group_labels=F,
                       person_ci_alpha=0.1,
                       show_true=FALSE,group_color=TRUE,
                       hpd_limit=10,
                       group_overlap=FALSE,
                       sample_persons=NULL,...) {

  person_params <- .prepare_legis_data(object,
                                       high_limit=high_limit,
                                       low_limit=low_limit)

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
    if(object@model_type %in% c(1,2) || (model_type>6 && model_type<13)) {
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

  if(group_color==TRUE) {
    outplot <- person_params %>% ggplot() +
      geom_linerange(aes(x=reorder(person_id,median_pt),
                         ymin=low_pt,ymax=high_pt,color=group_id),
                     alpha=person_ci_alpha,
                     show.legend = FALSE) +
      geom_text(aes(x=reorder(person_id,median_pt),
                    y=median_pt,label=reorder(group_id,median_pt),
                    color=group_id),size=text_size_group,
                check_overlap = group_overlap,show.legend = FALSE) 
  } else if(group_color==FALSE) {
    outplot <- person_params %>% ggplot() +
      geom_linerange(aes(x=reorder(person_id,median_pt),ymin=low_pt,ymax=high_pt),alpha=person_ci_alpha) +
      geom_text(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(group_id,median_pt)),size=text_size_group,check_overlap = group_overlap)
      
  }
    
    # determine if legislator names should be plotted

  if(person_labels==TRUE & group_color==TRUE) {
    outplot <- outplot + geom_text(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt),color=group_id),
                                       check_overlap=TRUE,hjust=hjust_length,size=text_size_label,show.legend = FALSE)
  } else if(person_labels==TRUE & group_color==FALSE) {
    outplot <- outplot + geom_text(aes(x=reorder(person_id,median_pt),y=median_pt,label=reorder(person_id,median_pt)),
                                   check_overlap=TRUE,hjust=hjust_length,size=text_size_label)
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
  
  outplot <- outplot  + theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip() 

  
  if(return_data==TRUE) {
    
    return_list <- list(outplot=outplot,plot_data=person_params)
    if(!is.null(item_plot)) {
      return_list$bill_data <- bill_pos
    }
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
#' @param object A fitted \code{idealstan} object
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param item_plot The column index of the bill/item midpoint to overlay on the plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_group ggplot2 text size for group text used for points
#' @param line_size Sets the size of the line of the time-varying ideal points.
#' @param person_plot If \code{TRUE}, plots the individual person/legislator ideal points.
#' If \code{FALSE}, plots the group-level estimates. Group-level estimates only exist if
#' \code{use_groups=TRUE} was set in the \code{\link{id_estimate}} function.
#' @param person_labels character string for the name of the column in the person/legislator data frame 
#' with the names of the persons/legislators. Defaults to \code{"person.name"}
#' @param group_color If \code{TRUE}, use the groups instead of individuals to plot colours
#' @param group_labels the name of the column to plot text markers for the group (parties) from the person/legislator data (default=\code{'group'}).
#' @param highlight A character referring to one of the persons in \code{person_labels} that the plot can highlight relative to other persons
#' @param person_ci_alpha The transparency level of ribbon confidence interval around the time-varying ideal points
#' @param person_line_alpha The transparency level of the time-varying ideal point line
#' @param plot_text If \code{TRUE}, will plot \code{person_labels} over the lines.
#' @param use_ci Whether or not high-posterior density intervals (credible intervals) should be
#' plotted over the estimates (turn off if the plot is too busy)
#' @param show_score Show only person/legislator ideal points that have a certain score/vote category from the outcome (character string)
#' @param item_plot_type Whether to show 'both' absence and regular item/bill midpoints if the model is absence-inflated, the default,
#' or 'Absence Points' for only the absence midpoints or 'Vote Points' for only the non-inflated midpoints
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param group_overlap Whether to prevent the text from overlapping itself (ggplot2 option)
#' @param hpd_limit The greatest absolute difference in high-posterior density interval shown for any point. Useful for excluding imprecisely estimated persons/legislators from the plot. Leave NULL if you don't want to exclude any.
#' @param sample_persons If you don't want to use the full number of persons/legislators from the model, enter a proportion (between 0 and 1) to select
#'  only a fraction of the persons/legislators.
#' @param plot_sim Whether to plot the true values of parameters if a simulation was used to generate data 
#' (see \code{\link{id_sim_gen}})
#' @param ... Other options passed on to plotting function, currently ignored
#' @importFrom gghighlight gghighlight
#' @export
#' @examples 
id_plot_legis_dyn <- function(object,return_data=FALSE,item_plot=NULL,
                              text_size_label=2,text_size_group=2.5,
                              line_size=1,
                              person_plot=T,
                              person_labels=NULL,
                              group_labels=NULL,
                              highlight=NULL,
                              plot_text=TRUE,
                              use_ci=TRUE,
                              person_line_alpha=0.3,
                              person_ci_alpha=0.8,
                              show_score=NULL,
                              item_plot_type='both',show_true=FALSE,group_color=TRUE,
                              hpd_limit=10,
                              group_overlap=FALSE,
                              sample_persons=NULL,
                              plot_sim=FALSE,...) {
  
  # prepare data
  
  person_labels <- quo(person_id)
  group_labels <- quo(group_id)
  person_params <- .prepare_legis_data(object) 
  
  if(object@use_groups) {
    base_id <- ~group_id
  } else {
    base_id <- ~person_id
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
             person_id=fct_relevel(person_id,object@score_data@restrict_ind_low,
                                   object@score_data@restrict_ind_high,
                                   
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
      
      outplot <- outplot + 
        geom_line(aes_(y=~median_pt,group=base_id,
                       colour=~group_id),
                  #alpha=person_ci_alpha,
                  size=line_size)
    } else {
      
      outplot <- outplot + 
        geom_line(aes_(y=~median_pt,colour=base_id),
                  #alpha=person_ci_alpha,
                  size=line_size)
    }
  }

  
  # plot random labels
  
  if(plot_text==TRUE) {
    
    # need new data that scatters names around the plot
    
    sampled_data <- group_by(person_params,!!as_quosure(base_id)) %>% sample_n(1)
    
    if(!is.null(highlight)) {
      
      sampled_data <- filter(sampled_data,!(!!as_quosure(base_id) %in% highlight))
    }
    
    outplot <- outplot + 
      geom_text(aes_(x=~time_id,y=~median_pt,label=base_id),data=sampled_data,
                check_overlap=TRUE,size=text_size_label)
    
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
    geom_point() + geom_text(aes(label=reorder(person_id,median_pt)),size=text_size_label,
                             check_overlap=TRUE,hjust=hjust) +
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

#' Display Coefficient Plot of Hierarchical Covariates
#' 
#' This function will pull the estimates of the hierarchical covariates (whether at the person or 
#' item-discrimination level) and then plot them on a vertical coefficient plot. Names of 
#' the parameters are taken from the levels of the factor if a categorical variable and the
#' column names otherwise. 
#' 
#' @param object A fitted \code{idealstan} object
#' @param cov_type Either 'person_cov' for person-level hierarchical parameters,
#' 'discrim_reg_cov' for bill/item discrimination parameters from regular (non-inflated) model, and 
#' 'discrim_infl_cov' for bill/item discrimination parameters from inflated model.
#' @return A \code{ggplot2} plot that can be further customized with \code{ggplot2} functions if need be.
#' @export
id_plot_cov <- function(object,
                        cov_type) {
  
  param_name <- switch(cov_type,person_cov='legis_x',
                       discrim_reg_cov='sigma_reg_x',
                       discrim_infl_cov='sigma_abs_x')
  
  to_plot <- as.array(object@stan_samples,
                   pars=param_name)
  
  # reset names of parameters
  new_names <- switch(cov_type,person_cov=attributes(object@score_data@person_cov)$dimnames$colnames,
                      discrim_reg=attributes(object@score_data@item_cov)$dimnames$colnames,
                      discrim_abs=attributes(object@score_data@item_cov_miss)$dimnames$colnames)

  attributes(to_plot)$dimnames$parameters <- new_names
  
  mcmc_intervals(to_plot) + xlab('Ideal Point Score')
}