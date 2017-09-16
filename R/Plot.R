#' Plot Legislator/Person and Bill/Item Ideal Points
#' 
#' This function can be used on a fitted \code{\link{idealstan}} object to plot the relative positions and 
#' uncertainties of legislator/persons and bills/items.
#' 
#' This plot shows the distribution of ideal points for the legislators/persons in the model. It will plot them as a vertical
#' dot plot with associated high-density posterior interval (10\% to 90\%). In addition, if the column index for a 
#' bill/item from the response matrix is passed to the \code{bill_plot} option, then a bill midpoint will be overlain
#' on the ideal point plot, showing the point at which legislators/persons are indifferent to voting/answering on the 
#' bill/item. Note that because this is an ideal point model, it is not possible to tell from the midpoint itself
#' which side will be voting which way. For that reason, the legislators/persons are colored by their votes/scores to
#' make it clear.
#' 
#' @param return_data If true, the calculated legislator/bill data is returned along with the plot in a list
#' @param bill_plot The column index of the bill/item midpoint to overlay on the plot
#' @param text_size_label ggplot2 text size for legislator labels
#' @param text_size_party ggplot2 text size for party text used for points
#' @param hjust_length horizontal adjustement of the legislator labels
#' @param legis_labels if \code{TRUE}, use the legis.names column to plot legislator labels
#' @param legis_ci_alpha The transparency level of the dot plot and confidence bars
#' @param abs_and_reg Whether to show 'both' absence and regular bill midpoints if the model is absence-inflated, the default,
#' or 'Absence Points' for only the absence midpoints or 'Vote Points' for only the non-inflated midpoints
#' @param show_true Whether to show the true values of the legislators (if model has been simulated)
#' @param party_color If \code{TRUE}, give each party/bloc a different color
#' @param party_overlap Whether to prevent the text from overlapping itself (ggplot2 option)
#' 
#' @import ggplot2
#' @import lazyeval
#' @export

id_plot_legis <- function(object,return_data=FALSE,bill_plot=NULL,
                       text_size_label=2,text_size_party=2.5,hjust_length=-0.7,legis_labels=TRUE,
                       legis_ci_alpha=0.1,abs_and_reg='both',show_true=FALSE,party_color=TRUE,
                       party_overlap=FALSE,...) {

  legis_data <- object@vote_data@legis_data
  
  # Apply any filters from the data processing stage so that the labels match
  
  if(length(object@vote_data@subset_legis)>0) {
    legis_data <- filter(legis_data,legis.names %in% object@vote_data@subset_legis)
  } else if(length(object@vote_data@subset_party)>0) {
    legis_data <- filter(legis_data,party %in% object@vote_data@subset_legis)
  }
  
  if(length(object@vote_data@to_sample)>0) {
    legis_data <- slice(legis_data,object@vote_data@to_sample)
  }
  
  # Reorder rows to match those rows that were switched for identification purposes

  legis_data <- slice(legis_data,as.numeric(row.names(object@vote_data@vote_matrix)))
  
  person_params <- rstan::extract(object@stan_samples,pars='L_full')[[1]] %>% 
    as_data_frame 
  names(person_params) <- as.character(1:length(person_params))
  person_params <- person_params %>% gather(key = legis,value=ideal_pts) %>% 
    mutate(legis=as.numeric(legis)) %>% 
    group_by(legis) %>% 
    summarize(low_pt=quantile(ideal_pts,0.1),high_pt=quantile(ideal_pts,0.9),
              median_pt=median(ideal_pts))

  person_params <- mutate(person_params,legis.names=legis_data$legis.names,
                          party=legis_data$party)
  
  # Rescale simulated values to ensure that they match estimated values in terms of scale multiplicativity

  if(show_true==TRUE) {
    person_params <- mutate(person_params,true_vals=scale(legis_data$true_legis)[,1],
                            median_pt=scale(median_pt)[,1],
                            low_pt=scale(low_pt)[,1],
                            high_pt=scale(high_pt)[,1])
  }
  
  # Default plot: party names plotted as points

  if(party_color==TRUE) {
    outplot <- person_params %>% ggplot() +
      geom_text(aes(x=reorder(legis.names,median_pt),
                    y=median_pt,label=reorder(party,median_pt),
                    color=party),size=text_size_party,
                check_overlap = party_overlap,show.legend = FALSE) +
      geom_linerange(aes(x=reorder(legis.names,median_pt),
                         ymin=low_pt,ymax=high_pt,color=party),
                     alpha=legis_ci_alpha,
                     show.legend = FALSE)
  } else if(party_color==FALSE) {
    outplot <- person_params %>% ggplot() +
      geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(party,median_pt)),size=text_size_party,check_overlap = party_overlap) +
      geom_linerange(aes(x=reorder(legis.names,median_pt),ymin=low_pt,ymax=high_pt),alpha=legis_ci_alpha)
  }
    
    # determine if legislator names should be plotted

  if(legis_labels==TRUE & party_color==TRUE) {
    outplot <- outplot + geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(legis.names,median_pt),color=party),
                                       check_overlap=TRUE,hjust=hjust_length,size=text_size_label,show.legend = FALSE)
  } else if(legis_labels==TRUE & party_color==FALSE) {
    outplot <- outplot + geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(legis.names,median_pt)),
                                   check_overlap=TRUE,hjust=hjust_length,size=text_size_label)
  }
  
  # Add a dirty trick to enable the legend to show what party labels instead of just the letter 'a'
  
  if(party_color==TRUE) {
    
    outplot <- outplot + geom_point(aes(x=reorder(legis.names,median_pt),y=median_pt,color=party),size=0,stroke=0) +
      guides(colour = guide_legend(title="",override.aes = list(size = 5)))
  }
  #, shape = sapply(levels(person_params$party),utf8ToInt)
     
    # Add theme elements
    
  outplot <- outplot  + theme_minimal() + ylab("") + xlab("") +
      theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip() 
  

  if(!is.null(bill_plot)) {

    bill_num <- which(colnames(object@vote_data@vote_matrix) %in% bill_plot)
    bill_discrim_reg <- paste0('sigma_reg_full[',bill_num,']')
    bill_diff_reg <- paste0('B_int_full[',bill_num,']')
    
    if(any(object@model_type %in% c(2,4,6))) {
      bill_discrim_abs <- paste0('sigma_abs_full[',bill_num,']')
      bill_diff_abs <- paste0('A_int_full[',bill_num,']')
      
      to_rstan <- c(bill_discrim_reg,bill_diff_reg,bill_discrim_abs,bill_diff_abs)
    } else {
      to_rstan <- c(bill_discrim_reg,bill_diff_reg)
    }
    
    if(any(object@model_type %in% c(3,4,5,6))) {
      steps <- rstan::extract(object@stan_samples,pars='steps_votes') 
    }

    bill_pos <- rstan::extract(object@stan_samples,pars=to_rstan) %>% as_data_frame %>% 
      gather(key = bills,value=ideal_pts) %>% 
      group_by(bills) %>% 
      mutate(rownum=1:n())
    bill_pos <- spread(bill_pos,bills,ideal_pts) %>% 
      select(-rownum)
    names(bill_pos) <- stringr::str_replace_all(names(bill_pos),'[\\[\\]]',replacement='_')
    bill_diff_reg <- stringr::str_replace_all(bill_diff_reg,'[\\[\\]]',replacement='_')
    bill_discrim_reg <- stringr::str_replace_all(bill_discrim_reg,'[\\[\\]]',replacement='_')
    bill_diff_abs <- stringr::str_replace_all(bill_diff_abs,'[\\[\\]]',replacement='_')
    bill_discrim_abs <- stringr::str_replace_all(bill_discrim_abs,'[\\[\\]]',replacement='_')
    bill_pos <- lapply(1:length(bill_num),function(x) {

      if(any(object@model_type %in% c(3,4,5,6))) {
        # ordinal models
        if(any(object@model_type %in% c(4,6))) {
          #inflated ordinal
          out_reg <- .calc_bill(bill_pos,
                                int_reg=rlang::parse_quosure(bill_diff_reg[x]),
                                sigma_reg=rlang::parse_quosure(bill_discrim_reg[x]),
                                int_abs=rlang::parse_quosure(bill_diff_abs[x]),
                                sigma_abs=rlang::parse_quosure(bill_discrim_abs[x]),
                                steps_data=steps$steps_votes,
                                step_num=ncol(steps$steps_votes),
                                this_num=bill_num[x])
        } else {
          #non-inflated Ordinal
          out_reg <- .calc_bill(bill_pos,
                                int_reg=rlang::parse_quosure(bill_diff_reg[x]),
                                sigma_reg=rlang::parse_quosure(bill_discrim_reg[x]),
                                steps_data=steps$steps_votes,
                                step_num=ncol(steps$steps_votes),
                                this_num=bill_num[x])
        }
      } else {
        #binary models
        #inflated binary
        if(object@model_type==2) {
          out_reg <- .calc_bill(bill_pos,
                                int_reg=rlang::parse_quosure(bill_diff_reg[x]),
                                sigma_reg=rlang::parse_quosure(bill_discrim_reg[x]),
                                int_abs=rlang::parse_quosure(bill_diff_abs[x]),
                                sigma_abs=rlang::parse_quosure(bill_discrim_abs[x]),
                                this_num=bill_num[x])
        } else {
          #non-inflated binary
          out_reg <- .calc_bill(bill_pos,
                                int_reg=rlang::parse_quosure(bill_diff_reg[x]),
                                sigma_reg=rlang::parse_quosure(bill_discrim_reg[x]),
                                this_num=bill_num[x])
        }
      }
      return(out_reg)
    })

    bill_pos <- bind_rows(bill_pos) %>% 
                gather(key = ci_type,value=ci_value,high_bill,low_bill)
    
    #Redo the legislator plot to make room for bill covariates
    
    # Pick up bills and put the labels back on
    cols <- object@vote_data@vote_matrix[,bill_plot] %>% as_data_frame 
    cols <- lapply(cols,function(x) {
      x <- factor(x,levels=object@vote_data@vote_int,labels=object@vote_data@vote_labels)
    }) %>% as_data_frame

    if(length(bill_plot)>1) {
      person_params <- bind_cols(person_params,cols) %>% gather(bill_type,bill_vote,one_of(bill_plot))
    } else {
      person_params <- bind_cols(person_params,cols) 
      names(person_params) <- c(names(person_params)[-length(names(person_params))],'bill_vote')
      person_params <- mutate(person_params,bill_type=bill_plot)
    }

    if(show_true==TRUE) {
      person_params <- mutate(person_params,true_vals=scale(legis_data$true_legis)[,1],
                              median_pt=scale(median_pt)[,1],
                              low_pt=scale(low_pt)[,1],
                              high_pt=scale(high_pt)[,1])
    }
    person_params <- left_join(person_params,bill_pos,c('bill_type'='bill_num')) %>% 
      mutate(ci_type=factor(ci_type,levels=c('low_bill','high_bill'),
                            labels=c('10%','90%')))
    
    # Choose a plot based on the user's options

    if(any(object@model_type %in% c(2,4,6)) & abs_and_reg!='both') {

      person_params <- filter(person_params,param==abs_and_reg)
    } 
    
    outplot <- person_params %>% ggplot() + 
      geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(party,median_pt),color=bill_vote),size=text_size_party,
                check_overlap = party_overlap) + 
      geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(legis.names,median_pt),color=bill_vote),
                check_overlap=TRUE,hjust=hjust_length,size=text_size_label) +
      geom_linerange(aes(x=reorder(legis.names,median_pt),ymin=low_pt,ymax=high_pt),alpha=legis_ci_alpha) + 
      theme_minimal() + ylab("") + xlab("") +
      theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip()
    
    # Add in bill vertical line (50% equiprobability voting line)
      outplot <- outplot + geom_hline(aes(yintercept=mean_bill),linetype=2,alpha=0.6) +
        geom_rug(aes(y=ci_value,linetype=ci_type)) +
        guides(linetype=guide_legend('Bill\nMidpoint\nHPD'),
               colour=guide_legend('')) +
        theme(legend.position = 'bottom')

    
    
    #Whether or not to add a facet_wrap
    
    if(any(object@model_type %in% c(2,4,6)) & abs_and_reg=='both' & length(bill_plot)>1) {
      outplot <- outplot + facet_wrap(~param + bill_type,dir='v')
    } else if(any(object@model_type %in% c(2,4,6)) & abs_and_reg %in% c('both','Absence-inflated') & length(bill_plot)==1) {
      outplot <- outplot + facet_wrap(~param,dir='v') 
    } else if(length(bill_plot)>1) {
      outplot <- outplot + facet_wrap(~bill_type,dir='v') 
    }
    
    
  }
  
  if(show_true==TRUE) {
    outplot <- outplot + geom_point(aes(x=reorder(legis.names,median_pt),y=true_vals),color='black',shape=2)
    
  }

  
  if(return_data==TRUE) {
    
    return_list <- list(outplot=outplot,plot_data=person_params)
    if(!is.null(bill_plot)) {
      return_list$bill_data <- bill_pos
    }
    return(return_list)
    
  } else (
    return(outplot)
  )
  
}

#' Function to compare two fitted idealstan models by plotting ideal points. Assumes that underlying data
#' is the same for both models.
#' @param scale_flip This parameter is set to true if you have two models that are reflected around the ideal point axis. This can happen as a result of identification and is harmless.
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
  
  outplot <- combined_data %>% ggplot(aes(y=reorder(legis.names,median_pt),x=median_pt,color=this_model)) + 
    geom_point() + geom_text(aes(label=reorder(legis.names,median_pt)),size=text_size_label,
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

#' @export
id_plot_all_hist <- function(object,params=NULL,param_labels=NULL,hist_type='all',
                          return_data=FALSE,func=median,...) {
  
  stan_params <- switch(params,
                   absence_diff='A_int',
                   absence_discrim='sigma_abs_full',
                   regular_diff='B_int',
                   regular_discrim='sigma_reg_full',
                   legis='L_full')
  
  estimates <- rstan::extract(object@stan_samples,pars=stan_params)[[1]] 
  param_length <- ncol(estimates)
  estimates <- estimates %>% as_data_frame %>% 
    gather(param,value) 
  if(!is.null(param_labels)) {
    if(length(param_labels)==param_length) {
      estimates$param_id <- paste0(param_labels,'_',1:param_length)  
    } else {
      warning('The length of the parameter labels is not the same as the actual number of parameters. Using default labels instead.')
      estimates$param_id <- paste0(params,'_',1:param_length)
    }
    
  } else {
    estimates$param_id <- paste0(params,'_',1:param_length)
  }
  
  estimates <- select(estimates,value,param_id) %>% group_by(param_id) %>%
    summarize(low_pt=quantile(value,0.1),high_pt=quantile(value,0.9),
                                                          median_pt=func(value))
  estimates <- gather(estimates,obs_type,value,-param_id)
  outplot <- ggplot(estimates,aes(x=value)) + geom_density() + theme_minimal()
  if(hist_type=='all') {
    outplot <- ggplot(estimates,aes(x=value)) + geom_density() + theme_minimal() + facet_wrap(~obs_type,
                                                                                              nrow=3,scales = "free")
  } else if(hist_type=='high') {
    outplot <- filter(estimates,obs_type='high_pt') %>% 
    ggplot(aes(x=value)) + geom_density() + theme_minimal() 
  } else if(hist_type=='low') {
    outplot <- filter(estimates,obs_type='low_pt') %>% 
      ggplot(aes(x=value)) + geom_density() + theme_minimal() 
  } else if(hist_type=='function') {
    outplot <- filter(estimates,obs_type='median_pt') %>% 
      ggplot(aes(x=value)) + geom_density() + theme_minimal() 
  }
  
  if(return_data==TRUE) {
    return(list(plot=outplot,plot_data=estimates))
  } else {
    return(outplot)
  }
  
  
}
#' This function plots the results from a simulation generated by \code{\link{id_sim_gen}}.
#' @export

id_plot_sims <- function(sims,type='RMSE') {

  stat_func <- switch(type,
                      RMSE=id_sim_rmse,
                      Residuals=id_sim_resid)
  
  
  stat_func(sims) %>% 
    bind_rows(.id='ID') %>% 
    ggplot(aes(y=avg,x=Params,ymax=high,ymin=low)) + geom_pointrange(size=.3) +
      theme_minimal() + xlab("Parameters") + ylab(type) + facet_wrap(~ID,scales = "free",
                                                                   ncol=1) +
      theme(panel.grid = element_blank(),
          axis.text.x = element_blank()) + 
      geom_hline(yintercept=0,linetype=3) 
  
}

#' @export
id_plot_rhats <- function(obj) {
  # first get all summaries
  get_out <- rstan::summary(obj@stan_samples)$summary
  data_frame(Rhats=get_out[,'Rhat']) %>% 
    ggplot(aes(x=Rhats)) + theme_minimal() + geom_histogram() +
    ylab('Parameters') +theme(panel.grid=element_blank())
}