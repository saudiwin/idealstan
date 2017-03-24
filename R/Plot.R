#' @import ggplot2
#' @import lazyeval
legis_plot <- function(object,labels='legis',return_data=FALSE,bill_plot=NULL,
                       text_size_label=2,text_size_party=2.5,hjust_length=-0.7,
                       legis_ci_alpha=0.5,abs_and_reg='both',...) {

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
  if(is.numeric(object@vote_data@restrict_legis)) {
  reordered <- object@vote_data@restrict_legis
  legis_data <- bind_rows(filter(legis_data,!(row_number() %in% reordered)),
                          slice(legis_data,reordered))
  }
  
  person_params <- rstan::extract(object@stan_samples,pars='L_full')[[1]] %>% 
    as_data_frame %>% gather(key = legis,value=ideal_pts) %>% mutate(legis=stringr::str_extract(pattern = '[0-9]+',
                                                                                                string=legis),
                                                                     legis=as.numeric(legis)) %>% 
    group_by(legis) %>% 
    summarize(low_pt=quantile(ideal_pts,0.1),high_pt=quantile(ideal_pts,0.9),
              median_pt=median(ideal_pts))
  
  person_params <- mutate(person_params,legis.names=legis_data$legis.names,
                          party=legis_data$party)
  
  outplot <- person_params %>% ggplot() + 
    geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(party,median_pt)),size=text_size_party) +
    geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(legis.names,median_pt)),
              check_overlap=TRUE,hjust=hjust_length,size=text_size_label) +
    geom_linerange(aes(x=reorder(legis.names,median_pt),ymin=low_pt,ymax=high_pt),alpha=legis_ci_alpha) + 
    theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip()

  

  if(!is.null(bill_plot)) {
    bill_num <- which(colnames(object@vote_data@vote_matrix) %in% bill_plot)
    bill_discrim_reg <- paste0('sigma_full[',bill_num,']')
    bill_diff_reg <- paste0('B_yes[',bill_num,']')
    
    if(grepl('inflate',object@model_type)) {
      bill_discrim_abs <- paste0('sigma_abs_open[',bill_num,']')
      bill_diff_abs <- paste0('B_abs[',bill_num,']')
      
      to_rstan <- c(bill_discrim_reg,bill_diff_reg,bill_discrim_abs,bill_diff_abs)
    } else {
      to_rstan <- c(bill_discrim_reg,bill_diff_reg)
    }

    bill_pos <- rstan::extract(object@stan_samples,pars=to_rstan) %>% as_data_frame %>% gather(key = bills,value=ideal_pts) %>% 
      group_by(bills)
    
    bill_pos <- lapply(1:length(bill_num),function(x) {

      # Need to solve spread issue
      
      test_data <- filter(bill_pos,bills==bill_diff_reg[x])
      row_length <- nrow(test_data)
      this_dataset <- bill_pos
      this_dataset$row_num <- 1:row_length
      this_dataset <- spread(this_dataset,bills,ideal_pts)
      
      dots <- list(interp(~mean(var1/var2),var1=as.name(bill_diff_reg[x]),var2=as.name(bill_discrim_reg[x])),
                   interp(~quantile(var1/var2,0.9),var1=as.name(bill_diff_reg[x]),var2=as.name(bill_discrim_reg[x])),
                   interp(~quantile(var1/var2,0.1),var1=as.name(bill_diff_reg[x]),var2=as.name(bill_discrim_reg[x])))
      out_reg <- summarize_(this_dataset,.dots=setNames(dots,c('mean_bill','high_bill','low_bill')))
      out_reg <- mutate(out_reg,param='regular')
      if(grepl('inflate',object@model_type)) {
        dots <- list(interp(~mean(var1/var2),var1=as.name(bill_diff_abs[x]),var2=as.name(bill_discrim_abs[x])),
                     interp(~quantile(var1/var2,0.9),var1=as.name(bill_diff_abs[x]),var2=as.name(bill_discrim_abs[x])),
                     interp(~quantile(var1/var2,0.1),var1=as.name(bill_diff_abs[x]),var2=as.name(bill_discrim_abs[x])))
        
        out_abs <- summarize_(this_dataset,.dots=setNames(dots,c('mean_bill','high_bill','low_bill')))
        out_abs <- mutate(out_abs,param='absence')
        out_reg <- bind_rows(out_reg,out_abs)
      }
      
      out_reg <- mutate(out_reg, bill_num=bill_plot[x])
      return(out_reg)
    })
    
    bill_pos <- bind_rows(bill_pos)
    bill_pos <- gather(bill_pos,key = ci_type,value=ci_value,high_bill,low_bill)
    
    #Redo the legislator plot to make room for bill covariates
    
    cols <- object@vote_data@vote_matrix[,bill_plot] %>% as_data_frame
    if(length(bill_plot)>1) {
      person_params <- bind_cols(person_params,cols) %>% gather(bill_type,bill_vote,one_of(bill_plot))
    } else {
      person_params <- bind_cols(person_params,cols) 
      names(person_params) <- c(names(person_params)[-length(names(person_params))],'bill_vote')
      person_params <- mutate(person_params,bill_type=bill_plot)
    }
    person_params <- mutate(person_params,bill_vote=factor(bill_vote,levels=as.numeric(as.factor(object@vote_data@vote_labels)),
                                                           labels = object@vote_data@vote_labels))
    person_params <- left_join(person_params,bill_pos,c('bill_type'='bill_num'))
    
    # Choose a plot based on the user's options
    
    if(grepl('inflate',object@model_type) & abs_and_reg!='both') {
      person_params <- filter(person_params,param==abs_and_reg)
    } 
    
    outplot <- person_params %>% ggplot() + 
      geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(party,median_pt),color=bill_vote),size=text_size_party) + 
      geom_text(aes(x=reorder(legis.names,median_pt),y=median_pt,label=reorder(legis.names,median_pt),color=bill_vote),
                check_overlap=TRUE,hjust=hjust_length,size=text_size_label) +
      geom_linerange(aes(x=reorder(legis.names,median_pt),ymin=low_pt,ymax=high_pt),alpha=legis_ci_alpha) + 
      theme_minimal() + ylab("") + xlab("") +
      theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank()) + coord_flip()
    
    # Add in bill vertical line (50% equiprobability voting line)
    
    outplot <- outplot + geom_hline(aes(yintercept=mean_bill),linetype=2,alpha=0.6) +
      geom_rug(aes(y=ci_value,linetype=ci_type))
    
    #Whether or not to add a facet_wrap
    
    if(grepl('inflate',object@model_type) & abs_and_reg=='both' & length(bill_plot)>1) {
      outplot <- outplot + facet_wrap(~param + bill_type,dir='v')
    } else if(grepl('inflate',object@model_type) & abs_and_reg %in% c('both','absence') & length(bill_plot)==1) {
      outplot <- outplot + facet_wrap(~param,dir='v') 
    } else if(length(bill_plot)>1) {
      outplot <- outplot + facet_wrap(~bill_type,dir='v') 
    }
    
    
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
compare_models <- function(model1=NULL,model2=NULL,scale_flip=FALSE,return_data=FALSE,
                           labels=NULL,hjust=-.1) {
  

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
  
  outplot <- combined_data %>% ggplot(aes(y=reorder(legis.names,median_pt),x=median_pt,color=this_model)) + 
    geom_point() + geom_text(aes(label=reorder(legis.names,median_pt)),check_overlap=TRUE,hjust=hjust) +
    geom_errorbarh(aes(xmin=low_pt,xmax=high_pt)) + theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank())
  
  if(!is.null(labels)) {
    outplot <- outplot + scale_colour_brewer(palette='Set1',labels=labels,guide=guide_legend(title=''))
  }
  
  if(return_data==TRUE) {
    return(list(plot=outplot,plot_data=combined_data))
  } else {
    return(outplot)
  }
  
}

all_hist_plot <- function(object,params=NULL,param_labels=NULL,hist_type='all',
                          return_data=FALSE,func=median,...) {
  
  stan_params <- switch(params,
                   absence_diff='B_abs',
                   absence_discrim='sigma_abs_open',
                   regular_diff='B_yes',
                   regular_discrim='sigma_full',
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