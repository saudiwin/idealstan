# create function for IRFs with idealstan

# Bob Kubinec

require(idealstan)
require(dplyr)
require(bayesplot)

unemp1 <- readRDS('C:/Users/rkubinec/Documents/idalstan_compare/data/unemp1_fit.rds')

# first modify id_plot_cov function to allow for calculating effects differently for high/low discrim

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
                        recalc_vals=NULL,
                        ...) {
  
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
    
    to_plot <- bind_rows(neg_eff,
                         pos_eff,
                         neg_eff_recalc,
                         pos_eff_recalc)
    
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

id_plot_cov(object,label_high = "Conservative",
            label_low="Liberal",pred_outcome = "Yes",
            new_cov_names = c(`unemp_rate:party_codeR`="Republican X\nUnemployment",
                              `unemp_rate:party_codeI`="Independent X\nUnemployment",
                              unemp_rate="Unemployment",
                              party_codeR="Republican",
                              party_codeI="Independent",
                              `(Intercept)`="Intercept"),
            recalc_vals = c("Republican X\nUnemployment",
                            "Unemployment",
                            "Combined\nRepublican"),
            filter_cov = "Intercept")

ggsave("overall_eff_ar1.png")

# need to select and run all Democrats/Republicans separately

rep_ids <- select(object@score_data@score_matrix,person_id,group_id) %>% 
  distinct %>% filter(group_id=="R")

id_plot_irf(object,label_high = "Conservative",
            label_low="Liberal",pred_outcome = "Yes",
            recalc_vals = T,
            line_type=1,
            line_width = .4,
            line_alpha = 0.3,
            time_label= "Months Since Unemployment Rate Increase",
            line_color='black',
            include=rep_ids$person_id,
            cov_name = c("unemp_rate:party_codeR",
                         "unemp_rate"),
            use_ci=F)

ggsave("irf_rep.png")

dem_ids <- select(object@score_data@score_matrix,person_id,group_id) %>% 
  distinct %>% filter(group_id=="D")

id_plot_irf(object,label_high = "Conservative",
            label_low="Liberal",pred_outcome = "Yes",
            recalc_vals = F,
            line_type=1,
            line_width = .4,
            line_alpha = 0.3,
            time_label= "Months Since Unemployment Rate Increase",
            line_color='black',
            include=dem_ids$person_id,
            cov_name = c("unemp_rate"),
            use_ci=F)

ggsave("irf_dem.png")



# function to generate IRFs

id_plot_irf <- function(object,
                        label_high="Liberal",
                        label_low="Conservative",
                        pred_outcome=NULL,
                        recalc_vals=F,
                        include=NULL,
                        time_calc=10,
                        cov_name=NULL,
                        line_type=2,
                        line_width=1,
                        line_alpha=1,
                        line_color="red",
                        ci_color='black',
                        ci_alpha=0.5,
                        use_ci=TRUE,
                        high_quantile=0.95,
                        low_quantile=0.05,
                        calc_varying=T,
                        ...) {
  
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
  new_names <- switch(cov_type,person_cov=object@score_data@person_cov,
                      discrim_reg=object@score_data@item_cov,
                      discrim_abs=object@score_data@item_cov_miss)
  
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

.irf <- function( time=1,shock=1,
                  adj_in=NULL,
                  y_1=0,
                  total_t=10,
                  old_output=NULL) {
    
    # set up the exogenous shock
    # unless the shock comes from an exogenous covariate beta_x
    if(time==1) {
      x_1 <- shock
    } else {
      x_1 <- 0
    }
    
    print(paste0('Now processing time point ',time))
    
    # Calculate current values of y and x given posterior uncertainty
    
    output <- data_frame(y_shock= adj_in*y_1 + x_1,
                         time=time,
                         iter=1:length(adj_in))
    
    
    if(!is.null(old_output)) {
      new_output <- bind_rows(old_output,output)
    } else {
      new_output <- output
    }
    
    # run function recursively until time limit is reached
    
    if(time<total_t) {
      .irf(time=time+1,
           adj_in=adj_in,
           y_1=output$y_shock,
           total_t=total_t,
           old_output=new_output)
    } else {
      return(new_output)  
    }
}
