#' Function that works with id_model to re-arrange bills or legislators to fix/constrain for identification
id_params <- function(lookat_params=NULL,fixparams=NULL,nfix=NULL,x=NULL) {

  fixparams <- sort(fixparams,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  # Use different identifications depending on whether we want to do bills, legislators or both
  if(('person' %in% fixparams) && ('bill' %in% fixparams)) {
    
    # First pull the person fixes
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    x <- rbind(x[-keep_rows,],x[keep_rows,])
    
    # Then do the bill fixes
    
    sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
    sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    sigmas <- arrange(sigmas_est,avg)
    sigmas_est_abs <- lookat_params[,grepl('sigma_abs',colnames(lookat_params))]
    sigmas_est_abs <- sigmas_est_abs %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    sigmas_abs <- arrange(sigmas_est_abs,avg)
    if((mean(sigmas$avg[1:nfix[1]])<mean(sigmas_abs$avg[1:nfix[1]])) & (mean(sigmas$interval[1:nfix[1]])<mean(sigmas_abs$interval[1:nfix[1]]))) {
      keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix[1]])
      param_fix <- c('person','bill','sigma')
    } else {
      keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix[1]])
      param_fix <- c('person','bill','sigma_abs')
    }
    x <- cbind(x[,-keep_cols],x[,keep_cols])
    
    return(list(restrict=list(restrict_l=nfix[2],restrict_b=nfix[1]),matrix=x,param_fix=param_fix,
                restrict_vals=c(sigmas_abs$avg[1:nfix[1]],
                                sigmas$avg[1:nfix[1]],
                                legis$avg[1:nfix[2]])))
    
  } else if('person' %in% fixparams) {
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    x <- rbind(x[-keep_rows,],x[keep_rows,])
    param_fix <- 'person'
    return(list(restrict=list(restrict=nfix[2]),matrix=x,param_fix=param_fix,
           restrict_vals=legis$avg[1:nfix[2]]))
  } else if('bill' %in% fixparams) {
    sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
    sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    sigmas <- arrange(sigmas_est,avg)
    sigmas_est_abs <- lookat_params[,grepl('sigma_abs',colnames(lookat_params))]
    sigmas_est_abs <- sigmas_est_abs %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    sigmas_abs <- arrange(sigmas_est_abs,avg)
    if((mean(sigmas$avg[1:nfix[1]])<mean(sigmas_abs$avg[1:nfix[1]])) & (mean(sigmas$interval[1:nfix[1]])<mean(sigmas_abs$interval[1:nfix[1]]))) {
      keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix[1]])
      param_fix <- c('bill','sigma')
    } else {
      keep_cols <- as.numeric(stringr::str_extract(sigmas_abs$param_name,'[0-9]+')[1:nfix[1]])
      param_fix <- c('bill','sigma_abs')
    }
    x <- cbind(x[,-keep_cols],x[,keep_cols])
    return(list(restrict=list(restrict=nfix[1]),matrix=x,param_fix=param_fix,
           restrict_vals=c(sigmas$avg[1:nfix[1]],
                           sigmas_abs$avg[1:nfix[2]])))
  } else {
    stop('Improper identification parameters passed to the identification function.')
  }
  
  
}