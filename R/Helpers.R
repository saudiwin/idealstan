#' Function that works with id_model to re-arrange bills or legislators to constrain for identification
id_params_constrain_guided_inflate <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {

  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  # Use different identifications depending on whether we want to do bills, legislators or both
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    
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
                                legis$avg[1:nfix[2]]),
                unrestricted=lookat_params,
                restrict_legis=keep_rows,
                restrict_bills=keep_cols))
    
  } else if('person' %in% restrict_params) {
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows_low <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    legis <- arrange(legis_est,avg)
    keep_rows_high <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
    param_fix <- 'person'
    return(list(restrict=list(restrict=nfix[2]),matrix=x,param_fix=param_fix,
           restrict_vals=legis$avg[1:nfix[2]],
           unrestricted=lookat_params,
           restrict_legis=c(keep_rows_high,keep_rows_low),
           restrict_bills='None'))
  } else if('bill' %in% restrict_params) {
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
                           sigmas_abs$avg[1:nfix[1]]),
           unrestricted=lookat_params,
           restrict_bills=keep_cols,
           restrict_legis='None'))
  } else {
    stop('Improper identification parameters passed to the identification function.')
  }
  
  
}

id_params_constrain_guided_2pl <- function(lookat_params=NULL,restrict_params=NULL,nfix=NULL,x=NULL) {
  
  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  # Use different identifications depending on whether we want to do bills, legislators or both
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    
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

    keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix[1]])
    param_fix <- c('person','bill','sigma')

    x <- cbind(x[,-keep_cols],x[,keep_cols])
    
    return(list(restrict=list(restrict_l=nfix[2],restrict_b=nfix[1]),matrix=x,param_fix=param_fix,
                restrict_vals=c(sigmas$avg[1:nfix[1]],
                                legis$avg[1:nfix[2]]),
                unrestricted=lookat_params,
                restrict_legis=keep_rows,
                restrict_bills=keep_cols))
    
  } else if('person' %in% restrict_params) {
    legis_est <- lookat_params[,grepl('L_free\\[',colnames(lookat_params))]
    legis_est <- legis_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)
    
    legis <- arrange(legis_est,desc(avg))
    keep_rows_low <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    legis <- arrange(legis_est,avg)
    keep_rows_high <- as.numeric(stringr::str_extract(legis$param_name,'[0-9]+')[1:nfix[2]])
    x <- rbind(x[-c(keep_rows_high,keep_rows_low),],x[c(keep_rows_high,keep_rows_low),])
    param_fix <- 'person'
    return(list(restrict=list(restrict=nfix[2]),matrix=x,param_fix=param_fix,
                restrict_vals=legis$avg[1:nfix[2]],
                unrestricted=lookat_params,
                restrict_legis=c(keep_rows_high,keep_rows_low),restrict_bills='None'))
  } else if('bill' %in% restrict_params) {
    sigmas_est <- lookat_params[,grepl('sigma\\[',colnames(lookat_params))]
    sigmas_est <- sigmas_est %>% as_data_frame %>% gather(param_name,value) %>% group_by(param_name) %>% 
      summarize(avg=mean(value),high=quantile(value,0.95),low=quantile(value,0.05),sd=sd(value),interval=high-low)

     keep_cols <- as.numeric(stringr::str_extract(sigmas$param_name,'[0-9]+')[1:nfix[1]])
      param_fix <- c('bill','sigma')

    x <- cbind(x[,-keep_cols],x[,keep_cols])
    return(list(restrict=list(restrict=nfix[1]),matrix=x,param_fix=param_fix,
                restrict_vals=sigmas$avg[1:nfix[1]],
                unrestricted=lookat_params,
                restrict_bills=keep_cols,
                restrict_legis='None'))
  } else {
    stop('Improper identification parameters passed to the identification function.')
  }
  
  
}

id_params_pin_unguided_inflate <- function(restrict_params=NULL,nfix=NULL,x=NULL,
                                   restrict_names=NULL,bill_names=NULL,person_names=NULL) {
  
  restrict_params <- sort(restrict_params,decreasing=TRUE)
  
  #Create row.names for the vote matrix to preserve order
  
  if(is.null(row.names(x))) {
    row.names(x) <- as.character(1:nrow(x))
  } 
  if(is.null(colnames(x))) {
    colnames(x) <- as.character(1:ncol(x))
  }
  
  if(('person' %in% restrict_params) && ('bill' %in% restrict_params)) {
    if(length(restrict_names)<2) {
      stop('You must pass both a bill and a person name to pin a parameter for both bills and persons.')
    }
    restrict_pos[1] <- which(restrict_names[1]==person_names)
    restrict_pos[2] <- which(restrict_names[2]==bill_names)
    x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
    x <- cbind(x[,-restrict_pos[2]],x[,restrict_pos[2]])
  } else if('person' %in% restrict_params) {
    restrict_pos[1] <- which(restrict_names[1]==person_names)
    x <- rbind(x[-restrict_pos[1],],x[restrict_pos[1],])
  } else if('bill' %in% restrict_params) {
    restrict_pos[1] <- which(restrict_names[1]==bill_names)
    x <- cbind(x[,-restrict_pos[1]],x[,restrict_pos[1]])
  }
  
  return(list(restrict=list(matrix=x,param_fix=restrict_params,restrict_vals=restrict_pos)))
  
}