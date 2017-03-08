#' @import ggplot2
#' @export
legis_plot <- function(object,labels='legis',return_data=FALSE,...) {

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
  
  outplot <- person_params %>% ggplot(aes(y=reorder(legis.names,median_pt),x=median_pt,color=party)) + 
    geom_point() + geom_text(aes(label=reorder(legis.names,median_pt)),check_overlap=TRUE,hjust='left') +
    geom_errorbarh(aes(xmin=low_pt,xmax=high_pt)) + theme_minimal() + ylab("") + xlab("") +
    theme(axis.text.y=element_blank(),panel.grid.major.y = element_blank())
  
  if(return_data==TRUE) {
    
    return(list(outplot=outplot,plot_data=person_params))
    
  } else (
    return(outplot)
  )
  
}

#' Function to compare two fitted idealstan models by plotting ideal points. Assumes that underlying data
#' is the same for both models.
#'  @export
compare_models <- function(model1=NULL,model2=NULL) {
  
  
}