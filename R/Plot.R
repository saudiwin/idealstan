legis_plot <- function(object,...) {
  
  person_params <- rstan::extract(object@stan_samples,pars='L_full')[[1]] %>% 
    as_data_frame %>% gather(key = legis,value=ideal_pts) %>% mutate(legis=stringr::str_extract(pattern = '[0-9]+',
                                                                                                string=legis),
                                                                     legis=as.numeric(legis)) %>% 
    group_by(legis) %>% 
    summarize(low_pt=quantile(ideal_pts,0.1),high_pt=quantile(ideal_pts,0.9),
              median_pt=median(ideal_pts))
  
  person_params %>% ggplot(aes(x=legis,y=median_pt)) + geom_point() + geom_errorbar(aes(ymin=low_pt,ymax=high_pt))
  
}