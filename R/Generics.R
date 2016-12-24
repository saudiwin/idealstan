
#' @export
make_idealdata <- function(vote_data=NULL,legis_data=NULL,bill_data=NULL,
                            votes=NULL,abs_vote=NA) {

  if(class(vote_data)=='matrix') {
    if(!is.null(abs_vote)) {
      votes <- c(votes,abs_vote)
    }
    # Register all possible votes as integers, then before running the model we can change them if need be.
    cleaned <- vote_data %>% map(~factor(.x,levels=votes)) %>% map(~as.integer(.x)) %>% as.matrix
  } else if(class(vote_data)=='rollcall') {

  } else {
    stop('Please provide either a matrix or a rollcall object as the vote_data argument.')
  }
  new('idealdata',
      vote_matrix=cleaned,
      legis_data=legis_data,
      vote_labels=votes,
      vote_count=length(votes),
      abs_vote=abs_vote)
}

.make_idealstan <- function() {




  if(use_subset==TRUE) {
    # Subset the datasets by party if one was given
    to_subset <- lapply(member_data,function(x){
      x <- x %>% filter(parliament_bloc %in% subset_party)  %>% select(legis_names)
    })
    to_subset %<>% bind_rows(to_subset) %>% distinct

    cleaned <- lapply(cleaned,function(x) {
      x %<>% filter(legis.names %in% to_subset$legis_names)
    })
  }

  if(sample_it==TRUE) {
    cleaned <- lapply(cleaned,function(x) {
      if(nrow(x)>sample_amt) {
        x <-    x %>% sample_n(sample_amt)
      }
      all_bills <-  grep('Bill',names(x),value=TRUE)
      bills_sample <- sample(all_bills,100)
      bills_sample <- match(bills_sample,names(x))
      x %<>% select(id,legis.names,bloc,type,bills_sample)
      return(x)
    })


  }

  # Should be at least two types of votes per bill for ordinal & binary, three types for ordinal with more than
  # 3 categories
  if(use_nas==FALSE) {
    cleaned <- cleaned %>% lapply(function(y) {
      orig <- y %>% select(-matches("Bill"))
      y <- y %>% select(matches("Bill"))
      y <- y %>%  select_if(function(x) {
        if(length(table(x))<2) {
          FALSE
        } else {
          TRUE
        }
      })
      orig <- bind_cols(orig,y)
    })
  } else if(use_nas==TRUE) {
    cleaned <- cleaned %>% lapply(function(y) {
      orig <- y %>% select(-matches("Bill"))
      y <- y %>% select(matches("Bill"))
      y <- y %>%  select_if(function(x) {
        if(length(table(x))<3) {
          FALSE
        } else {
          TRUE
        }
      })
      orig <- bind_cols(orig,y)
    })
  }

  # Order dataset by legislator names

  cleaned <- lapply(cleaned,function(x){
    # y <-  x %>%  filter(legis.names==refleg)
    # z <- x %>% filter(legis.names!=refleg)
    x <- arrange(x,legis.names)
  })


  return(cleaned)

}

