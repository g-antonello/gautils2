#' Pick samples proportionally from factor
#' 
#' A very simple function that helps picking some samples. I thought it to split data into a training and test set for machine learning puroposes, but it is not perfect. 
#' The next step would be to allow some other covariates to be evenly distributed across the subsets.
#' 
#' @param x a \code{phyloseq} or \code{data.frame} object
#' @param factor a \code{character} vector with the name of the non-numeric variable to split the data set by
#' @param proportion the proportion of samples you want to get from each category in the factor above 
#' @param seed if you want to specify your seed to ensure perfect replication, feel free
#' 
#' @return A \code{list} containing the subsetted data and the remaining data, which you can use as you wish. I thought them as a "training" and "test" subset  
#' @export 
#'
#' @examples
#' 
#' 
#' 
pickEvenSamples_factor <- function(x, factor, proportion, seed = NULL){
  if (class(x) == "phyloseq"){
    data0 <- meta(x)
    data0 <- filter(data0, !is.na(eval(parse(text = factor))))  
  }else{
    data0 <- filter(x, !is.na(eval(parse(text = factor))))  
  }
  
  # get the indices of the rows that correspond to each factor
  indices_per_factor_category <- split(1:nrow(data0), data0[[factor]]) 
  
  # get the subset of indices for each category, but now the indices will actually be the original ones in data0
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  indices_chosen <- sapply(indices_per_factor_category, function(indx) 
    sample(x = indx, size = floor(length(indx)*proportion), replace = FALSE)) %>% 
    unlist() # this function converts the list into an integer vector
  
  if(class(x) == "phyloseq"){
    return(list(chosen = filter_sample_data(x, (1:nrow(data0) %in% indices_chosen)),
                not_chosen = filter_sample_data(x, !(1:nrow(data0) %in% indices_chosen))))
  }else{
    return(list(chosen=data0[indices_chosen,],
              not_chosen = data0[!(1:nrow(data0) %in% indices_chosen),]))
  }
   
}
  

