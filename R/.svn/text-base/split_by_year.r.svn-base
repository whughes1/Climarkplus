#' split a dataset into a list of datasets, indexed by year
#' 
#' @param data  a dataset with column \code{Date}
#' @param year_begins_in_july If we have to add \code{mod_year}
#' when to begin the year
#'
#' @details  A convenience function.  First check if column \code{mod_year}
#' exists and if not call \code{add_dmy} to get it.  Then spit datasets into
#' a list of datasets.  The names of the elements of the list will be the years
#' @export

split_by_year=function(data,year_begins_in_july=FALSE){
  #add year feild if needed
  if(!("mod_year" %in% names(data))){ 
    new_data=add_dmy(data,year_begins_in_july)
  } else{
    new_data=data
  }
  out=split(new_data,new_data$mod_year)
  out
}