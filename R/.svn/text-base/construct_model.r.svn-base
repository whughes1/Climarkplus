#' Construct a markov model from a standard data set.
#' This model can be used to synthesize a data set
#' 
#' 
#'
#' @param data  a standard data set (e.g. every year has DOY 61 March 1)
#' @param order  The order of the markov model
#' @param thresh  The rain threshold below which a day will be assumed
#' not to have rain
#' @param inter  TRUE if fitting of the model should be interactive
#' @return A standard model
#' @export

construct_model = function(data,order=2,thresh=0.12,inter=FALSE){
  data_wm=add_markov(data,order=order,thresh=thresh)
  data_wp=make_probs(data_wm,order=order,thresh=thresh)
  model=make_model(data_wp,order=order,fit_order=4,inter=inter)
}