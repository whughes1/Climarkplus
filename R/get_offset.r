
#' Find the offset by which curve best fits data
#' 
#' 
#' @param data the data points to be fitted 
#' @param data_ws weights for the data
#' @param curve the "shape" that will be fitted
#' @param curve_ws  weights for the curve
#' 
#' @return A list consisting of the fitted fitted curve,
#' (curve + intercept), and the intercept
#' 
#' 
#' @details  The data is fitted by least squares to a constant
#' model. the curve is used as an offset.  The weighting applied
#' is the square root of the data weight times the curve weight.
#' It is not necessary that the "shape" have mean 0.
#' 
#' @export

get_offset=function(data,data_ws=NULL,curve,curve_ws=NULL)
{
  if(length(data) != length(curve))
  {
    stop("data and curve must have the same length")
  }
    
    if(is.null(data_ws)) data_ws=rep(1,length(data))
    if(is.null(curve_ws))  curve_ws=rep(1,length(curve))
    
    ws = sqrt(data_ws*curve_ws)
    
    sol=lm(data~offset(curve)+1,weights=ws)
    #curve should not contain any NA's
    no_na= curve + coefficients(sol)[1]
    
    retval=list(no_na,coefficients(sol)[1])
    retval
    
}
