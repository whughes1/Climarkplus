
#' Given the coefficients of a fit and a lag provide
#' a string suitable for computing fourier expansion
#' of the given lag
#' 
#' @param cs  the coefficients, for form see details
#' @param lag  a string giving the lag to be found
#' @details This is a very specific function as it will only
#' work with the coefficients of the fits provided by
#' \code{fit_rainy} and \code{fit_amounts}.  It is delicate
#' as any change in the output format of glm will break it.
#' It depends on the name of the Fourier coefficient for lag
#' having the form ULAGS lag : Fourier function.
#' @export
make_fourier_string=function(cs,lag){
  f_string=NULL
  csn=names(cs)
  pat=paste("[^dw]",lag,"[^dw]",sep="") #anything containing lag not preceded or followed by d or w
  temp=cs[grepl(pat,csn)] #coefficients of anything with pfix in name 
  #this give the vector of Fourier coefficients
  #for lag
  tsn=gsub("^[^cs]*","",names(temp))  #get rid or everything up to cos or sin
  tsn= gsub(")$","",tsn) #get rid or trailing paren, what is left is fourier coef
  f_string=cdot(temp,tsn) 
  
  f_string
}