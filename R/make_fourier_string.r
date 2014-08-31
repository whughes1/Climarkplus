
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
  pfix=paste("ULAGS",lag,":",sep="") #eg ULAGSdd:
  temp=cs[grepl(pfix,csn)] #coefficients of anything with pfix in name 
  #this give the vector of Fourier coefficients
  #for lag
  tsn=gsub(pfix,"",names(temp)) #names without pfix (Fourier terms)
  f_string=cdot(temp,tsn) 
  
  f_string
}