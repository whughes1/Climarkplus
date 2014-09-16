#'@name mask_util
#'
#'@title Mask Utilities
#'
#'@description A mask is a vector of length 366 containing 0s and 1's.
#' The concept is that any observation on a day of year with mask value
#' 0 is ignored (e.g. in fitting routines).  The idea is to mask out
#' the dry season, but the mask can be used to mask out any time
#' period (e.g. the month of February if for some reason the data
#' from February is suspect).  
#' 
#' This file contains a number of simple utility functions that
#' use a mask.  Note that many functions take an optional mask
#' parameter
#' 
#'@note There is nothing magic about 366.   The mask functions
#'could be made more general

mask_plot=function(mask,yl=par("usr")[3],yu=par("usr")[4]){
  
  #mask out the current plot using a transparent mask
  rect(which(mask==0)-.5,yl,which(mask==0)+.5,yu,
       border=NA,col=rgb(.5,.5,.5,.5))
}
