#'@name mask_util
#'
#'@title Mask Utilities
#' @param mask  a standard mask (see description)
#' @param yl,yu  The upper and lower y limits
#' @param x  a vector or a data set.  If a data set
#'        then xcol  (default "Rain") is masked through
#'        x$DOY
#' @parma na.rm  if \code{TRUE} NA values are not used
#' 
#'@description A mask is a vector of length 366 containing 0s and 1's.
#' The concept is that any observation on a day of year with mask value
#' 0 is ignored (e.g. in fitting routines).  The idea is to mask out
#' the dry season, but the mask can be used to mask out any time
#' period (e.g. the month of February if for some reason the data
#' from February is suspect, or to mask out everything but January
#' and February if you want to focus on those two months).  
#' 
#' This file contains a number of simple utility functions that
#' use a mask.  Note that many functions in this package take an
#' optional mask parameter, in particular all functions in
#' \code{\link{model_util}}. 
#' 
#' Most of the utilities and their parameters are self
#' explanatory
#' 
#'@details \code{mask_plot} can be called when a plot is open.  It puts
#' slightly transparent gray where mask is 0 from yl to yu.
#' 
#'@note There is nothing magic about 366.   The mask functions
#'could be made more general  (many already are)

NULL

#'@rdname mask_util
#'@export

mask_plot=function(mask,yl=par("usr")[3],yu=par("usr")[4]){
  
  #mask out the current plot using a transparent mask
  rect(which(mask==0)-.5,yl,which(mask==0)+.5,yu,
       border=NA,col=rgb(.5,.5,.5,.5))
}


#'@rdname mask_util
#'@export
mean_mask=function(x,mask=NULL,na.rm=F,xcol="Rain"){
  if(is.null(mask)) mask=rep(1, if("DOY" %in% names(x)) 366 else length(x))
  if("DOY" %in% names(x)){
    mask_x=x[,xcol][mask[x$"DOY"]!=0]
  }else{
    mask_x=x[mask!=0]
  }
  ret_val=mean(mask_x,na.rm=na.rm)
  ret_val
}

#'@rdname mask_util
#'@export

sd_mask=function(x,mask=NULL,na.rm=F,xcol="Rain"){
  if(is.null(mask)) mask=rep(1, if("DOY" %in% names(x)) 366 else length(x))
  if("DOY" %in% names(x)){
    mask_x=x[,xcol][mask[x$"DOY"]!=0]
  }else{
    mask_x=x[mask!=0]
  }
  
  ret_val=sd(mask_x,na.rm=na.rm)
  ret_val
}


#'@rdname mask_util
#'@export

length_mask=function(x,mask=NULL,xcol="Rain"){
  if(is.null(mask)) mask=rep(1, if("DOY" %in% names(x)) 366 else length(x))
  if("DOY" %in% names(x)){
    mask_x=x[,xcol][mask[x$"DOY"]!=0]
  }else{
    mask_x=x[mask!=0]
  }
  ret_val=length(x[mask_x])
  ret_val
}



