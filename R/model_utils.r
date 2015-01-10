
#'@name model_util
#'
#'@title Model Utilities
#' @param dta    a standard data set
#' @param mask  a standard mask (of length 366)
#' @param plot  If true a very basic plot is made
#' @param spell  "w" for wet spells, "d" for dry spells
#' @parma na.rm  if \code{TRUE} NA values are not used
#' 
#'@description These utilities are to help examine and evaluate
#'datasets and models.  Most have a plot utility.  However, this plot is
#'very simple and is just meant as a sanity check.
#'These utilities are very dependent on the properties of standard
#'data sets and standard masks.  
#'
NULL

#'@rdname model_util
#'@export

avg_daily_rainfall=function(dta,plot=FALSE,mask=NULL) {
  data_day=split(dta$Rain,dta$DOY)
  avg_d_r=sapply(data_day,mean,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_d_r,type="l")
    if(!is.null(mask)) mask_plot(mask)
  }
  avg_d_r
}

#'@rdname model_util
#'@export
#'
sd_daily_rainfall=function(dta,plot=FALSE,mask=NULL) {
  data_day=split(dta$Rain,dta$DOY)
  sd_d_r=sapply(data_day,sd,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_d_r,type="l")
    if(!is.null(mask)) mask_plot(mask)
  }
  sd_d_r
}

#'@rdname model_util
#'@export

avg_yearly_rainfall=function(dta,plot=FALSE,mask=NULL) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy,dta_wy$mod_year)
  avg_y_r=sapply(data_year,mean_mask,mask=mask,na.rm=TRUE)
  if(plot ==TRUE){
    plot(avg_y_r,type="l")
  }
  avg_y_r
}

#'@rdname model_util
#'@export

sd_yearly_rainfall=function(dta,plot=FALSE,mask=NULL) {
  dta_wy=add_dmy(dta)
  data_year=split(dta_wy,dta_wy$mod_year)
  sd_y_r=sapply(data_year,sd_mask,mask=mask,na.rm=TRUE)
  if(plot ==TRUE){
    plot(sd_y_r,type="l")
  }
  sd_y_r
}

#'@rdname model_util
#'@export

avg_day_rain_in_year=function(dta,plot=FALSE,mask=NULL) {
  if(is.null(mask)) mask=rep(1,366)
  dta_wm=add_markov(dta)
  dta_wy=add_dmy(dta_wm)
  data_year=split(dta_wy,dta_wy$mod_year)
  avg_dr_y=sapply(data_year,
                  function(i){sum((mask[i$DOY] != 0) &
                                    (i$wet_or_dry == "w"),na.rm=T)})
  if(plot ==TRUE){
    plot(avg_dr_y,type="l")
  }
  avg_dr_y
}

#'@rdname model_util
#'@export

spell_dist=function(dta,plot=FALSE,mask=NULL,spell="w"){
  
  #mask off values as NA
  dta=add_markov(dta)
  dta$wet_or_dry[mask[dta$DOY] == 0] = NA
  
  unsorted_runs=rle(dta$wet_or_dry)
  
  #keep only runs of type spell
  temp=(unsorted_runs$values == spell)
  temp[is.na(temp)]=FALSE
  
  sorted_runs= sort(unsorted_runs$length[temp])
  
  run_distribution = rle(sorted_runs)
  
  if(plot==T){
    plot(run_distribution$values,run_distribution$lengths,
         type="h",lwd=30,lend="butt")
  }
  
  
  run_distribution
  
}

#'@rdname model_util
#'@export


make_stats_object=function(dta,mask=NULL){
  
  
  
  stats_no_mask=NULL
  stats_mask=NULL
  
  
  stats_no_mask[[1]]=avg_daily_rainfall(dta)
  stats_no_mask[[2]]=sd_daily_rainfall(dta)
  stats_no_mask[[3]]=avg_yearly_rainfall(dta)
  stats_no_mask[[4]]=sd_yearly_rainfall(dta)
  stats_no_mask[[5]]=avg_day_rain_in_year(dta)
  stats_no_mask[[6]]=spell_dist(dta,spell="d")
  
  names(stats_no_mask) = c("Avg daily rainfall","SD daily rainfall",
                           "Avg yearly rainfall","SD yearly rainfall",
                           "Avg day rain", "Number of Dry Spells")
  
  
  
  
  if(!is.null(mask)){
    
    stats_mask=NULL
    stats_mask[[1]]=avg_daily_rainfall(dta,mask=mask)
    stats_mask[[2]]=sd_daily_rainfall(dta)
    stats_mask[[3]]=avg_yearly_rainfall(dta)
    stats_mask[[4]]=sd_yearly_rainfall(dta)
    stats_mask[[5]]=avg_day_rain_in_year(dta)
    stats_mask[[6]]=spell_dist(dta,spell="d")
    
    names(stats_mask) = c("Avg daily rainfall","SD daily rainfall",
                          "Avg yearly rainfall","SD yearly rainfall",
                          "Avg day rain", "Number of Dry Spells")
  }
  
  
  ret_val=list(stats_no_mask,stats_mask)
  names(ret_val)=c("No Mask","Mask")
  
  ret_val
}