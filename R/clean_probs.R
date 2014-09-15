#replace NA's by linear filtered versions.  
#shift so starting or trailing NA's are caught
#by na.approx()
# @note should be moved to climate_utils
clean_probs=function(probs){
  clean=na.approx(probs,na.rm=FALSE)
  s=trunc(length(clean)/2)
  clean=na.approx(shift(clean,s))
  clean=shift(clean,-s)
}