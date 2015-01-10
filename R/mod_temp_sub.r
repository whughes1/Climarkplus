mod_temp_sub = function(data){
  data[,"Tmin_sub"] = data$Tmin_smooth
  data[,"Tmax_sub"] = data$Tmax_smooth
  data
}