mod_temp_prev = function(data){
  data[,"Tmin_prev"] = data$Tmin
  data[,"Tmax_prev"] = data$Tmax
  data
}