smooth_temp= function(data,N=4){
  data[,"Tmin,smooth"] = filter(data$Tmin_prev,rep(1/N,N))
  data[,"Tmax_smooth"] = filter(data$Tmax_prev,rep(1/N,N))
  data
}