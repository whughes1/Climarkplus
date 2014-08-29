require(lubridate)
require(zoo)

inputdir   <- "/home/william/Reading/rstudio/packages/climate/data"

#make DOY consistent by making march 1 day 61 for
#every year

yday.ssc <- function(x) {
  tmp <- yday(x)
  if(!is.na(x) && tmp>59  && !leap_year(year(x))){
    tmp = tmp + 1
  }
  tmp
}

#leap year works on dates

v_leap_year = function(x){
  (year(x)%%4 == 0) & ((year(x)%%100 != 0) | (year(x)%%400 == 0)) 
}

#shift a vector

shift=function(x,N=0){
  if(N>=0){
     x[c((N+1):length(x),1:N)]
  }else{
     x[c((length(x)+N+1):length(x),1:(length(x)+N))]
  }
}

#add to a string of w's and d's

add_level=function(list){
  ll=NULL
  for (k in list){
    temp=paste("w",k,sep="")
    ll=c(ll,temp)
    temp=paste("d",k,sep="")
    ll=c(ll,temp)
  }
  ll
}

#trim whitepace from start and end of string

climate_trim=function(s) {
  s=sub("^ *","",s)
  sub(" *$","",s)
}


# a trival key=value reader

climate_kv=function(key_value_string){
  x=strsplit(key_value_string,"=")
  assign(climate_trim(x[[1]][1]),climate_trim(x[[1]][2]),envir=.GlobalEnv)
}
      

#simple function to get a list of levels

add_level=function(list){
  ll=NULL
  for (k in list){
    temp=paste("w",k,sep="")
    ll=c(ll,temp)
    temp=paste("d",k,sep="")
    ll=c(ll,temp)
  }
  ll
}

#a character based dot product

cdot=function(x,y) {
  if(length(x) != length(y)) stop("In cdot vectors must be the same size")
  dot=paste(x[1],"*",y[1])
  if(length(x) > 1){
    
    for(i in 2:length(x)){
      dot= paste(dot,"+", x[i],"*",y[i])
    }
  }
  dot
}

# get inverse logit without loading a new package

inv.logit=function(x){1/(1+exp(-x))}