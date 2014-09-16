#' Given a model object, simulate a data set
#' 
#' 
#' @param model  a standard model object
#' @param start_year  The year to start the synthetic data
#' @param r_seed if non null, the random seed to use 
#' @param num_years  number of years to synthesize
#' @param start_string  The initial conditons (e.g. wdwdw)
#' @param label  The "station name"
#' @return A standard data set
#' @details  simulator gets the probablity of rain
#'          (given the previous days pattern of w and d)
#'          and determines if the day in rainy.  If so
#'          it then determined how much rain will fall
#'          (using a gamma distribution).  Currently it
#'          only uses stuff that is dependent on DOY
#'          and assumes that the numbers in the model
#'          represent probablities.
#' @note The function takes about 90 seconds to synthesize 1000 years of data.
#' Thus producing 1000 data sets of 1000 years each (usefull for bootstraping)
#' will take about 25 hours.  Longer data stretches  (>10,000 years) should
#' be possible but cause memory problems.  Tests suggest that memory
#' problems start around 5000 years, and become major around 20,000
#' years.   If data stretches of longer than 20,000 years are needed then
#' the function needs a different approach  (going to 64 bit R might
#' also help)
#' @export

simulate_data_set=function(model,start_year=1970,num_years=10, 
                        r_seed=NULL,start_string=NULL,
                        rain_start_string=NULL,
                        label=NULL,thresh=0.12){
  
  
  if(is.null(label)) label="simulated"
  
  
  #set up initial lags, for w or d and for rain ammount
  #the length of the lag is the max of the two orders
  #the length of the lag for rain may be different
  
  #we need to break out the markov model
  #(length 366 only dependent on DOY)
  #and the others vectors (longer, indexed
  #by julian day)
  
  info=model[[1]]
  markov_mod=model[[2]]
  others_offset=model[[3]]
  others_rain_offset=model[[4]]
  
  order=as.numeric(read_pl(text=info[1]))
  
  
  
  if(is.null(start_string)){
    start_string=paste(rep("d",order),collapse="")
  }
  lags= start_string
  
  r_order=as.numeric(read_pl(text=info[2]))
  shape=as.numeric(read_pl(text=info[3]))
  
  if(is.null(rain_start_string)){
    rain_start_string=paste(rep("d",r_order),collapse="")
  }
  r_lags= rain_start_string
  
 
  tdd=ymd(paste(as.character(start_year),"-1-1"))
  ldd=ymd(paste(as.character(start_year+num_years),"-1-1"))
  
  td=as.numeric(as.Date(tdd))
  
  #Use the exact number of days as the length
  
  length = as.numeric(as.Date(ldd)-as.Date(tdd))

  
  data_set= matrix(nrow=length,ncol=4)
  
  colnames(data_set)=c("Station","Date","Rain","DOY")
  
  
  #As we go through we will need both the day of year
  #(index) and the julian day index jul_index
  line=1
  index=yday.ssc(tdd)
  
  
  #other_offset set up.
  #default is other offset 0
  #for speed  (no need to check if other offset
  #exists)
  
  if(!is.null(others_offset)){
    julday=julian(tdd)
    first_jul = read_pl(text=info[4])
    jul_index=first_jul-julday
  }else{
    others_offset=(rep(0,length))
    jul_index=1
  }
  if(!is.null(others_rain_offset)){
    julday=julian(tdd)
    first_jul = read.pl(text=info[5])
    jul_rain_index=first_jul-julday
  }else{
    others_rain_offsets=(rep(0,length))
    jul_rain_index=1
  }
  
  if(!is.null(r_seed)){
     set.seed(r_seed)
  }else{
    set.seed(as.numeric(Sys.time()))
  }
  
  
  print("start loop")
  
  while(line <= length) {
    
    #determine if the day is wet or dry
    #use a markov model
    
    mark_val = markov_mod[,paste("P(w|",lags,")",sep="")][index]
    
    #we now need to add contributions from others
    
    oth_val=others_offset[jul_index]
    
    wprob= inv.logit(mark_val + oth_val)  #we need probablility space
    
    if (runif(1,0,1)< wprob){
      w_or_d = "w"
    } else{
      w_or_d = "d"
    }
    
    data_set[line,1]=label
    data_set[line,2]=td
    
    
 
    data_set[line,3]= 0  
    
    # if the day is wet determine the amount
    # of rain,  use a gamma distribution with
    # the parameters from a markov model
    # note we need a special case when both
    # wet and dry are order 0
    
    
    #We determine the amount of rain
    #whether or not it is used to keep
    #the random number generator in step
    
    r_string=r_lags
    if(r_string == "" | r_order==0) {
      r_string="<rain>"
    }else{
      r_string=paste("<(r|",r_string,")>",sep="")
    }
     
    
    oth = others_rain_offsets[jul_rain_index]
    
    mean=1/(markov_mod[index,r_string]+oth)-thresh
    rate=shape/mean
    t_rain= rgamma(1,shape,rate=rate)+thresh
    
    if(w_or_d == "w"){
      
      data_set[line,3]= t_rain
      
    }
    
    
    data_set[line,4]= index   
    lags = paste(w_or_d,substring(lags,1,nchar(lags)-1),sep="")
    r_lags = paste(w_or_d,substring(r_lags,1,nchar(r_lags)-1),sep="")
    
     #In non leap years add 1 to
     # index, as we do not have day 60
   
    if(data_set[line,4]==59){
      y=round((line-59)/365.25) + start_year
      if(!leap_year(y)){        
        index=index+1
      }
    }
    
    td=td+1
    line=line+1
    jul_index = jul_index+1
    jul_rain_index= jul_rain_index+1
    
    
    index=(index+1)%%366
    if(index==0)index=366
    
  }
  
  print("finished loop")
  
  #ugly and a bit slow
  #need better stuff for long >10,000 year
  #data sets
  
  
  
  synth=as.data.frame(data_set)
  synth$Rain=as.numeric(as.character(synth$Rain))
  synth$DOY=as.numeric(as.character(synth$DOY))
  synth$Date=as.Date(as.numeric(as.character(synth$Date)),origin="1970-1-1")
  synth
  
}

