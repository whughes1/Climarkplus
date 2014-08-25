#' Given a mixed markov model set, synthesize a data set
#' 
#' 
#' @param m_model  a mixed markov model
#' @param start_year  The year to start the synthetic data
#' @param r_seed if non null, the random seed to use 
#' @param num_years  number of years to synthesize
#' @param start_string  The initial conditons (e.g. wdwdw)
#' @param label  The "station name"
#' @inheritParams construct_model
#' @return A standard data set
#' @note The function takes about 7 seconds to synthesize 100 years of data.
#' Thus producing 1000 data sets of 1000 years each (usefull for bootstraping)
#' will take about 20 hours. 
#' @export

synth_data_set_mod=function(m_model,start_year=1970,num_years=10, 
                        r_seed=NULL,start_string=NULL,
                        rain_start_string=NULL,
                        label=NULL,thresh=0.12){
  
  
  if(is.null(label)) label="synth"
  
  
  #set up initial lags, for w or d and for rain ammount
  #the length of the lag is the max of the two orders
  #the length of the lag for rain may be different
  
  
  order=m_model[1,1]
  markov_mod = get_mixed_models(m_model[2,1],m_model[3,1])
  if(is.null(start_string)){
    start_string=paste(rep("d",order),collapse="")
  }
  lags= start_string
  
  r_order=max(m_model[4,1],m_model[5,1])
  r_markov_mod = get_mixed_models(m_model[4,1],m_model[5,1])
  if(is.null(rain_start_string)){
    rain_start_string=paste(rep("d",r_order),collapse="")
  }
  r_lags= rain_start_string
  
  tdd=ymd(paste(as.character(start_year),"-1-1"))
  td=as.numeric(as.Date(tdd))
  
  #The data set will be a bit
  #bigger than needed as non leap years
  #need only 365 rows.  Just leave this
  # for now
  
  length=366*num_years
  
  data_set= matrix(nrow=length,ncol=4)
  
  colnames(data_set)=c("Station","Date","Rain","DOY")
  
  

  
  
  line=1
  index=yday.ssc(tdd)
  
  if(!is.null(r_seed)){
     set.seed(r_seed)
  }else{
    set.seed(as.numeric(Sys.time()))
  }
  
  while(line <= length) {
    
    #determine if the day is wet or dry
    #use a markov model
    
    
    if (runif(1,0,1)< (as.matrix(m_model[,paste("P(w|",
      markov_mod[lags],")",sep="")]))[index]){
      w_or_d = "w"
    }
    else{
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
    
    
    
    
    
    
    if(w_or_d == "w"){
      
      r_string=r_markov_mod[r_lags]
      if(r_string == "" | r_order==0) {
        r_string="<rain>"
      }else{
        r_string=paste("<(r|",r_string,")>",sep="")
      }
      
      sd_string=r_markov_mod[r_lags]
      if(sd_string == "" | r_order==0) {
        sd_string="sd(rain)"
      }else{
        sd_string=paste("sd(r|",sd_string,")",sep="")
      }
      
      mean=m_model[index,r_string]-thresh
      shape=mean^2/m_model[index,sd_string]^2
      rate=shape/mean
      data_set[line,3]= rgamma(1,shape,rate=rate)+thresh
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
    
    
    index=(index+1)%%366
    if(index==0)index=366
    
  }
  synth=as.data.frame(data_set)
  synth$Rain=as.numeric(as.character(synth$Rain))
  synth$DOY=as.numeric(as.character(synth$DOY))
  synth$Date=as.Date(as.numeric(as.character(synth$Date)),origin="1970-1-1")
  synth
  
}

