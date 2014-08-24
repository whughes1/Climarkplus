#' Given a markov model, synthesize a data set
#' 
#' 
#' @param m_model  a standard model
#' @param start_year  The year to start the synthetic data
#' @param r_seed if non null, the random seed to use 
#' @param num_years  number of years to synthesize
#' @param start_string  The initial conditons (e.g. wdwdw)
#' @param label  The "station name"
#' @inheritParams construct_model
#' @return A standard data set
#' @note The fuction takes about 7 seconds to synthesize 100 years of data.
#' Thus producing 1000 data sets of 1000 years each (usefull for bootstraping)
#' will take about 20 hours. 
#' @export

synth_data_set=function(m_model,start_year=1970,num_years=10, 
                        r_seed=NULL,start_string=NULL,label=NULL,thresh=0.12){
  order=m_model[1,1]
  if(is.null(label)) label="synth"
  if(is.null(start_string)){
    start_string=paste(rep("d",order),collapse="")
  }
  lags= start_string
 
  tdd=ymd(paste(as.character(start_year),"-1-1"))
  td=as.numeric(as.Date(tdd))
  
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
  
  #tlags=NULL
  #tlags["dd"]=.7
  #tlags["dw"]=.25
  #tlags["wd"]=.5
  #tlags["ww"]=.3
  
  while(line <= length) {
    
    #index=1
    if (runif(1,0,1)< (as.matrix(m_model[,paste("P(w|",lags,")",sep="")]))[index]){
    #if (runif(1,0,1)< tlags[lags]){
        
      w_or_d = "w"
    }
    else{
      w_or_d = "d"
    }
    
    data_set[line,1]=label
    data_set[line,2]=td
    
    
 
    data_set[line,3]= 0  
    if(w_or_d == "w"){
      mean=m_model[index,"rain_mean"]-thresh
      shape=mean^2/m_model[index,"rain_sd"]^2
      rate=shape/mean
      data_set[line,3]= rgamma(1,shape,rate=rate)+thresh
      #data_set[line,3]= 8.0
    }
    
    
    
    
    data_set[line,4]= index   
    lags = paste(w_or_d,substring(lags,1,1),sep="")
    
    if(data_set[line,4]==59){
      y=(line-59)/366 + 1970
      if(!leap_year(y)){
        line=line+1
        index=index+1
        data_set[line,1]=label
        data_set[line,2]=NA
        data_set[line,3]= NA
        data_set[line,4]= 60
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

