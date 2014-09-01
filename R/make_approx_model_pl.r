#' Make an approximate model by fitting  estimated probabilites
#' Use a parameter \code{.pl} file to guide the fitting
#' process
#' 
#' @param probs  the raw markov wet/dry probabilities and the rain mean/std
#' @param filename
#' 
#' @details  The probabilities will normally be produced by taking the estimates
#' from the raw data.  A fourier fit is done.  Which lags are used,
#' the fitting order, and whether an offset is applied to get the final curve
#' is determined by the values of parameters in the given file.
#' @export

make_approx_model_pl=function(probs,filename=NULL)
  
{
  params=read_pl(filename)
  order = as.numeric(params["order"])
  rain_order=as.numeric(params["rain_order"])
  
  #the mixed markov model is not fully meaningful here
  #pretend we are using a full model
  markov_mod= get_mixed_models(order,order)
  
  
  #we will fill all columns  (some may be duplicates
  #and some columns many be offsets of others)
  
  ncols=1+2**(order)+2
  m_model=matrix(nrow=366,ncol=ncols)
  colnames(m_model)=c("info",rep(" ",2**(order)),"<rain>","sd(rain)")
  
  
  #The first column contains the markov orders.
  #note that we are changing the columns
  #so we use all columns
  #we do not need this info, but it
  #is included so we do not have to change
  #the synthesizer
  
  m_model[1,1]=order
  m_model[2,1]=order
  m_model[3,1]=order
  m_model[4,1]=rain_order
  m_model[5,1]=rain_order
  
  #first we fill all w/d columns
  
  # set up names of 2^order levels
  levels=c("w","d")
  if(order>1){
    for(i in 1:(order-1)){
      levels=add_level(levels)
    }
  }
  
  for(i in 1:2^order)
  {
    #start out with tempcol the "shape", a fitted version
    #of some raw probabilities
    
    
    lag=levels[i]
    colname=paste("P(w|",lag,")",sep="")
    curve_lag=params[lag]
    curve_colname=paste("P(w|",curve_lag,")",sep="")
    fit_order=params[paste(lag,"_fit_order",sep="")]
    tempcol=fit_probs(probs[,curve_colname],
                      ws=probs[,paste("#",curve_lag,sep="")],
                      order=fit_order)[[1]]
    
    #add an offset if needed
    
    offset = params[paste(lag,"_offset",sep="")]
    
    if(!(offset=="NO")){
      tempy=get_offset(data=probs[,paste("P(w|",offset,")",sep="")],
                         data_ws=probs[,paste("#",offset,sep="")],
                         curve=tempcol,
                         curve_ws=probs[,paste("#",curve_lag,sep="")])
      tempcol=tempy[[1]]
    }
    
    m_model[,i+1]=tempcol
    colnames(m_model)[i+1]=colname
    
    
  }
  
  #the rain stuff is a bit messier because we
  #have to deal with the 0 order stuff
  
  
  #the synthesizer will only expect <rain>
  #and sd(rain) columns if the rain_order
  #is 0
  #note there are no offsets availaible for the
  #order 0 stuff
  
  if(rain_order == 0){
    
    fit_order=params["r0_fit_order"]
    
    temp=fit_probs(probs[,"<rain>"],ws=probs[,"# wet days"],
                   order=fit_order,method="std")[[1]]
    m_model[,"<rain>"]=temp
    
    fit_order=params["r0_sd_fit_order"]
    temp=fit_probs(probs[,"sd(rain)"],ws=probs[,"# wet days"],
                   order=fit_order,method="std")[[1]]
    m_model[,"sd(rain)"]=temp
  }
  else{
    #need new set of levels
    levels=c("w","d")
    if(rain_order>1){
      for(i in 1:(rain_order-1)){
        levels=add_level(levels)
      }
    }
    
    
    for( i in 1:2^rain_order){
      
      lag=levels[i]
      
      #mean rain
      
      colname=paste("<(r|",lag,")>",sep="")
      
      probname=params[paste("r",lag,sep="")]
      
      if(probname=="0"){
        probcol="<rain>"
        ws_col="# wet days"
      }
      else
      {
        probcol=paste("<(r|",probname,")>",sep="")
        ws_col=paste("#r",probname,sep="")
      }
      fit_order=params[paste("r",lag,"_fit_order",sep="")]
      temp=fit_probs(probs[,probcol],ws=probs[,ws_col],
                     order=fit_order,method="std")[[1]]
      
      
      
      #add an offset if needed
      
      offset = params[paste("r",lag,"_offset",sep="")]
      
      if(!(offset=="NO")){
        temp=get_offset(data=probs[,paste("P(w|",params[offset],")",sep="")],
                           data_ws=probs[,paste("#r",params[offset],sep="")],
                           curve=temp,
                           curve_ws=probs[,ws_col])[[1]]
      }
      
      
      
      
      t_colnames=c(colnames(m_model),colname)
      m_model=cbind(m_model,temp)
      colnames(m_model)=t_colnames
      
      
      #standard deviation rain
      
      colname=paste("sd(r|",lag,")>",sep="")
      
      probname=params[paste("r",lag,"_sd",sep="")]
      
      if(probname=="0"){
        probcol="sd(rain)"
        ws_col="# wet days"
      }
      else
      {
        probcol=paste("sd(r|",probname,")",sep="")
        ws_col=paste("#r",probname,sep="")
      }
      fit_order=params[paste("r",lag,"_sd_fit_order",sep="")]
      temp=fit_probs(probs[,probcol],ws=probs[,ws_col],
                     order=fit_order,method="std")[[1]]
      
      
      #add an offset if needed
      
      offset = params[paste("r",lag,"_offset",sep="")]
      
      if(!(offset=="NO")){
        temp=get_offset(data=probs[,paste("sd(r|",params[offset],")",sep="")],
                        data_ws=probs[,paste("#r",params[offset],sep="")],
                        curve=temp,
                        curve_ws=probs[,ws_col])[[1]]
      }
      
      
      t_colnames=c(colnames(m_model),colname)
      m_model=cbind(m_model,temp)
      colnames(m_model)=t_colnames
      
      
      
      
      
      
    }
  }
  
  m_model
}
