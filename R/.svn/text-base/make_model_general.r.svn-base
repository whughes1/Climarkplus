#' Make a model by fitting probabilites
#' Use the markov orders to determine which probabilities and
#' lags to use
#' 
#' @param probs  the markov wet/dry probabilities and the rain mean/std
#' @param max_w_order  the degree of lag to use in caluclating the wet probability given that
#' the previous day was wet
#' @param max_d_order the degree of lag to use in calculating
#' the wet probability given that the previous day was dry.
#' @param fit_order  the degree of the fourier fit if non interactive, if
#' interactive the first fit order used
#' @param inter  TRUE if fitting is to be interactive
#' @return A mixed markov model of maximum order \code{order}, that can be used to synthesize
#' a data set
#' @details  The probabilities will normally be produced by taking the estimates
#' from the raw data.  A fourier fit is done, either with a fixed degree of fit,
#' or if \code{inter} is \code{TRUE} then the user can choose the degree of fit
#' for each fitted probability.  Fitting is robust in presence of \code{NA}'s,
#' however, the fitted probabities will not contain \code{NA}'s.
#' @export

make_model_general=function(probs,max_w_order=2,max_d_order=2,
                            max_rain_w_order=0, max_rain_d_order=0,
                            fit_order=4,inter=FALSE){
  
  
  
  
  markov_mod= get_mixed_models(max_w_order,max_d_order)
  order = as.numeric(markov_mod[1])
  
  #may not use all columns but leave
  #calculation for now
  
  ncols=1+2**(order)+2
  m_model=matrix(nrow=366,ncol=ncols)
  colnames(m_model)=c("info",rep(" ",2**(order)),"<rain>","sd(rain)")
  
  #The first column contains the markov orders.
  
  m_model[1,1]=order
  m_model[2,1]=max_w_order
  m_model[3,1]=max_d_order
  m_model[4,1]=max_rain_w_order
  m_model[5,1]=max_rain_d_order
  
  num_tries=2**order
  tried=NULL
  colnum=0
  
  for(i in 1:num_tries){
     
    c_name = markov_mod[[i+1]]
    if ( c_name == "")  next  #  no markov probs needed
    if ( c_name %in% tried) next #already done this one
    colnum = colnum+1
    tried[colnum] = c_name
    colname=paste("P(w|",c_name,")",sep="")
    
    
    #each of the lines is a fit of the raw probabilities
    if(!inter){
      temp=fit_probs(probs[,colname],
                        ws=probs[,paste("#",c_name,sep="")],
                        order=fit_order)
      tempcol=temp[[1]]
    }else{
      tempcol=fit_probs_inter(probs[,colname],
                              ws=probs[,paste("#",c_name,sep="")],N=1,
                              start_order=fit_order,
                              y_label=colname)
    }
    m_model[,colnum+1]=tempcol
    colnames(m_model)[colnum+1]=colname
  }

  #order 0 columns have different names, treat separately
  if((max_rain_w_order == 0) | (max_rain_d_order==0) ){
    
    
    #fit the mean and standard deviation of rainfall on rainy days
    if(!inter){
      temp=fit_probs(probs[,"<rain>"],ws=probs[,"# wet days"],
                        order=fit_order,method="std")
      tempcol=temp[[1]]
    }else{
      tempcol=fit_probs_inter(probs[,"<rain>"],ws=probs[,"# wet days"],
                              N=1,start_order=fit_order,
                              y_label="<rain>",method="std") 
    }
    m_model[,"<rain>"]=tempcol
    if (!inter){
      temp=fit_probs(probs[,"sd(rain)"],ws=probs[,"# wet days"],
                        order=fit_order,method="std")
      tempcol=temp[[1]]
    }else{
      tempcol=fit_probs_inter(probs[,"sd(rain)"],ws=probs[,"# wet days"],
                              N=1,start_order=fit_order,
                              y_label="sd(rain)",method="std")
    }
    m_model[,"sd(rain)"]=tempcol
  }
  
  
  num_tries=2^max(max_rain_w_order,max_rain_d_order)
  markov_mod=get_mixed_models(max_rain_w_order,max_rain_d_order)
  
  if(num_tries > 1){
    tried=NULL
    t_count=0
    
    for(i in 1:num_tries){
      
      c_name = markov_mod[[i+1]]
      if ( c_name == "")  next  #  no markov probs needed
      if ( c_name %in% tried) next #already done this one
      colnum = colnum+1
      t_count=t_count+1
      tried[t_count] = c_name
      colname=paste("<(r|",c_name,")>",sep="")
      
      
      #each of the lines is a fit of the raw probabilities
      if(!inter){
        temp=fit_probs(probs[,colname],
                          ws=probs[,paste("#r",c_name,sep="")],
                          order=fit_order,method="std")
        tempcol=temp[[1]]
      }else{
        tempcol=fit_probs_inter(probs[,colname],
                                ws=probs[,paste("#r",c_name,sep="")],N=1,
                                start_order=fit_order,
                                y_label=colname,method="std")
      }
      t_colnames=c(colnames(m_model),colname)
      m_model=cbind(m_model,tempcol)
      colnames(m_model)=t_colnames
      
      colname=paste("sd(r|",c_name,")",sep="")
      
      
      #each of the lines is a fit of the raw probabilities
      if(!inter){
        temp=fit_probs(probs[,colname],
                          ws=probs[,paste("#r",c_name,sep="")],
                          order=fit_order,method="std")
        tempcol=temp[[1]]
      }else{
        tempcol=fit_probs_inter(probs[,colname],
                                ws=probs[,paste("#r",c_name,sep="")],N=1,
                                start_order=fit_order,
                                y_label=colname,method="std")
      }
      t_colnames=c(colnames(m_model),colname)
      m_model=cbind(m_model,tempcol)
      colnames(m_model)=t_colnames
     
    }
  }
  
  
  m_model   
}