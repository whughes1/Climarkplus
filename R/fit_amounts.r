#' fit an extended Markov model to the amount of rain
#' Use a parameter \code{.pl} file to guide the fitting
#' process
#' 
#' @param wms  the raw data, a data set
#' @param filename  The parameter file, if NULL the markov
#'        part of the fitting string will not be produced
#' @param others  vector of Names of other rows of the data set that are
#'        to be used as predictors (no interactions)
#' @param other_model_string  Can be anything.  Added to the
#'        fitting formula verbatim.  Will probably mean the
#'        fit cannot be used for the simulator                
#' @param mask  a standard mask see \code{\link{mask_util}}
#' @details The basic idea is to fit the Fourier coefficients of the
#' Markov probabilities to the raw data using Gamma regression.
#' Which lags are used,
#' the fitting order, and whether an offset is applied to get the final curve
#' is determined by the values in the parameter file.  This produces a markov
#' model of chance of rain (year independent).  However, unlike in the
#' probability of a wet day case we can have a Markov model of order 0.
#' In this case lag independent Fourier coefficients are calculated.
#' The parameter \code{others}
#' should be a vector or character strings.   It is used to add predictors
#' to the fit formula.   The strings should be the names of columns in the
#' data set.   The parmeter \code{other_model_string} is used to experiment with
#' different types of models and is usually NULL
#' @export

fit_amounts=function(wms,filename=NULL,others=NULL,other_model_string=NULL,
                     mask=NULL){
  
  temp = make_ulags(wms,filename,is_rain=TRUE)
  if(!is.null(temp)){
    wms["ULAGS"]= as.factor(temp)
  }
  
  #add offset factors
  
  params=read_pl(filename)
  order = as.numeric(params["rain_order"])
  if(order>0){
    levels=c("w","d")
    if(order>1){
      for(i in 1:(order-1)){
        levels=add_level(levels)
      }
    }
  }else{
    levels=NULL
  }
  
  
  for (lev in levels){
    arg= paste("r",lev,"_offset",sep="")
    if(params[arg]=="YES"){
      k=nchar(lev)
      colname=paste("lag_",k,sep="")
      new_colname=paste("Off",lev,sep="")
      wms[new_colname]=wms[,colname]==lev
      wms[,new_colname]=as.numeric(wms[,new_colname])
    }
  }
      
  fit_string=make_fit_string(filename,others=others,
                             other_model_string=other_model_string,
                             is_rain=TRUE)
  
  
  if(is.null(mask)){
    subdata<-subset(wms,wet_or_dry=="w")  
    fit=NULL
    result = tryCatch({
      fit=glm(fit_string,family="Gamma",subdata) 
    }, warning = function(w){ message("GLM gave the warning:\n")
                              message(w)
                              message("\n")
                              message("This may be due to numeric instability")
                              message("The fit may not have been produced")
                              message("If so we try using a simple Gaussian fit")
                              message("but this may cause problems with")
                              message("modeling and/or simulation")
                              
                              #Who needs functional programming
                              #this should probably be a return through
                              #tryCatch
                              if(is.null(fit)) {
                                fit<<-glm(fit_string,gaussian("inverse"),subdata) 
                              }
    }
    , error = function(e){

      message("GLM FIT failed (numerical instability?)")
      message("try a Gaussian fit, but there may be problems")
      
       fit<<-glm(fit_string,gaussian("inverse"),subdata)
    }, finally = { #empty
    })
  } else{
    subdata<-subset(wms,wet_or_dry=="w" & mask[wms$DOY]==1)  
    fit=glm(fit_string,family="Gamma",subdata)
  }
  
  
  info_list=list(params,fit_string,data,others,mask,"end")
  fit_object = list(info_list,fit)
  
  fit_object
  
}