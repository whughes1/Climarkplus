#' Read a list of parameters of the form <key>=value
#' 
#' @param file  The filename of the parameter file
#' @return  a list of the form list[key]=value  (value a string)
#' @details  Only lines consisting of <key>=value are parsed, other lines
#' are ignored.  
#' @export

read_pl=function(file="filename",text=NULL){
  
  #pat matches a line with <key> = value
  #we need better definitions of key and value
  key_pat="\\w+"
  value_pat="\\S+"
  pat=paste("^ *<",key_pat,"> *= *",value_pat," *$",sep="")
  
  
  p_list=NULL
  
  if(is.null(text)){
    con  <- file(file, open = "r")
    lns=readLines(con)
  }else {
    lns=text
  }
  for (ln in lns){
    if (grepl(pat,ln)){
      x=strsplit(ln,"=")
      p_list[gsub("<|>","",climate_trim(x[[1]][1]))]=climate_trim(x[[1]][2])
    }
  }
  if(is.null(text)){
     close(con)
  }
  p_list
  
}

# a trival key=value reader

climate_kv=function(key_value_string){
  x=strsplit(key_value_string,"=")
  assign(climate_trim(x[[1]][1]),climate_trim(x[[1]][2]),envir=.GlobalEnv)
}
