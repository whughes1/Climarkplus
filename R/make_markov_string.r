make_markov_string = function(f_order=4,order=1)
{
  if(order>0){
    markov_string=paste("ULAGS:cos(DOY*","0","*2*pi/366)","-1",sep="")
    for(i in 1:f_order){
      temp_c=paste("cos(DOY*",i,"*2*pi/366)",":ULAGS",sep="")
      temp_s=paste("sin(DOY*",i,"*2*pi/366)",":ULAGS",sep="")
      markov_string=paste(markov_string,"+",temp_c,"+", temp_s)
    }
  } else{
    markov_string="cos(DOY*0*2*pi/366) - 1"
    for(i in 1:f_order){
      temp_c=paste("cos(DOY*",i,"*2*pi/366)",sep="")
      temp_s=paste("sin(DOY*",i,"*2*pi/366)",sep="")
      markov_string=paste(markov_string,"+",temp_c,"+", temp_s)
    }
  }
  
  markov_string
  
  
}