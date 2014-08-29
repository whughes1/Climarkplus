make_fourier_string=function(cs,lag){
  f_string=NULL
  csn=names(cs)
  pfix=paste("ULAGS",lag,":",sep="") #eg ULAGSdd:
  temp=cs[grepl(pfix,csn)] #coefficients of anything with pfix in name 
  #this give the vector of Fourier coefficients
  #for lag
  tsn=gsub(pfix,"",names(temp)) #names without pfix (Fourier terms)
  f_string=cdot(temp,tsn) 
  
  f_string
}