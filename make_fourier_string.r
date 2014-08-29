make_fourier_string=function(cs,ulags){
  f_string=NULL
  csn=names(cs)
  for (lag in ulags){
    pfix=paste("ULAGS",lag,":",sep="") #eg ULAGSdd:
    temp=cs[grepl(pfix,csn)] #coefficients of anything with pfix in name 
                             #this give the vector of Fourier coefficients
                             #for lag
    tsn=gsub(pfix,"",names(temp)) #names without pfix (Fourier terms)
    f_string[lag]=cdot(temp,tsn) #note we can index f_string by lag
  }
  f_string
}