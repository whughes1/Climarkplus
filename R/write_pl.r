
write_pl=function(params,filename){
  

  out="Automaticly Produced Parameter File\n"


nms = names(params)

for (nm in nms){
  temp=paste("<",nm,"> = ",params[nm],sep="")
  out=c(out,temp)
}

out1=out[1]
out2=out[grepl("^<[^r]",out)]
out3=out[grepl("^<r",out)]
output = c(out1,out2[sort.list(out2)],out3[sort.list(out3)])
writeLines(output,filename)
#close(fileConn)
}
