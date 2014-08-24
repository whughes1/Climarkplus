
#' Find the offset by which curve best fits data
#' 
#' 
#' @param the model to be plotted
#' 
#' @return none
#' 
#' 
#' @details  A plot is mad
#' 
#' @export

plot_model=function(model,mname=NULL)
{
  
  
   cols=c("red","blue","green","black")
   curves=colnames(mod)
   
   plot(model[,curves[2]],type="l",col="red",ylim=c(0,1.2),
        main=mname,xlab="day",ylab="probability")
   
   for(i in 2:4){
     lines(model[,curves[i+1]],col=cols[i])
   }
   legend("topleft", curves[2:5],lty=c(1,1,1,1),
          col = cols,cex=.8)
   
   
}
