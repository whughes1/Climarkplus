#' change a probability and number or tries
#' to success/failure form
#' 
#' 
#' @param probs  Probability of success
#' @param atts  number of attempts  
#'
#'
make_bernoulli_vector = function(probs,atts)
{
  success = round(probs*atts)
  len = length(atts)
  b_mat=NULL
  for(i in 1:len) {
    if(!is.na(atts[i])){
      failures = atts[i]-success[i]      
      temp_mat=matrix(nrow=1,ncol=2)
      temp_mat[1,1]=success[i]
      temp_mat[1,2]=failures
         
      b_mat= rbind(b_mat,temp_mat)
      
    }
  }
  b_mat
}