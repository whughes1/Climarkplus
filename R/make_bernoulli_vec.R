make_bernoulli_vector = function(probs,weights)
{
  success = round(probs*weights)
  len = length(weights)
  b_mat=NULL
  for(i in 1:len) {
    if(!is.na(weights[i])){
      failures = weights[i]-success[i]      
      temp_mat=matrix(nrow=1,ncol=2)
      temp_mat[1,1]=success[i]
      temp_mat[1,2]=failures
         
      b_mat= rbind(b_mat,temp_mat)
      
    }
  }
  b_mat
}