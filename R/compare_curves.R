

 
compare_curves=function(probs,case="d"){
  if(case == "d"){
    #plot(probs$"P(w|d)")
    #points(probs$"P(w|dw)",col="green")
    #points(probs$"P(w|dd)",col="blue")
    line_d=fit_probs(probs$"P(w|d)",ws=probs$"#d",order=4)[[1]]
    line_dd=fit_probs(probs$"P(w|dd)",ws=probs$"#dd",order=4)[[1]]
    line_dw=fit_probs(probs$"P(w|dw)",ws=probs$"#dw",order=4)[[1]]
    #lines(line_d)
    plot(line_d,type="l",ylim=c(0,.8))
    lines(line_dw,col="green")
    lines(line_dd,col="blue")
  }
  else {
    #plot(probs$"P(w|w)")
    #points(probs$"P(w|ww)",col="green")
    #points(probs$"P(w|wd)",col="blue")
    line_w=fit_probs(probs$"P(w|w)",ws=probs$"#w",order=4)[[1]]
    line_wd=fit_probs(probs$"P(w|wd)",ws=probs$"#wd",order=4)[[1]]
    line_ww=fit_probs(probs$"P(w|ww)",ws=probs$"#ww",order=4)[[1]]
    #lines(line_d)
    plot(line_w,type="l",ylim=c(0,.8))
    lines(line_ww,col="green")
    lines(line_wd,col="blue")
  } 
}
  