\name{make_eval_string}
\alias{make_eval_string}
\title{Create a string that can be evaluated to give the
output of the model}
\usage{
  make_eval_string(cs, lag, is_rain = FALSE)
}
\arguments{
  \item{cs}{the coefficients of the fit}

  \item{lag}{the lag for which we want a value}

  \item{is_rain}{The function works slightly differently
  for fit_amounts}
}
\description{
  Create a string that can be evaluated to give the output
  of the model
}
\details{
  This function and underlying functions use R string
  manipulation to produce strings that can be evaluated as
  R code
}

