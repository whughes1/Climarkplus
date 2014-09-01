\name{make_approx_model_pl}
\alias{make_approx_model_pl}
\title{Make an approximate model by fitting  estimated probabilites
Use a parameter \code{.pl} file to guide the fitting
process}
\usage{
  make_approx_model_pl(probs, filename = NULL)
}
\arguments{
  \item{probs}{the raw markov wet/dry probabilities and the
  rain mean/std}

  \item{filename}{}
}
\description{
  Make an approximate model by fitting estimated
  probabilites Use a parameter \code{.pl} file to guide the
  fitting process
}
\details{
  The probabilities will normally be produced by taking the
  estimates from the raw data.  A fourier fit is done.
  Which lags are used, the fitting order, and whether an
  offset is applied to get the final curve is determined by
  the values of parameters in the given file.
}

