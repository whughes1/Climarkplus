\name{make_model_pl}
\alias{make_model_pl}
\title{Make a model by fitting probabilites
Use a parameter \code{.pl} file to guide the fitting
process}
\usage{
  make_model_pl(probs, filename = NULL)
}
\arguments{
  \item{probs}{the raw markov wet/dry probabilities and the
  rain mean/std}

  \item{filename}{}
}
\description{
  Make a model by fitting probabilites Use a parameter
  \code{.pl} file to guide the fitting process
}
\details{
  The probabilities will normally be produced by taking the
  estimates from the raw data.  A fourier fit is done.
  Which lags are used, the fitting order, and whether an
  offset is applied to get the final curve is determined by
  the values of parameters in the given file.
}
\note{
  only produces an approximate model
}

