\name{fit_amounts}
\alias{fit_amounts}
\title{fit an extended Markov model to the amount of rain
Use a parameter \code{.pl} file to guide the fitting
process}
\usage{
  fit_amounts(wms, filename = NULL, others = NULL,
    other_model_string = NULL)
}
\arguments{
  \item{wms}{the raw data, a data set}

  \item{filename}{The parameter file, if NULL the markov
  part of the fitting string will not be produced}

  \item{others}{vector of Names of other rows of the data
  set that are to be used as predictors (no interactions)}

  \item{other_model_string}{Can be anything.  Added to the
  fitting formula verbatim.  Will probably mean the fit
  cannot be used for the simulator}
}
\description{
  fit an extended Markov model to the amount of rain Use a
  parameter \code{.pl} file to guide the fitting process
}
\details{
  The basic idea is to fit the Fourier coefficients of the
  Markov probabilities to the raw data using Gamma
  regression. Which lags are used, the fitting order, and
  whether an offset is applied to get the final curve is
  determined by the values in the parameter file.  This
  produces a markov model of chance of rain (year
  independent).  However, unlike in the probability of a
  wet day case we can have a Markov model of order 0. In
  this case lag independent Fourier coefficients are
  calculated. The parameter \code{others} should be a
  vector or character strings.  It is used to add
  predictors to the fit formula.  The strings should be the
  names of columns in the data set.  The parmeter
  \code{other_model_string} is used to experiment with
  different types of models and is usually NULL
}

