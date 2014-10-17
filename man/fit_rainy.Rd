\name{fit_rainy}
\alias{fit_rainy}
\title{fit an extended Markov model to the probability of rain
Use a parameter \code{.pl} file to guide the fitting
process}
\usage{
  fit_rainy(wms, filename = NULL, others = NULL,
    other_model_string = NULL, mask = NULL)
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

  \item{mask}{a standard mask see \code{\link{mask_util}}}
}
\description{
  fit an extended Markov model to the probability of rain
  Use a parameter \code{.pl} file to guide the fitting
  process
}
\details{
  The basic idea is to fit the Fourier coefficients of the
  Markov probabilities to the raw data using logistic
  regression. Which lags are used, the fitting order, and
  whether an offset is applied to get the final curve is
  determined by the values in the parameter file.  This
  produces a markov model of chance of rain (year
  independent).  The parameter \code{others} should be a
  vector of character strings.  It is used to add
  predictors to the fit formula.  The strings should be the
  names of columns in the data set.  The parmeter
  \code{other_model_string} is used to experiment with
  different types of models and is usually NULL
}

