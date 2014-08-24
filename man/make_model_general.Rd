\name{make_model_general}
\alias{make_model_general}
\title{Make a model by fitting probabilites
Use the markov orders to determine which probabilities and
lags to use}
\usage{
  make_model_general(probs, max_w_order = 2,
    max_d_order = 2, max_rain_w_order = 0,
    max_rain_d_order = 0, fit_order = 4, inter = FALSE)
}
\arguments{
  \item{probs}{the markov wet/dry probabilities and the
  rain mean/std}

  \item{max_w_order}{the degree of lag to use in
  caluclating the wet probability given that the previous
  day was wet}

  \item{max_d_order}{the degree of lag to use in
  calculating the wet probability given that the previous
  day was dry.}

  \item{fit_order}{the degree of the fourier fit if non
  interactive, if interactive the first fit order used}

  \item{inter}{TRUE if fitting is to be interactive}
}
\value{
  A mixed markov model of maximum order \code{order}, that
  can be used to synthesize a data set
}
\description{
  Make a model by fitting probabilites Use the markov
  orders to determine which probabilities and lags to use
}
\details{
  The probabilities will normally be produced by taking the
  estimates from the raw data.  A fourier fit is done,
  either with a fixed degree of fit, or if \code{inter} is
  \code{TRUE} then the user can choose the degree of fit
  for each fitted probability.  Fitting is robust in
  presence of \code{NA}'s, however, the fitted probabities
  will not contain \code{NA}'s.
}

