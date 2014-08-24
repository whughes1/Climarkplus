\name{make_model}
\alias{make_model}
\title{Make a model by fitting probabilites}
\usage{
  make_model(probs, order = 2, fit_order = 4,
    inter = FALSE)
}
\arguments{
  \item{probs}{the markov wet/dry probabilities and the
  rain mean/std}

  \item{fit_order}{the degree of the fourier fit if non
  interactive, if interactive the first fit order used}

  \item{order}{The order of the markov model}

  \item{inter}{TRUE if fitting of the model should be
  interactive}
}
\value{
  A markov model of order \code{order}, that can be used to
  synthesize a data set
}
\description{
  Make a model by fitting probabilites
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

