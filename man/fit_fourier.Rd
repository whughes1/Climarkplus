\name{fit_fourier}
\alias{fit_fourier}
\title{fit the points given by pts, by a fourier seriers}
\usage{
  fit_fourier(pts, ws = NULL, order = 2, thresh = 5,
    method = "standard")
}
\arguments{
  \item{pts}{The points to be fitted}

  \item{ws}{optonal weights}

  \item{order}{The order of the fourier fit or "choose" for
  a simple automatic choise}

  \item{thresh}{The amount the deviance of the fit must
  decrease if the order is to be used}

  \item{method}{The method to be used in the fit.
  Currently if this is set to "bernoulli" bernoulli fitting
  is done, otherwise least squares with the given weights
  (bernoulli assumes pts is a vector of probability of
  success and ws is the number of attempts)}
}
\value{
  A list of two items, the fitted curve (without NA's) and
  the R fit object
}
\description{
  fit the points given by pts, by a fourier seriers
}
\details{
  fit a fourer series of order \code{order} to pts.  If
  \code{order=="choose"} then keep trying higher orders
  until the drop in deviance is less than thresh (note this
  was originally fit_probs)
}

