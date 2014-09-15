\name{fit_probs_inter}
\alias{fit_probs_inter}
\title{fit the points given by pts, by a fourier seriers
Allow the user to see the fit given by
different orders and choose the order}
\usage{
  fit_probs_inter(probs, ws = NULL, start_order = 1, N = 8,
    y_label = "", method = "bernoulli")
}
\arguments{
  \item{pts}{The points to be fitted}

  \item{ws}{optonal}

  \item{start_order}{The first order to be displayed}

  \item{N}{pts is smoothed by a rectangular window of
  length N to aid in visual smoothing (the fit does not use
  the smoothed data)}

  \item{y_label}{A label to put on the y axis}

  \item{method}{The method to be used in the fit.
  Currently if this is set to "bernoulli" bernoulli fitting
  is done, otherwise least squares with the given weights
  (bernoulli assumes pts is a vector of probaility of
  success and ws is the number of attempts)}
}
\value{
  A list of three items, the orderof the fit, the fitted
  curve (without NA's) and the R fit object
}
\description{
  fit the points given by pts, by a fourier seriers Allow
  the user to see the fit given by different orders and
  choose the order
}
\details{
  fit a fourer series of order \code{start_order} to pts.
  Display the fit with the raw data visually weighted (area
  of circles) by ws.  Allow the user to choose higher or
  lower fit orders or to accept the fit order given.
}

