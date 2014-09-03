\name{get_fourier}
\alias{get_fourier}
\title{Determine the number of harmonics needed to adaquately
model the probabilities associated with lag}
\usage{
  get_fourier(probs, first_fit_order = 1, lag = "d",
    is_rain = FALSE, method = "bernoulli")
}
\arguments{
  \item{probs}{the markov wet/dry probabilities and the
  rain mean}

  \item{first_fit_order}{the first fit order used}

  \item{lag}{the lag to be fit}

  \item{is_rain}{TRUE if the fit is to mean rain}

  \item{the}{fitting method to use}
}
\value{
  the order used to fit lag
}
\description{
  Determine the number of harmonics needed to adaquately
  model the probabilities associated with lag
}
\details{
  The function detemines the name of the column of data
  corresponding to the lag or rain lag.  This column is
  fitted by finite Fourier functions in the interactive
  function \code{fit_probs_inter}.
}

