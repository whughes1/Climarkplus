\name{add_markov}
\alias{add_markov}
\title{Add makov columns up to order to a standard data set}
\usage{
  add_markov(data, order = 2, thresh = 0.12)
}
\arguments{
  \item{data}{a standard data set}

  \item{order}{calculate lags to this order}

  \item{thresh}{threshold of rain for a wet day}
}
\value{
  A standard data set with columns "wet_or_dry" and Lag_1
  Lag_2 ... Lag_order
}
\description{
  Add makov columns up to order to a standard data set
}

