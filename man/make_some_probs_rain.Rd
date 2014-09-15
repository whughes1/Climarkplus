\name{make_some_probs_rain}
\alias{make_some_probs_rain}
\title{make a file giving the probabilities of a rainy day
and the mean and standard deviation of the rainfall
given that the day is rainy for each lag up to order}
\usage{
  make_some_probs_rain(data, order = 2)
}
\arguments{
  \item{data}{A standard data set}

  \item{order}{The order of the lags to calcualte}
}
\value{
  A file with 366 rows and 2 + 3*(2^order) columns giving
  the mean and deviation of rainfall on rainy days and for
  each lag, the number of rainy days
}
\description{
  make a file giving the probabilities of a rainy day and
  the mean and standard deviation of the rainfall given
  that the day is rainy for each lag up to order
}
\note{
  standard deviation is no longer used and may be removed
}

