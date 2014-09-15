\name{make_probs}
\alias{make_probs}
\title{make a file giving the probabilities of a rainy day
and the mean and standard deviation of the rainfall
given that the day is rainy,}
\usage{
  make_probs(data, order = 2, thresh = 0.12)
}
\value{
  A file with 366 rows and 2^(order+1)+3 columns giving the
  probability of rain for a given lag, the number of days
  on which the probability is based, for mean rainfall,
  standard deviation of rainfall and number of rainy days
}
\description{
  make a file giving the probabilities of a rainy day and
  the mean and standard deviation of the rainfall given
  that the day is rainy,
}
\note{
  obsolete, use make_all_probs
}

