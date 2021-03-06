\name{make_some_probs}
\alias{make_some_probs}
\title{make a file giving the probabilities of a rainy day
with lags up to oder}
\usage{
  make_some_probs(data, order = 2)
}
\arguments{
  \item{order}{The order of the lags to calculate}
}
\value{
  A file with 366 rows and 2^(order+1) columns giving the
  probability of rain given a certain lag, and the number
  of times this lag shows up
}
\description{
  make a file giving the probabilities of a rainy day with
  lags up to oder
}

