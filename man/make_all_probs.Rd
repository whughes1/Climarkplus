\name{make_all_probs}
\alias{make_all_probs}
\title{make a file giving the probabilities of a rainy day
for all lags up to max_rainy_day_order
and the mean  of the rainfall
given that the day is rainy, both unconditional and
conditional on some rain pattern up to max_mean_rain_oder}
\usage{
  make_all_probs(data, max_rainy_day_order = 2,
    max_mean_rain_order = 1)
}
\arguments{
  \item{data}{the raw probability data}

  \item{max_rainy_day_order}{All lags up to this order are
  calculated to determine if the day is rainy}

  \item{max_mean_rain_order}{All lags up to this order are
  calculated to determine the mean of the amount of rain or
  rainy days}
}
\value{
  A file with 366 rows and a number of columns giving the
  probability of rain for a given lag, for each lag the
  number of days on which the probability is based, the
  mean and rainfall given that the day had rain, again for
  a number of lags and for each lag number of rainy days
}
\description{
  make a file giving the probabilities of a rainy day for
  all lags up to max_rainy_day_order and the mean of the
  rainfall given that the day is rainy, both unconditional
  and conditional on some rain pattern up to
  max_mean_rain_oder
}
\details{
  for the probability of a rainy day, take all lags up, to
  length max_rainy_day_order, for the mean of rain take all
  lags up to max_mean_rain_order
}
\note{
  can be a bit slow for large orders
}

