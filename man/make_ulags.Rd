\name{make_ulags}
\alias{make_ulags}
\title{A simple utility to make a vector of the
lag actually used for a given line}
\usage{
  make_ulags(wms, filename = NULL, is_rain = FALSE)
}
\arguments{
  \item{wms}{a standard data set}

  \item{filename}{a parameter file}

  \item{is_rain}{names slightly different if called for
  rain lags}
}
\value{
  A vector of the lags actually used
}
\description{
  A simple utility to make a vector of the lag actually
  used for a given line
}
\note{
  No error checking is done
}

