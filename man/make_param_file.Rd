\name{make_param_file}
\alias{make_param_file}
\title{Given a data set help determine a suitable parameter file
for fitting the model.  In particular for each lag we see
if the (possibly shifted) curve from a previous lag can
be used and for each lag used we deternine the number of Fourier
harmonics needed to adaquately model it}
\usage{
  make_param_file(data_set, max_order = 3,
    max_rain_order = 2, filename = NULLm, all_pbs = NULL)
}
\arguments{
  \item{data_set}{(must have Markov lags)}

  \item{max_order}{Check all lags up to max_order for
  fitting probability of rain}

  \item{max_rain_order}{As above but fitting the mean rain}

  \item{filename}{the parameter file to output to}

  \item{all_pbs}{This is calculated by the program but it
  is not fast.  When dealing multiple times with one data
  set it is worth computing this once and keepint it.}
}
\value{
  A parameter list, the same list that would be obtained by
  reading the output parameter file.
}
\description{
  Given a data set help determine a suitable parameter file
  for fitting the model.  In particular for each lag we see
  if the (possibly shifted) curve from a previous lag can
  be used and for each lag used we deternine the number of
  Fourier harmonics needed to adaquately model it
}
\details{
  For each lag we help determine if the (possibly shifted)
  curve from a previous lag can be used and for each lag
  used we deternine the number of Fourier harmonics needed
  to adaquately model it.  The quantities are written to a
  parameter file.
}

