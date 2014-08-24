\name{compare_lags}
\alias{compare_lags}
\title{Compare  curves to see
when you need a higher order model}
\usage{
  compare_lags(all_pbs, first_lag = "d")
}
\arguments{
  \item{probs}{all raw probabilities}

  \item{first_lag}{The first case to try}
}
\value{
  none, this produces plots
}
\description{
  Compare curves to see when you need a higher order model
}
\details{
  You are asked for a lag (e.g d) The function makes new
  lags by appending d and w (eg. dd dw).  The three curves
  are plotted
}

