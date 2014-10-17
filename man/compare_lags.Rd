\name{compare_lags}
\alias{compare_lags}
\title{Compare  curves to see
when you need a higher order model}
\usage{
  compare_lags(all_pbs, search_lag = "dd", is_rain = FALSE,
    mask = NULL)
}
\arguments{
  \item{probs}{all raw probabilities}

  \item{search}{lag Try all the prefixes of search lag to
  see if any can be used (perhaps with an offset)}

  \item{is_rain}{True if we are looking at mean rainfall}

  \item{mask}{a standard mask see \code{\link{mask_util}}}
}
\value{
  The lag to use and the offset
}
\description{
  Compare curves to see when you need a higher order model
}
\details{
  For every prefix of the search lag (e.g search_lag =
  "ddw" prefixes = "d". "dd", "ddw") fit the prefix and the
  search lag to the data and compare the curves.  You can
  decide wether or not to accept the lag, perhaps with an
  offset.
}

