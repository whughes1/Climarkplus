\name{get_offset}
\alias{get_offset}
\title{Find the offset by which curve best fits data}
\usage{
  get_offset(data, data_ws = NULL, curve, curve_ws = NULL)
}
\arguments{
  \item{data}{the data points to be fitted}

  \item{data_ws}{weights for the data}

  \item{curve}{the "shape" that will be fitted}

  \item{curve_ws}{weights for the curve}
}
\value{
  A list consisting of the fitted fitted curve, (curve +
  intercept), and the intercept
}
\description{
  Find the offset by which curve best fits data
}
\details{
  The data is fitted by least squares to a constant model.
  the curve is used as an offset.  The weighting applied is
  the square root of the data weight times the curve
  weight. It is not necessary that the "shape" have mean 0.
}
\note{
  not currently used
}

