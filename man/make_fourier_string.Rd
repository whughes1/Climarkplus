\name{make_fourier_string}
\alias{make_fourier_string}
\title{Given the coefficients of a fit and a lag provide
a string suitable for computing fourier expansion
of the given lag}
\usage{
  make_fourier_string(cs, lag, is_rain = FALSE)
}
\arguments{
  \item{cs}{the coefficients, for form see details}

  \item{lag}{a string giving the lag to be found}
}
\description{
  Given the coefficients of a fit and a lag provide a
  string suitable for computing fourier expansion of the
  given lag
}
\details{
  This is a very specific function as it will only work
  with the coefficients of the fits provided by
  \code{fit_rainy} and \code{fit_amounts}.  It is delicate
  as any change in the output format of glm will break it.
  It depends on the name of the Fourier coefficient for lag
  having the form I((ULAGS[lag]) * Fourier function)
}

