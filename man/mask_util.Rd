\name{mask_util}
\alias{length_mask}
\alias{mask_plot}
\alias{mask_util}
\alias{mean_mask}
\alias{sd_mask}
\title{Mask Utilities}
\usage{
  mask_plot(mask, yl = par("usr")[3], yu = par("usr")[4])

  mean_mask(x, mask = NULL, na.rm = F, xcol = "Rain")

  sd_mask(x, mask = NULL, na.rm = F, xcol = "Rain")

  length_mask(x, mask = NULL, xcol = "Rain")
}
\arguments{
  \item{mask}{a standard mask (see description)}

  \item{yl,yu}{The upper and lower y limits}

  \item{x}{a vector or a data set.  If a data set then xcol
  (default "Rain") is masked through x$DOY}
}
\description{
  A mask is a vector of length 366 containing 0s and 1's.
  The concept is that any observation on a day of year with
  mask value 0 is ignored (e.g. in fitting routines).  The
  idea is to mask out the dry season, but the mask can be
  used to mask out any time period (e.g. the month of
  February if for some reason the data from February is
  suspect, or to mask out everything but January and
  February if you want to focus on those two months).

  This file contains a number of simple utility functions
  that use a mask.  Note that many functions in this
  package take an optional mask parameter, in particular
  all functions in \code{\link{model_util}}.

  Most of the utilities and their parameters are self
  explanatory
}
\details{
  \code{mask_plot} can be called when a plot is open.  It
  puts slightly transparent gray where mask is 0 from yl to
  yu.
}
\note{
  There is nothing magic about 366.  The mask functions
  could be made more general (many already are)
}

