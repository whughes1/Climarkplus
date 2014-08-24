\name{choose_order}
\alias{choose_order}
\title{Fitting probabilites to a fourier series of some order
determined by the drop in the deviance being above a threshold}
\usage{
  choose_order(probs, ws, thresh = 5, method = "bernoulli")
}
\arguments{
  \item{probs}{probability of success}

  \item{ws}{total number of successes and failures}

  \item{thresh}{The deviance threshold}

  \item{method}{the fitting method used, bernoulli or std}
}
\value{
  A list consisting of the fitted probabilities, (without
  NA's, the oder of the solution, and the solution)
}
\description{
  Fitting probabilites to a fourier series of some order
  determined by the drop in the deviance being above a
  threshold
}
\details{
  The fit order will be increases by one and the decrease
  in deviance checked.  This procedure will continue until
  the decrease in devience is less than the threshold.
  This seems to work well with the bernoulli method, but
  can be problematic with standard method.
}

