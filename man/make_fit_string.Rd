\name{make_fit_string}
\alias{make_fit_string}
\title{Make a string to pass to fitting code
Use a parameter \code{.pl} file to guide the fitting
process}
\usage{
  make_fit_string(filename = NULL, others = NULL,
    other_model_string = NULL)
}
\arguments{
  \item{filename}{The name of the parameter file}

  \item{others}{The name of other files in the data that
  are included as simple (no interaction) parameters}

  \item{other_model_string}{You can put anything you want
  here and it will be passed to the fit function.  Note, if
  this is not NULL then you will get a fit that cannot be
  used to make a model for the simulator}
}
\description{
  Make a string to pass to fitting code Use a parameter
  \code{.pl} file to guide the fitting process
}
\details{
  <TBD>
}

