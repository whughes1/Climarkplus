\name{make_model_from_fit_objects}
\alias{make_model_from_fit_objects}
\title{Make a model from fit objects}
\usage{
  make_model_from_fit_objects(fit_object_rainy,
    fit_object_amount)
}
\arguments{
  \item{fit_object_rainy}{a fit object that gives the
  probability that a day is rainy}

  \item{fit_object_amount}{A fit object that give the
  amount of rain on a rainy day}
}
\value{
  a model suitable as a parameter to
  synth_data_set_from_model
}
\description{
  Make a model from fit objects
}
\details{
  The underlying functions use some r string maniplation
  which this function evaluates as R code to compute the
  output of the fits.  As yet this only handles stuff
  dependent only on DOY
}

