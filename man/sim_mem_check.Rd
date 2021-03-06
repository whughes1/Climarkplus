\name{sim_mem_check}
\alias{sim_mem_check}
\title{Given a mixed markov model set, synthesize a data set}
\usage{
  sim_mem_check(model, start_year = 1970, num_years = 10,
    r_seed = NULL, start_string = NULL,
    rain_start_string = NULL, label = NULL, thresh = 0.12)
}
\arguments{
  \item{model}{a standard model}

  \item{start_year}{The year to start the synthetic data}

  \item{r_seed}{if non null, the random seed to use}

  \item{num_years}{number of years to synthesize}

  \item{start_string}{The initial conditons (e.g. wdwdw)}

  \item{label}{The "station name"}
}
\value{
  A standard data set
}
\description{
  Given a mixed markov model set, synthesize a data set
}
\details{
  The synthesizer gets the probablity of rain (given the
  previous days pattern of w and d) and determines if the
  day in rainy.  If so it then determined how much rain
  will fall (using a gamma distribution).  Currently it
  only uses stuff that is dependent on DOY and assumes that
  the numbers in the model represent probablities.
}
\note{
  The function takes about 90 seconds to synthesize 1000
  years of data. Thus producing 1000 data sets of 1000
  years each (usefull for bootstraping) will take about 25
  hours.
}

