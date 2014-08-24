\name{synth_data_set}
\alias{synth_data_set}
\title{Given a markov model, synthesize a data set}
\usage{
  synth_data_set(m_model, start_year = 1970,
    num_years = 10, r_seed = NULL, start_string = NULL,
    label = NULL, thresh = 0.12)
}
\arguments{
  \item{m_model}{a standard model}

  \item{start_year}{The year to start the synthetic data}

  \item{r_seed}{if non null, the random seed to use}

  \item{num_years}{number of years to synthesize}

  \item{start_string}{The initial conditons (e.g. wdwdw)}

  \item{label}{The "station name"}

  \item{thresh}{The rain threshold below which a day will
  be assumed not to have rain}
}
\value{
  A standard data set
}
\description{
  Given a markov model, synthesize a data set
}
\note{
  The fuction takes about 7 seconds to synthesize 100 years
  of data. Thus producing 1000 data sets of 1000 years each
  (usefull for bootstraping) will take about 20 hours.
}

