\name{convert_data}
\alias{convert_data}
\title{Convert a data set to standard form}
\usage{
  convert_data(Data)
}
\arguments{
  \item{Data}{a dataset}
}
\value{
  A standard data set (every year has DOY 61 March 1)
}
\description{
  Convert a data set to standard form
}
\note{
  This function does little but add DOY 60 to non_leap
  years
}

