\name{fdgs}
\alias{fdgs}
\title{Given a year of data, determine the first
Day of the growing season}
\usage{
  fdgs(year_data, n_total = 25, over = 2, span = 30,
    dry_length = 6, type = 1)
}
\arguments{
  \item{year_data}{standard data for a single year (must
  have wet_or_dry column)}

  \item{n_total}{total amount of rainfall (see details)}

  \item{over}{over how many days(see details)}

  \item{span}{length in which there is no dry spell(see
  details)}

  \item{dry_length}{length of maximum dry spell(see
  details)}

  \item{type}{1 or 2 (see details)}
}
\value{
  The day or year of the start of the growing season.
}
\description{
  Given a year of data, determine the first Day of the
  growing season
}
\details{
  Two definitons are used, \code{type} 1 and 2:

  1: the first day of a rainy period in which there is
  \code{n_total} millimetres of rain over \code{over} days

  2: the first day of a period in which there is
  \code{n_total} millimetres of rain over \code{over} days
  and there is no dry spell of length {dry_spell} in the
  next \code{span} days
}
\note{
  The function is robust against \code{NA}'s, and partial
  or mixed data but may give bad results
}

