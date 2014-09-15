\name{add_dmy}
\alias{add_dmy}
\title{Add columns with the Day, Month and "Year" to a standard
data set}
\usage{
  add_dmy(dataset, year_change_in_july = FALSE)
}
\arguments{
  \item{dataset}{a standard data set (may have extra
  columns, e.g. markov info)}

  \item{year_change_in_july}{usefull if growing season
  crosses years. July 1970 will have "Year" 1971}
}
\value{
  A standard data set with added columns for day, month and
  mod_year
}
\description{
  Add columns with the Day, Month and "Year" to a standard
  data set
}
\note{
  mod_year may not be the date year if the growing season
  crosses year boundaries.  We assume that either December
  or July is in the dry season
}

