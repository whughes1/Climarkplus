\name{split_by_year}
\alias{split_by_year}
\title{split a dataset into a list of datasets, indexed by year}
\usage{
  split_by_year(data, year_begins_in_july = FALSE)
}
\arguments{
  \item{data}{a dataset with column \code{Date}}

  \item{year_begins_in_july}{If we have to add
  \code{mod_year} when to begin the year}
}
\description{
  split a dataset into a list of datasets, indexed by year
}
\details{
  A convenience function.  First check if column
  \code{mod_year} exists and if not call \code{add_dmy} to
  get it.  Then spit datasets into a list of datasets.  The
  names of the elements of the list will be the years
}

