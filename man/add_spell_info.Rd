\name{add_spell_info}
\alias{add_spell_info}
\title{Add spell info to dataset}
\usage{
  add_spell_info(dataset)
}
\arguments{
  \item{dataset}{A standard dataset with \code{DOY} and
  \code{wet_or_dry} solums}
}
\value{
  A standard data set with columns \code{spell_length} and
  \code{First DOY}
}
\description{
  Add spell info to dataset
}
\details{
  A spell consists of consecutive days that are wet,dry or
  unkonwn. Spells can cross year boundaries.  For each day,
  the length of the current spell is in
  \code{spell_length}and the day it started is in
  \code{First DOY}.
}
\note{
  Leap years are handled correctly
}

