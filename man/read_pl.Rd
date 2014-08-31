\name{read_pl}
\alias{read_pl}
\title{Read a list of parameters of the form <key>=value}
\usage{
  read_pl(file = "filename", text = NULL)
}
\arguments{
  \item{file}{The filename of the parameter file}
}
\value{
  a list of the form list[key]=value (value a string)
}
\description{
  Read a list of parameters of the form <key>=value
}
\details{
  Only lines consisting of <key>=value are parsed, other
  lines are ignored.
}

