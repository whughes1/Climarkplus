\name{model_util}
\alias{avg_daily_rainfall}
\alias{avg_day_rain_in_year}
\alias{avg_yearly_rainfall}
\alias{make_stats_object}
\alias{model_util}
\alias{sd_daily_rainfall}
\alias{sd_yearly_rainfall}
\alias{spell_dist}
\title{Model Utilities}
\usage{
  avg_daily_rainfall(dta, plot = FALSE, mask = NULL)

  sd_daily_rainfall(dta, plot = FALSE, mask = NULL)

  avg_yearly_rainfall(dta, plot = FALSE, mask = NULL)

  sd_yearly_rainfall(dta, plot = FALSE, mask = NULL)

  avg_day_rain_in_year(dta, plot = FALSE, mask = NULL)

  spell_dist(dta, plot = FALSE, mask = NULL, spell = "w")

  make_stats_object(dta, mask = NULL)
}
\arguments{
  \item{dta}{a standard data set}

  \item{mask}{a standard mask (of length 366)}

  \item{plot}{If true a very basic plot is made}

  \item{spell}{"w" for wet spells, "d" for dry spells}
}
\description{
  These utilities are to help examine and evaluate datasets
  and models.  Most have a plot utility.  However, this
  plot is very simple and is just meant as a sanity check.
  These utilities are very dependent on the properties of
  standard data sets and standard masks.
}

