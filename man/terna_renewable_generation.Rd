% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/terna_renewable_generation.R
\name{terna_renewable_generation}
\alias{terna_renewable_generation}
\title{Get total load from the terna API.}
\usage{
terna_renewable_generation(
  date_from,
  date_to,
  type = c("Thermal", "Wind", "Geothermal", "Photovoltaic", "Self-consumption", "Hydro"),
  token = NULL
)
}
\arguments{
\item{date_from}{Date from}

\item{date_to}{Date to}

\item{type}{type.}

\item{token}{token}
}
\description{
Get total load from the terna API.
}
