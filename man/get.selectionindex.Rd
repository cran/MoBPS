% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.selectionindex.R
\name{get.selectionindex}
\alias{get.selectionindex}
\title{Export underlying last used selection index}
\usage{
get.selectionindex(
  population,
  database = NULL,
  gen = NULL,
  cohorts = NULL,
  use.id = FALSE
)
}
\arguments{
\item{population}{Population list}

\item{database}{Groups of individuals to consider for the export}

\item{gen}{Quick-insert for database (vector of all generations to export)}

\item{cohorts}{Quick-insert for database (vector of names of cohorts to export)}

\item{use.id}{Set to TRUE to use MoBPS ids instead of Sex_Nr_Gen based names (default: FALSE)}
}
\value{
Last applied selection index for in gen/database/cohorts selected individuals
}
\description{
Function to export last used selection index (mostly relevant for Miesenberger 1997 stuff)
}
\examples{
data(ex_pop)
get.selectionindex(ex_pop, gen=2)
}
