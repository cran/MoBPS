% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.age.point.R
\name{get.age.point}
\alias{get.age.point}
\title{Derive age point}
\usage{
get.age.point(
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
Time point selected gen/database/cohorts-individuals are born
}
\description{
Function to devide age point for each individual (Same as time.point unless copy.individual is used for aging)
}
\examples{
data(ex_pop)
get.age.point(ex_pop, gen=2)
}
