% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.pedmap.R
\name{get.pedmap}
\alias{get.pedmap}
\title{Generate plink-file (pedmap)}
\usage{
get.pedmap(
  population,
  path = NULL,
  database = NULL,
  gen = NULL,
  cohorts = NULL,
  non.genotyped.as.missing = FALSE,
  use.id = FALSE
)
}
\arguments{
\item{population}{Population list}

\item{path}{Location to save pedmap-file}

\item{database}{Groups of individuals to consider for the export}

\item{gen}{Quick-insert for database (vector of all generations to export)}

\item{cohorts}{Quick-insert for database (vector of names of cohorts to export)}

\item{non.genotyped.as.missing}{Set to TRUE to replaced non-genotyped entries with "./."}

\item{use.id}{Set to TRUE to use MoBPS ids instead of Sex_Nr_Gen based names}
}
\value{
Ped and map-file for in gen/database/cohorts selected individuals
}
\description{
Generate a ped and map file (PLINK format) for selected groups and chromosome
}
\examples{
data(ex_pop)
\donttest{
file_path <- tempdir()
get.pedmap(path=file_path, ex_pop, gen=2)
file.remove(paste0(file_path, ".ped"))
file.remove(paste0(file_path, ".map"))
}
}
