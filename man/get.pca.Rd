% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.pca.R
\name{get.pca}
\alias{get.pca}
\title{Derive class}
\usage{
get.pca(population, path = NULL, database = NULL, gen = NULL,
  cohorts = NULL, coloring = "group", components = c(1, 2))
}
\arguments{
\item{population}{Population list}

\item{path}{Location were to save the PCA-plot}

\item{database}{Groups of individuals to consider for the export}

\item{gen}{Quick-insert for database (vector of all generations to export)}

\item{cohorts}{Quick-insert for database (vector of names of cohorts to export)}

\item{coloring}{Coloring by "group", "sex", "plain"}

\item{components}{Default: c(1,2) for the first two principle components}
}
\value{
Genotype data for in gen/database/cohorts selected individuals
}
\description{
Function to devide the class for each individual
}
\examples{
data(ex_pop)
get.pca(ex_pop, gen=2)
}
