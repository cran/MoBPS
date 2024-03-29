% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.array.R
\name{add.array}
\alias{add.array}
\title{Add a genotyping array}
\usage{
add.array(population, marker.included = TRUE, array.name = NULL)
}
\arguments{
\item{population}{population list}

\item{marker.included}{Vector with number of SNP entries coding if each marker is on the array (TRUE/FALSE)}

\item{array.name}{Name of the added array}
}
\value{
Population list
}
\description{
Function to add a genotyping array for the population
}
\examples{
data(ex_pop)
population <- add.array(ex_pop, marker.included = c(TRUE, FALSE), array.name="Half-density")
}
