% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/json.simulation.R
\name{json.simulation}
\alias{json.simulation}
\title{Simulation of a breeding program based on a JSON-file from MoBPSweb}
\usage{
json.simulation(
  file = NULL,
  log = NULL,
  total = NULL,
  fast.mode = FALSE,
  progress.bars = FALSE,
  size.scaling = NULL,
  rep.max = 1,
  verbose = TRUE,
  miraculix.cores = NULL,
  miraculix.chol = NULL,
  skip.population = FALSE,
  time.check = FALSE,
  time.max = 7200,
  export.population = FALSE,
  export.gen = NULL,
  export.timepoint = NULL,
  fixed.generation.order = NULL
)
}
\arguments{
\item{file}{Path to a json-file generated by the user-interface}

\item{log}{Provide Path where to write a log-file of your simulation (or false to not write a log-file)}

\item{total}{Json-file imported via jsonlite::read_json}

\item{fast.mode}{Set to TRUE work on a small genome with few markers}

\item{progress.bars}{Set to TRUE to display progress bars}

\item{size.scaling}{Scale the size of nodes by this factor (especially for testing smaller examples)}

\item{rep.max}{Maximum number of repeats to use in fast.mode}

\item{verbose}{Set to FALSE to not display any prints}

\item{miraculix.cores}{Number of cores used in miraculix applications (default: 1)}

\item{miraculix.chol}{Set to FALSE to manually deactive the use of miraculix for any cholesky decompostion even though miraculix is actived}

\item{skip.population}{Set to TRUE to not execute breeding actions (only cost/time estimation will be performed)}

\item{time.check}{Set to TRUE to automatically check simulation run-time before executing breeding actions}

\item{time.max}{Maximum length of the simulation in seconds when time.check is active}

\item{export.population}{Path were to export the population to (at state selected in export.gen/timepoint)}

\item{export.gen}{Last generation to simulate before exporting population to file}

\item{export.timepoint}{Last timepoint to simulate before exporting population to file}

\item{fixed.generation.order}{Vector containing the order of cohorts to generate (Advanced // Testing Parameter!)}
}
\value{
Population-list
}
\description{
Function to simulate a breeding program based on a JSON-file from MoBPSweb
}
\examples{
data(ex_json)
\donttest{population <- json.simulation(total=ex_json)}
}
