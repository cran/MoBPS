'#
  Authors
Torsten Pook, torsten.pook@uni-goettingen.de

Copyright (C) 2017 -- 2020  Torsten Pook

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
'#

#' BV standardization
#'
#' Function to get mean and genetic variance of a trait to a fixed value
#' @param population Population list
#' @param mean.target Target mean
#' @param var.target Target variance
#' @param database Groups of individuals to consider for the export
#' @param gen Quick-insert for database (vector of all generations to export)
#' @param cohorts Quick-insert for database (vector of names of cohorts to export)
#' @param adapt.bve Modify previous breeding value estimations by scaling (default: FALSE)
#' @param adapt.pheno Modify previous phenotypes by scaling (default: FALSE)
#' @param verbose Set to TRUE to display prints
#' @examples
#' population <- creating.diploid(nsnp=1000, nindi=100, n.additive=100)
#' population <- bv.standardization(population, mean.target=200, var.target=5)
#' @return Population-list with scaled QTL-effects
#' @export



bv.standardization <- function(population, mean.target=100, var.target=10, gen=NULL, database=NULL, cohorts=NULL,
                               adapt.bve=FALSE, adapt.pheno=FALSE, verbose=FALSE){

  n_traits <- population$info$bv.nr

  modi1 <- rep(1, n_traits)
  modi2 <- population$info$base.bv

  if(length(mean.target)<n_traits) mean.target <- rep(mean.target, length.out = n_traits)
  if(length(var.target)<n_traits) var.target <- rep(var.target, length.out = n_traits)
  if(length(gen)==0 && length(database)==0 && length(cohorts)==0){
    gen <- nrow(population$info$size)
  }
  database <- get.database(population, gen, database, cohorts)
  ## Variance Standardization
  for(index in 1:n_traits){
    new_var <- var.target[index]

    if(population$info$bv.calculated==FALSE){
      population <- breeding.diploid(population, verbose=verbose)
    }

    var_test <- stats::var(get.bv(population, database= database)[index,])
    test1 <- TRUE
    if(length(population$info$real.bv.add[[index]])>0){
      population$info$real.bv.add[[index]][,3:5] <- population$info$real.bv.add[[index]][,3:5] * sqrt(  new_var / var_test)
      test1 <- FALSE
    }
    if(length(population$info$real.bv.mult[[index]])>0){
      population$info$real.bv.mult[[index]][,5:13] <- population$info$real.bv.mult[[index]][,5:13] * sqrt(  new_var / var_test)
      test1 <- FALSE
    }
    modi1[index] <- sqrt(new_var / var_test)

    if(test1 && verbose) cat("You entered a trait without quantitative loci. Is this intentional?\n")

  }

  for(gen in 1:length(population$breeding)){
    for(sex in 1:2){
      if(length(population$breeding[[gen]][[sex+6]])>0){
        population$breeding[[gen]][[sex+6]] <- (((population$breeding[[gen]][[sex+6]] - modi2) * modi1) + modi2)
      }
    }
  }

  ## Mean Standardization
  for(index in 1:n_traits){

    if(population$info$bv.calculated==FALSE){
      population <- breeding.diploid(population, verbose=FALSE)
    }

    mean_test <- mean(get.bv(population, database = database)[index,])

    population$info$base.bv[index] <- mean.target[index] + population$info$base.bv[index] - mean_test

  }

  for(gen in 1:length(population$breeding)){
    for(sex in 1:2){
      if(length(population$breeding[[gen]][[sex+6]])>0){
        population$breeding[[gen]][[sex+6]] <- population$breeding[[gen]][[sex+6]] - modi2 + population$info$base.bv
      }
    }
  }


  if(adapt.bve){
    for(gen in 1:length(population$breeding)){
      for(sex in 1:2){
        activ <- (population$breeding[[gen]][[sex+2]]!= 0)
        if(sum(activ)>0){
          population$breeding[[gen]][[sex+2]][activ] <- (((population$breeding[[gen]][[sex+2]] - modi2) * modi1) + population$info$base.bv)[activ]
        }
      }
    }
  }
  if(adapt.pheno){
    for(gen in 1:length(population$breeding)){
      for(sex in 1:2){
        activ <- !is.na(population$breeding[[gen]][[sex+8]]) & (population$breeding[[gen]][[sex+8]]!= 0) & (!population$info$phenotypic.transform)

        if(sum(activ)>0){
          population$breeding[[gen]][[sex+8]][activ] <- (((population$breeding[[gen]][[sex+8]] - modi2) * modi1) + population$info$base.bv)[activ]
        }
      }
    }
  }
  return(population)
}
