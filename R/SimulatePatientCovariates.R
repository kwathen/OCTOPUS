##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name SimulatePatientCovariates
#' @title SimulatePatientCovariates
#' @description SimulatePatientCovariates {This function is used to simulate the patient covariate values.
#' This function is intended to do one covariate for all patients.  }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulatePatientCovariates.R}{View Code on GitHub} }
#' @export
SimulatePatientCovariates <- function(  cSimCovariate,  nQtyOfPatients   )
{
    UseMethod( "SimulatePatientCovariates", cSimCovariate )
}


#' @title SimulatePatientCovariates.default
#' @describeIn SimulatePatientCovariates  { This function is used to simulate the patient covariate values.
#' This function is intended to do one covariate, for all patients.
#' Because several options are provided and there in no well defined default
#' an stop error occurs if you call the default method. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulatePatientCovariates.R}{View Code on GitHub} }
#' @export
SimulatePatientCovariates.default <- function(  cSimCovariate,  nQtyOfPatients )
{
    stop(print("ERROR: The default SimulatePatientCovariates is not defined, the provided object has class = ", class( cSimCovariate )))
}
