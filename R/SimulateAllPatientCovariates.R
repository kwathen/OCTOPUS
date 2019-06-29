##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name SimulateAllPatientCovariates
#' @title SimulateAllPatientCovariates
#' @description SimulateAllPatientCovariates {This function is used to simulate all patient covariates in the platform.  }
#' @export
SimulateAllPatientCovariates <- function( cSimCovariates,  cTrialDesign   )
{
    UseMethod( "SimulateAllPatientCovariates", cSimCovariates )
}

#' @name SimulateAllPatientCovariates
#' @title SimulateAllPatientCovariates
#' @description SimulateAllPatientCovariates {This function is used to simulate all patient covariates in the platform.
#' This function is generic in case there is a need to override but
#' is implemented such that it should be suitable for most cases.  Specifically,
#' this function calls SimulatePatientCovariates for each ISA. }
#' @return A list with length equal to the number of ISAs in the trial.   Each element represents the simulated covariates for each ISA.
#' @export
SimulateAllPatientCovariates.default <- function( cSimCovariates,  cTrialDesign   )
{
    nQtyPats = cTrialDesign$nMaxQtyPats
    nQtyCovs = length( cSimCovariates )
    iCov = 1
    lCovRet = list( )
    repeat
    {
        lCovRet[[ iCov ]] = SimulatePatientCovariates( cSimCovariates[[ iCov ]], nQtyPats  )

        if( iCov == nQtyCovs )
            break
        iCov = iCov + 1

    }
    return( lCovRet )
}

SimulateAllPatientCovariates.NULL <- function( cSimCovariates,  cTrialDesign   )
{

    return( NULL )
}
