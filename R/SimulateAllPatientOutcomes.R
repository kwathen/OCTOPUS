##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name SimulateAllPatientOutcomes
#' @title SimulateAllPatientOutcomes
#' @description SimulateAllPatientOutcomes {This function is used to simulate all patient outcomes in the platform.}
#' @export
SimulateAllPatientOutcomes <- function( cScen,  cTrialDesign, lPatCov  )
{
    UseMethod( "SimulateAllPatientOutcomes", cScen )
}


#' @title SimulateAllPatientOutcomes.default
#' @describeIn SimulateAllPatientOutcomes { This function is used to simulate all patient outcomes in the platform.
#' This function is generic in case there is a need to override but
#' is implemented such that it should be suitable for most cases.  Specifically,
#' this function calls SimPatientOutcomes for each ISA. }
#' @export
SimulateAllPatientOutcomes.default <- function( cScen,  cTrialDesign, lPatCov )
{
    nQtyISAs <- cTrialDesign$nQtyISAs
    iISA     <- 1
    lSimOutcomes <- list( )
    #Loop over ISAs,
    repeat
    {
        if( is.null( lPatCov) == FALSE )
        {
            #if lPatCov != NULL then we need to sample the number of patients for that ISA from lPatCov to be sent to the SimPatientOutcomes
            #and remove them from lPatCov
            lRet        <- RandomlySelectPatientCovariates( lPatCov, cTrialDesign$vMaxQtyPatsInISA[ iISA ] )
            lPatCovISA  <- lRet$lPatCovISA
            lPatCov     <- lRet$lPatCov

        }
        else
        {
            lPatCovISA    <- NULL
        }

        lSimOutcomes[[ iISA ]]  <-  SimPatientOutcomes( cScen$cISADesigns[[ iISA ]]$cSimOutcomes, cTrialDesign$cISADesigns[[ iISA ]], lPatCovISA = lPatCovISA )

        if( iISA == nQtyISAs )
            break
        iISA <- iISA + 1
    }
    return( lSimOutcomes )


}

#Helper funciton

RandomlySelectPatientCovariates <- function( lPatCov, nQtyPats )
{
    nQtyCov       <- length( lPatCov )
    iCov          <- 1
    nQtyPatsInISA <- cTrialDesign$vMaxQtyPatsInISA[ iISA ]
    vIndx         <- sample.int( n = length( lPatCov[[ 1 ]] ), size = nQtyPatsInISA  )

    repeat
    {
        lPatCovISA[[ iCov ]] <- lPatCov[[ iCov ]][ vIndx ]      # Copy covariates to the ISA cov list
        lPatCov[[ iCov ]]    <- lPatCov[[ iCov ]][ -vIndx ]     # Remove the covaraites used for this ISA
        if( iCov == nQtyCov )
            break
        iCov <- iCov + 1
    }
    return( list( lPatCov = lPatCov, lPatCovISA = lPatCovISA) )

}
