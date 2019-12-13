##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name SimulateAllPatientOutcomes
#' @title SimulateAllPatientOutcomes
#' @description SimulateAllPatientOutcomes {This function is used to simulate all patient outcomes in the platform.}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateAllPatientOutcomes.R}{View Code on GitHub} }
#' @export
SimulateAllPatientOutcomes <- function( cScen,  cTrialDesign, dfPatCov  )
{
    UseMethod( "SimulateAllPatientOutcomes", cScen )
}


#' @title SimulateAllPatientOutcomes.default
#' @describeIn SimulateAllPatientOutcomes { This function is used to simulate all patient outcomes in the platform.
#' This function is generic in case there is a need to override but
#' is implemented such that it should be suitable for most cases.  Specifically,
#' this function calls SimPatientOutcomes for each ISA. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateAllPatientOutcomes.R}{View Code on GitHub} }
#' @export
SimulateAllPatientOutcomes.default <- function( cScen,  cTrialDesign, dfPatCov )
{
    nQtyISAs <- cTrialDesign$nQtyISAs
    iISA     <- 1
    lSimOutcomes <- list( )
    #Loop over ISAs,
    repeat
    {
        if( is.null( dfPatCov) == FALSE )
        {
            #if lPatCov != NULL then we need to sample the number of patients for that ISA from lPatCov to be sent to the SimPatientOutcomes
            #and remove them from lPatCov
            #TODO(Covs) - Currently RamdomySelectPatientCovariates select patients at random but when we add the ability to
            # have ISAs that do not enroll in all groups this will need to be modified to account for this
            nQtyPats     <- floor( nrow( dfPatCov )/nQtyISAs )
            lRet         <- RandomlySelectPatientCovariates( dfPatCov, nQtyPats  )
            dfPatCovISA  <- lRet$dfPatCovISA
            dfPatCov     <- lRet$dfPatCov
            vQtyPats     <- cTrialDesign$cISADesigns[[ iISA ]]$vQtyPats
            vQtyPats     <- floor( nQtyPats * vQtyPats/sum(vQtyPats) )
            cTrialDesign$cISADesigns[[ iISA ]]$vQtyPats <- vQtyPats  # Because this object is not returned to the caller this change only has a local scope
            if( sum( vQtyPats ) < nrow( dfPatCovISA ) )
            {
                dfPatCovISA <- dfPatCovISA[1:sum(vQtyPats), ]
            }
            lSimOutcomes[[ iISA ]]  <-  SimPatientOutcomes( cScen$cISADesigns[[ iISA ]]$cSimOutcomes, cTrialDesign$cISADesigns[[ iISA ]], dfPatCovISA = dfPatCovISA )


        }
        else
        {
            dfPatCovISA    <- NULL
            lSimOutcomes[[ iISA ]]  <-  SimPatientOutcomes( cScen$cISADesigns[[ iISA ]]$cSimOutcomes, cTrialDesign$cISADesigns[[ iISA ]], dfPatCovISA = dfPatCovISA )

        }


        if( iISA == nQtyISAs )
            break
        iISA <- iISA + 1
    }
    names( lSimOutcomes ) <- paste( "lISA", 1:nQtyISAs, sep="" )
    return( lSimOutcomes )


}

#Helper funciton

RandomlySelectPatientCovariates <- function( dfPatCov, nQtyPatsInISA )
{
    nQtyCov       <- ncol( dfPatCov )
    iCov          <- 1
    vIndx         <- sample.int( n = length( dfPatCov[ , 1 ] ), size = nQtyPatsInISA  )
    dfPatCovISA   <- dfPatCov[ vIndx, ]
    dfPatCov      <- dfPatCov[ -vIndx, ]
    #lPatCovISA    <- list()
    # repeat
    # {
    #     lPatCovISA[[ iCov ]] <- lPatCov[[ iCov ]][ vIndx ]      # Copy covariates to the ISA cov list
    #     lPatCov[[ iCov ]]    <- lPatCov[[ iCov ]][ -vIndx ]     # Remove the covaraites used for this ISA
    #     if( iCov == nQtyCov )
    #         break
    #     iCov <- iCov + 1
    # }
    #names( lPatCovISA) <- names( lPatCov )
    return( list( dfPatCov  = dfPatCov , dfPatCovISA = dfPatCovISA ) )

}
