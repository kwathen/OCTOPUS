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
SimulateAllPatientOutcomes <- function( cScen,  cTrialDesign  )
{
    UseMethod( "SimulateAllPatientOutcomes", cScen )
}


#' @title SimulateAllPatientOutcomes.default
#' @describeIn SimulateAllPatientOutcomes { This function is used to simulate all patient outcomes in the platform.
#' This function is generic in case there is a need to override but
#' is implemented such that it should be suitable for most cases.  Specifically,
#' this function calls SimPatientOutcomes for each ISA. }
#' @export
SimulateAllPatientOutcomes.default <- function( cScen,  cTrialDesign )
{
    nQtyISAs <- cTrialDesign$nQtyISAs
    iISA     <- 1
    lSimOutcomes <- list( )
    repeat
    {

        lSimOutcomes[[ iISA ]]  <-  SimPatientOutcomes( cScen$cISADesigns[[ iISA ]]$cSimOutcomes, cTrialDesign$cISADesigns[[ iISA ]] )

        if( iISA == nQtyISAs )
            break
        iISA <- iISA + 1
    }
    return( lSimOutcomes )


}
