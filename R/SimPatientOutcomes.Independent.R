##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


# ' @name SimPatientOutcomes.Independent
#' @title SimPatientOutcomes.Independent
#' @describeIn SimPatientOutcomes { The cSimOutcomes object has a list of sim outcomes and this version will go through and call the
#' SimPatientOutcomes for each type.  Each outcome is simulated independently. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimPatientOutcomes.Independent.R}{View Code on GitHub} }
#' @export
SimPatientOutcomes.Independent <- function( cSimOutcomes, cISADesign, dfPatCovISA )
{
    # if( !is.null( dfPatCovISA ) )
    #     stop( "SimPatientOutcomes.Independent is not designed to incorporate patient covariates and dfPatCovISA is not NULL.")
    vQtyPats        <-  cISADesign$vQtyPats
    #print( "SimPatientOutcomes.Independent")

    #Loop through the outcomes and simulate each one
    nQtyOutcomes    <- length( cSimOutcomes )
    iOut            <- 1
    lSimDataRet     <- list()            #Each simulated outcome matrix will be added to the list with names mSimOut1, mSimOut2...
    repeat
    {

        lTmpSimOut  <- cSimOutcomes[[ iOut ]] #GetSimOutome( lScen, iOut )
        lSimOut     <- SimPatientOutcomes( lTmpSimOut, cISADesign,  dfPatCovISA )
        #mSimOut     <- SimulatePatientOutcome( lScen$vQtyPats, lTmpSimOut )
        #mSimOut     <- mSimOut[ lRandInfo$vRndIndx, ]

        lSimDataRet[[paste("mSimOut", iOut, sep="")]] <- lSimOut[[1]]
        class( lSimDataRet[[paste("mSimOut", iOut, sep="")]] ) <- class( lTmpSimOut )


        strObsTime <- paste( "vObsTime", iOut, sep="")
        lSimDataRet[[strObsTime]] <- lTmpSimOut$vObsTime
        if( iOut == nQtyOutcomes )
            break
        iOut <- iOut + 1


    }

    if( !is.null( dfPatCovISA ) )
        lSimDataRet[names( dfPatCovISA) ] <- dfPatCovISA[names( dfPatCovISA)]

    lSimDataRet$nQtyOut  <- nQtyOutcomes
    lSimDataRet$vPatTrt  <- rep( cISADesign$vTrtLab, cISADesign$vQtyPats )
    lSimDataRet$vPatISA  <- rep( cISADesign$vISA, cISADesign$vQtyPats )
    return( lSimDataRet  )

}
