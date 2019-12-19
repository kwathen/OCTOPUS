##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#This version will assume that lSimOutcomes1 contains the info for simulating all outcomes as a MVN.
#Each lSimOutcome should contain nIndex that gives the columns from the MVN to select for that outcome
#SimulateCorrelatedPatientData <- function( lScen, lRandInfo, bRandomize = TRUE )
# ' @name SimPatientOutcomes.Correlated
#' @title SimPatientOutcomes.Correlated
#' @describeIn SimPatientOutcomes { The cSimOutcomes object has a list of sim outcomes and this version will utilize the
#' first outcome to simulate all outcomes correlated.  Each simoutcome must have an attribute vColIndex that
#' specifies which columns out of the matrix will be used for that outcome. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimPatientOutcomes.Correlated.R}{View Code on GitHub} }
#' @export
SimPatientOutcomes.Correlated <- function( cSimOutcomes, cISADesign, dfPatCovISA )
{
    #if( !is.null( dfPatCovISA ) )
    #    stop( "SimPatientOutcomes.Correlated is not designed to incorporate patient covariates and dfPatCovISA is not NULL.")
    #print( "SimPatientOutcomes.Correlated")
    nQtyOutcomes    <- length( cSimOutcomes )
    lSimDataRet     <- list()            #Each simulated outcome matrix will be added to the list with names mSimOut1, mSimOut2...

    #The version assumes the first outcome has the mean vector and var-cov matrix for all outcome, then supplies a vIndx for each outcome to know
    #which columns to get off of the matrix

    lTmpSimOut  <- cSimOutcomes[[ 1 ]] #GetSimOutome( lScen, iOut )
    #print( paste("Sim class", class( lTmpSimOut)))
    lSimOut     <- SimPatientOutcomes( lTmpSimOut, cISADesign, dfPatCovISA  )
    mSimOut     <- lSimOut[[1]]

    #mSimOut     <- SimulatePatientOutcome( lScen$vQtyPats, lTmpSimOut )
    #mSimOut     <- mSimOut[ lRandInfo$vRndIndx, ]

    #Now loop through the outcomes and pull of the right indexes for each outcome
    iOut        <- 1
    repeat
    {

        lTmpSimOut  <- cSimOutcomes[[ iOut ]] #GetSimOutome( lScen, iOut )

        lSimDataRet[[paste("mSimOut", iOut, sep="")]] <- as.matrix( mSimOut[,lTmpSimOut$vColIndex ], ncol= length(lTmpSimOut$vColIndex))
        class( lSimDataRet[[paste("mSimOut", iOut, sep="")]] ) <- class( lTmpSimOut )

        strObsTime <- paste( "vObsTime", iOut, sep="")
        lSimDataRet[[strObsTime]] <- lTmpSimOut$vObsTime
        if( iOut == nQtyOutcomes )
            break
        iOut <- iOut + 1


    }
    lSimDataRet <- CopyCovariates( lSimDataRet, lSimOut )


    lSimDataRet$nQtyOut  <- nQtyOutcomes #lSimOut$nQtyOut
    lSimDataRet$vPatTrt  <- lSimOut$vPatTrt #rep( cTrialDesign$vTrtLab, cTrialDesign$vQtyPats )
    #lSimDataRet$vPatISA  <- lSimOut$vPatISA #rep( cTrialDesign$vISA, cTrialDesign$vQtyPats )

    return( lSimDataRet  )

}
