##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


##### TODO(Kyle) - This file need more documentation

#############################################################################################################################.
#  This function calls SimPatientOutcomes for each ISA.
#############################################################################################################################.
#' @export
SimulateAllPatientOutcomes <- function( cScen,  cTrialDesign )
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

#The cSimOutcomes is a class of type Independent or Correlated
# It should contain a list of outcomes to simulate
#
# 2 possibilities - 1) 1 outcome, 2) more than 1 outcome
# Option 1) 1 outcome
#  cSimOutcomes <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(1,2,3)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#
# Option 2) m outcome - mMeanNull1
#  cSimOutcome1 <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(1,2,3)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#  cSimOutcome2 <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(4,5,6)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#  cSimOutcomes <- structure( list( cSimOutcome1 = cSimOutcome1,
#                                   cSimOutcome2 = cSimOutcome2), class= "Correlated")
##
#
#
#

#' @export
SimPatientOutcomes <- function( cSimOutcomes, cISADesign )
{
    UseMethod( "SimPatientOutcomes", cSimOutcomes )
}

#' @export
SimPatientOutcomes.default <- function( cSimOutcomes,  cISADesign )
{
    stop(print("ERROR: The default sim patient outcomes is not defined.", class( cSimOutcomes)))
}


#' @name SimPatientOutcomes.Independent
#' @title SimPatientOutcomes
#' @description  The cSimOutcomes object has a list of sim outcomes and this version will go through and call the
#' SimPatientOutcomes for each type.  Each outcome is simulated independently.
#' @export
SimPatientOutcomes.Independent <- function( cSimOutcomes, cISADesign )
{
    vQtyPats        <-  cISADesign$vQtyPats
    #print( "SimPatientOutcomes.Independent")

    #Loop through the outcomes and simulate each one
    nQtyOutcomes    <- length( cSimOutcomes )
    iOut            <- 1
    lSimDataRet     <- list()            #Each simulated outcome matrix will be added to the list with names mSimOut1, mSimOut2...
    repeat
    {

        lTmpSimOut  <- cSimOutcomes[[ iOut ]] #GetSimOutome( lScen, iOut )
        lSimOut     <- SimPatientOutcomes( lTmpSimOut, cISADesign )
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

    lSimDataRet$nQtyOut  <- nQtyOutcomes
    lSimDataRet$vPatTrt  <- rep( cISADesign$vTrtLab, cISADesign$vQtyPats )
    lSimDataRet$vPatISA  <- rep( cISADesign$vISA, cISADesign$vQtyPats )
    return( lSimDataRet  )

}



#This version will assume that lSimOutcomes1 contains the info for simulating all outcomes as a MVN.
#Each lSimOutcome should contain nIndex that gives the columns from the MVN to select for that outcome
#SimulateCorrelatedPatientData <- function( lScen, lRandInfo, bRandomize = TRUE )
#' @name SimPatientOutcomes.Correlated
#' @title SimPatientOutcomes
#' @description  The cSimOutcomes object has a list of sim outcomes and this version will utilize the
#' first outcome to simulate all outcomes correlated.  Each simoutcome must have an attribute vColIndex that
#' specifies which columns out of the matrix will be used for that outcome.
#' @export
SimPatientOutcomes.Correlated <- function( cSimOutcomes, cISADesign)
{
    #print( "SimPatientOutcomes.Correlated")
    nQtyOutcomes    <- length( cSimOutcomes )
    lSimDataRet     <- list()            #Each simulated outcome matrix will be added to the list with names mSimOut1, mSimOut2...

    #The version assumes the first outcome has the mean vector and var-cov matrix for all outcome, then supplies a vIndx for each outcome to know
    #which columns to get off of the matrix

    lTmpSimOut  <- cSimOutcomes[[ 1 ]] #GetSimOutome( lScen, iOut )
    #print( paste("Sim class", class( lTmpSimOut)))
    lSimOut     <- SimPatientOutcomes( lTmpSimOut, cISADesign  )
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






