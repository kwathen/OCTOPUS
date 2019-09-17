##### COPYRIGHT #############################################################################################################.
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#This file contains functions used to setting up design elements.

#############################################################.
#  Setup the trial design structure                      ####
#############################################################.

NewTrialDesign <- function( lISADesigns, strISARandomizer )
{
    nQtyISAs  <- length( lISADesigns )
    vISANames <- paste( "cISA", 1:nQtyISAs, sep="" )
    names( lISADesigns ) <- vISANames
    cISADesigns <- structure( lISADesigns )

    nMaxQtyPats      <- 0
    vMaxQtyPatsInISA <-  rep( 0, nQtyISAs )


    vTrtLab <- vector()   # Used to help keep track of the number of patients on
    vISALab <- vector()
    for( nISA in 1:nQtyISAs)
    {
        nMaxQtyPatsISA           <- sum( cISADesigns[[ nISA ]]$vQtyPats )
        nMaxQtyPats              <- nMaxQtyPats + nMaxQtyPatsISA

        vMaxQtyPatsInISA[ nISA ] <- nMaxQtyPatsISA
        vTrtLab                  <- c( vTrtLab, cISADesigns[[ nISA ]]$vTrtLab )
        vISALab                  <- c( vISALab, rep( nISA, length( cISADesigns[[ nISA ]]$vTrtLab ) ) )
    }


    cTrialDesign   <- structure( list(
        nQtyISAs          = nQtyISAs,
        nMaxQtyPats       = nMaxQtyPats,
        vMaxQtyPatsInISA  = vMaxQtyPatsInISA,
        vISALab           = vISALab,
        vTrtLab           = vTrtLab,
        cISADesigns       = cISADesigns ), class=strISARandomizer )
    return( cTrialDesign )

}

########################################################################.
# 1 Dose ISA returned
#   Arguments:
#       strBorrow: "AllControls" or "NoBorrowing"
#       strModel:  "BayesianNormalAR1", "BayesianNormal"
########################################################################.
Create1DoseISA <- function(  vQtyPats, vTrtLab, vPUpper, vPLower, dFinalPUpper,   dFinalPLower,
                             vObsTime,
                                 strBorrow = "AllControls", strModel = "BayesianNormalAR1",
                                 vMinQtyPats = c(-1), vMinFUTime = c(-1), dQtyMonthsBtwIA = 0
                             )
{
    dConvWeeksToMonths <- 12/52

    bNoIA              <- FALSE
    nMaxQtyPats        <- sum( vQtyPats )

    if( all(vMinQtyPats == -1))
    {
        vMinQtyPats        <- c( nMaxQtyPats, nMaxQtyPats )# nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped

        vMinFUTime         <- c(24 * dConvWeeksToMonths, 24 * dConvWeeksToMonths)
    }

    strBorrow          <- strBorrow
    strRandomizer      <- "EqualRandomizer"
    lDecisionOut       <- structure(list(strApproachIA = "default", strApproachFA="default"), class = "General")

    vObsTime            <- c( 0,  4,  8, 12, 16, 20, 24) * dConvWeeksToMonths

    #Outcome 1
    vAnalysisInfo1      <- c( strModel, "MAVOnly", "ProcessReptMeasChngBaseline" )

    dMAV1               <- 0.5


    bPlaceMinusTrt1     <- TRUE
    vObsTimeOut1        <- vObsTime

    cISA1Info <- NewISAInfo( vTrtLab,
                             vQtyPats,
                             vMinQtyPats,
                             vMinFUTime,
                             dQtyMonthsBtwIA,
                             strRandomizer,
                             lDecisionOut,
                             strBorrow )

    cISAInfo <- AddBayesianOutcome( cISAInfo      = cISA1Info,
                                    vAnalysisInfo = vAnalysisInfo1,
                                    vTrtLab       = vTrtLab,
                                    vObsTime      = vObsTimeOut1,
                                    dMAV          = dMAV1,
                                    vPUpper       = vPUpper,
                                    vPLower       = vPLower,
                                    dFinalPUpper  = dFinalPUpper,
                                    dFinalPLower  = dFinalPLower,
                                    bPlaceMinusTrt= bPlaceMinusTrt1 )
    return( cISAInfo )
}




###########################################################.
#  Create the basic ISA structure
###########################################################.
NewISAInfo <- function( vTrtLab,
                        vQtyPats,
                        vMinQtyPats,
                        vMinFUTime,
                        dQtyMonthsBtwIA,
                        strRandomizer,
                        lDecisionOut,
                        strBorrow)
{
    cISAAnalysis <- structure( list(   vAnalysis = list()), class=c(strBorrow))# ,  "Independent"))

    cISAInfo <- structure( list(  vQtyPats        = vQtyPats,
                                  vTrtLab         = vTrtLab,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                  lDecision       = lDecisionOut,
                                  cISAAnalysis    = cISAAnalysis ), class=strRandomizer )

    return( cISAInfo )

}

#############################################################.
#  Add an outcome to the cISAInfo                       #####.
#############################################################.
AddOutcome  <- function( cISAInfo ,
                         vAnalysisInfo,
                         vTrtLab,
                         vObsTime ,
                         dMAV,
                         dTV,
                         vLowerCI,
                         vUpperCI ,
                         dFinalLowerCI,
                         dFinalUpperCI,
                         bPlaceMinusTrt )
{
    #Analysis object for outcome 1
    cAnalysis <-  structure( list( dMAV          = dMAV,
                                   dTV           = dTV,
                                   vUpperCI      = vUpperCI,
                                   vLowerCI      = vLowerCI,
                                   dFinalLowerCI = dFinalLowerCI,
                                   dFinalUpperCI = dFinalUpperCI,
                                   bPlacMinusTrt = bPlaceMinusTrt,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab,
                                   vObsTime      = vObsTime),
                             class= vAnalysisInfo)



    cISAAnalysis <- cISAInfo$cISAAnalysis
    nOut <- length( cISAAnalysis$vAnalysis ) + 1

    cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
    cISAInfo$cISAAnalysis = cISAAnalysis
    return( cISAInfo )

}

##################################################################################.
#  Add an outcome to the cISAInfo - Based on a Bayesian analysis (eg vPUpper) #####.
##################################################################################.
AddBayesianOutcome  <- function( cISAInfo ,
                                 vAnalysisInfo,
                                 vTrtLab,
                                 vObsTime ,
                                 dMAV,
                                 vPUpper,
                                 vPLower,
                                 dFinalPUpper,
                                 dFinalPLower,
                                 bPlaceMinusTrt )
{
    #Analysis object for outcome 1
    cAnalysis <-  structure( list( dMAV          = dMAV,
                                   vPUpper       = vPUpper,
                                   vPLower       = vPLower,
                                   dFinalPUpper  = dFinalPUpper,
                                   dFinalPLower  = dFinalPLower,
                                   bPlacMinusTrt = bPlaceMinusTrt,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab,
                                   vObsTime      = vObsTime),
                             class= vAnalysisInfo)



    cISAAnalysis <- cISAInfo$cISAAnalysis
    nOut <- length( cISAAnalysis$vAnalysis ) + 1

    cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
    cISAInfo$cISAAnalysis = cISAAnalysis
    return( cISAInfo )

}
