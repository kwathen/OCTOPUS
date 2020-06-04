##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/InitializePatientList.R}{View Code on GitHub} }
#' @export
InitializePatientList   <- function( cTrialDesign )
{
    UseMethod( "InitializePatientList", cTrialDesign )

}

#' @name InitializePatientList
#' @title InitializePatientList
#' @description{ This function will create a list to store information about the current patients in the trial.
#' The list will contain the following elements:
#'       vStartTimes        The start times for each patient in the trial,
#'       vTrt               Vector of treatment labels in the platform
#'       vISA               Vector of ISA labels in the trial,
#'       lPatOut            patient outcomes as a list (see more below),
#'       vCurrentQtyPatsISA The number of patients in each ISA,
#'       vTrtLab            The treatment labeled, taken directly from the cTrialDesign,
#'       vISALab            The ISA labels, taken directly from the cTrialDesign
#'       vQtyPatsArmISA     Number of patients in each arm in each ISA
#'
#'       lPatOut            A list of patient outcomes.  The list will contain a
#'                          mOutXX structure for each outcome where XX is the outcome number
#'                          vObsTimeXX The observed time for each outcome.
#'                          The class( mOutXX ) should be the same as the vISAAnalysis[[ XX ]]
#'
#'      For multiple ISAs, the first ISA is used to determine how to process the outcome data. Specifically,
#'      the class elements are copied from the first ISA analysis object vISAAnalysis.  That is,
#'      the class of mOutXX is class( cTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[ XX ]] )
#'
#'
#' The returned list will not have any patients in it but will be initialized. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/InitializePatientList.R}{View Code on GitHub} }
#' @export
InitializePatientList.default   <- function( cTrialDesign )
{

    vTrtLab               <- cTrialDesign$vTrtLab
    vISALab               <- cTrialDesign$vISALab
    nQtyISA               <- cTrialDesign$nQtyISAs
    vCurrentQtyPatsISA    <- rep( 0, nQtyISA )

    cEnrolledPats         <- list( lPatOut            = NULL,
                                   vTrtLab            = vTrtLab,
                                   vISALab            = vISALab,
                                   vCurrentQtyPatsISA = vCurrentQtyPatsISA,
                                   vQtyPatsArmISA     = rep(0, length( cTrialDesign$vTrtLab) ) )

    lPatOut <- list()
    iISA <-1

    for( iISA in 1:nQtyISA )
    {
        lPats <-list()

        vISAAnalysis <- cTrialDesign$cISADesigns[[iISA]]$cISAAnalysis$vAnalysis

        for(i in 1:length( vISAAnalysis ) )
        {
            lPats[[paste("mOut", i, sep="")]] <- structure( list())
            class( lPats [[paste("mOut", i, sep="")]] ) <- class( vISAAnalysis[[i]] )


            strObsTime <- paste( "vObsTime", i, sep="")
            lPats[[strObsTime]] <- vISAAnalysis[[i]]$vObsTime
            #print( paste("Obs time", paste(  vISAAnalysis[[i]]$vObsTime, collapse =", ")  ))

        }
        lPats$nQtyOut      <- length( vISAAnalysis )
        lPats$vStartTimes  <- vector()
        lPats$vTrt         <- vector()

        strISA <- paste( "lISA", iISA, sep="")
        lPatOut[[ strISA ]] <- lPats
    }


    cEnrolledPats$lPatOut <- lPatOut

    class( cEnrolledPats ) <- class( cTrialDesign$cISADesigns)



    return( cEnrolledPats   )
}

InitializePatientList.SingleISA   <- function( cTrialDesign )
{
    #print( paste( "Init list for Single ISA"))
    vTrtLab               <- cTrialDesign$vTrtLab
    vISALab               <- cTrialDesign$vISALab
    nQtyISA               <- 1
    vCurrentQtyPatsISA    <- c(0)

    cEnrolledPats         <- list( lPatOut            = NULL,
                                   vTrtLab            = vTrtLab,
                                   vISALab            = vISALab,
                                   vCurrentQtyPatsISA = vCurrentQtyPatsISA,
                                   vQtyPatsArmISA     = rep(0, length( cTrialDesign$vTrtLab) ) )

    lPatOut <- list()
    iISA <-1

    #for( iISA in 1:nQtyISA )
    #{
        lPats <-list()

        vISAAnalysis <- cTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis

        for(i in 1:length( vISAAnalysis ) )
        {
            lPats[[paste("mOut", i, sep="")]] <- structure( list())
            class( lPats [[paste("mOut", i, sep="")]] ) <- class( vISAAnalysis[[i]] )


            strObsTime <- paste( "vObsTime", i, sep="")
            lPats[[strObsTime]] <- vISAAnalysis[[i]]$vObsTime
            #print( paste("Obs time", paste(  vISAAnalysis[[i]]$vObsTime, collapse =", ")  ))

        }
        lPats$nQtyOut      <- length( vISAAnalysis )
        lPats$vStartTimes  <- vector()
        lPats$vTrt         <- vector()

        strISA <- "lISA1"
        lPatOut[[ strISA ]] <- lPats
    #}


    cEnrolledPats$lPatOut <- lPatOut

    class( cEnrolledPats ) <- class( cTrialDesign$cISADesigns)



    return( cEnrolledPats   )
}
