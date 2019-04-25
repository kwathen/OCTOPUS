##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name CreateDataSetForAnalysis
#' @title CreateDataSetForAnalysis
#' @description {This function is used to created the dataset for analysis.  }
#' @export
CreateDataSetForAnalysis <- function( cPats,  dCurrentTime, vISAStatus  )
{
    UseMethod( "CreateDataSetForAnalysis", cPats )

}

#cPats <- cEnrolledPats, dCurrentTime

#' @name CreateDataSetForAnalysis.default
#' @title CreateDataSetForAnalysis.default
#' @description {This function creates a dataset for analysis based on the current time (dCurrentTime)
#' and returns an object with class = "TrialData" that has a list with an element for each ISA data.
#' Before creating the dataset the ProcessData to create the version of the data needed, eg change from baseline,
#' and also coppies covariates that are present.}
#' @param cPats The object of patient data to create the data for analysis from.
#' @param dCurrentTime The current time in the trial.   In each simulated trial this will be the time at which
#' the analysis is created.
#' @param vISAStatus A vector with one element for each ISA.  If the status of the ISA != 0 then a dataset
#' is created, if the status = 0 for an ISA the dataset will not be created.
#' @return {An object with class = "TrialData" that has a list with an element for each ISA data.
#' Before creating the dataset the ProcessData to create the version of the data needed, eg change from baseline,
#' and also coppies covariates that are present.}
#' @export
CreateDataSetForAnalysis.default   <- function( cPats,  dCurrentTime, vISAStatus   )  #IndependentISA <- function( cPats,  dCurrentTime   )
{
    #print( "CreateDataSetForAnalysis.default")
    lRetData        <- structure( list(), class="TrialData")


    nQtyISA <- length( cPats$lPatOut )
    iISA    <- 1
    repeat
    {

        nQtyOutcomes    <- cPats$lPatOut[[iISA]]$nQtyOut
        iOut            <- 1
        lISA            <- structure( list(), class="TrialData")


        if( vISAStatus[ iISA ] != 0 )  #Only created a data set for the ISa that have been opened
        {
            repeat
            {


                strOut       <- paste( "mOut", iOut,sep="")
                cDS <- structure( list( lOut=NULL,
                                        vTrt        = cPats$lPatOut[[iISA]]$vTrt,
                                        vStartTimes = cPats$lPatOut[[iISA]]$vStartTimes),
                                  class= class(cPats$lPatOut[[iISA]][[strOut]] ))

                cDS$lOut     <- cPats$lPatOut[[iISA]][[strOut]]

                strObsTime   <- paste("vObsTime", iOut, sep="")
                cDS$vObsTime <- cPats$lPatOut[[iISA]][[ strObsTime ]]


                lData        <- ProcessData( cDS, dCurrentTime )
                lData        <- CopyCovariates( lData, cPats$lPatOut[[iISA]] )
                lISA[[paste("lDataOut", iOut, sep="")]] <- lData
                if( iOut == nQtyOutcomes )
                    break
                iOut <- iOut+1

            }
        }

        lRetData[[ iISA ]] <- lISA
        if( iISA == nQtyISA )
            break
        iISA <- iISA + 1
    }

    return( lRetData )
}

