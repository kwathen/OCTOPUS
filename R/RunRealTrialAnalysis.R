##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


# This file contains the functions needed for a real trial, eg called during trial conduct

#' @name RunRealTrialAnalysis
#' @title RunRealTrialAnalysis
#' @description {Generic function that could be used to run an analysis on trial data.  }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunRealTrialAnalysis.R}{View Code on GitHub} }
#' @export
RunRealTrialAnalysis  <- function( cTrialDesign, strInputFile, strOutputFile, nSeed )
{
    UseMethod( "RunRealTrialAnalysis", cTrialDesign )
}

#' @name RunRealTrialAnalysis.default
#' @title RunRealTrialAnalysis.default
#' @description {RunRealTrialAnalysis.default provides the default implmentation as it is not anticipated to be utilized very often.  }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunRealTrialAnalysis.R}{View Code on GitHub} }
#' @export
RunRealTrialAnalysis.default <- function( cTrialDesign, strInputFile, strOutputFile, nSeed )
{
    print( paste( "RunRealTrialAnalysis.default - Input Received: Input File: ", strInputFile, " Output file: ", strOutputFile, ", Seed: ", nSeed ))

    #This function expected the input file to have the following columns
    #Patient ID, Treatment, ISA, outcome index, outcome evaluation index (1,2,...), outcome value as a CSV with column headers
    mInputData <- read.table( strInputFile, header=TRUE, sep="," )

    lEnrolledPats    <- InitializePatientList( cTrialDesign )
    lEnrolledPats$vTrt <- c( mInputData[["Treatment"]] )
    lEnrolledPats$vISA <- c( mInputData[["ISA"]] )
    lEnrolledPats$vStartTimes <- rep( 0, length( lEnrolledPats$vISA) )
    #Using the first ISA as the template for what the data should look like
    nQtyOutcomes <- length( cTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis  )
    iOut <-1
    for( iOut in 1:nQtyOutcomes  )
    {

        nQtyObs <- length( cTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[iOut]]$vObsTime )
        if( nQtyObs > 1)
        {
            strOutcomeName <- paste( "Outcome", iOut, ".", 1:nQtyObs, sep="")
            mOutTmp <- as.matrix( mInputData[,strOutcomeName ])
        }else
        {
            strOutcomeName <- paste( "Outcome", iOut,  sep="")
            mOutTmp <- as.matrix( mInputData[,strOutcomeName ], ncol=1)
        }

        lEnrolledPats$lPatOut[[ iOut ]] <- structure(  mOutTmp, class=class(cTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[iOut]]) )



    }
    vUniqISA <- c(unique(lEnrolledPats$vISALab))
    lEnrolledPats$vCurrentQtyPatsISA <- table( c(lEnrolledPats$vISA, vUniqISA ))-1

    tabPatsPerArm <- table( c(lEnrolledPats$vTrt,lEnrolledPats$vTrtLab), c(lEnrolledPats$vISA,lEnrolledPats$vISALab))-1
    lEnrolledPats$vQtyPatsArmISA <- tabPatsPerArm[ cbind(lEnrolledPats$vTrtLab,lEnrolledPats$vISALab)]
    return( lEnrolledPats )

}
