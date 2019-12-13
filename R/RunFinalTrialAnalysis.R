##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#TODO(Kyle) - CHeck to see if this function is still needed, looks to be no longer needed 7.18.2019


#' @name RunFinalAnalysis
#' @title RunFinalAnalysis
#' @description{ Need description }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunFinalTrialAnalysis.R}{View Code on GitHub} }
#' @export
RunFinalTrialAnalysis <- function( cISADesigns, cEnrolledPats,  vISAStatus,  vISAAnalysisIndx  )
{
    UseMethod( "RunFinalTrialAnalysis", cISADesigns )
}


#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunFinalTrialAnalysis.R}{View Code on GitHub} }
#' @export
RunFinalTrialAnalysis.default <- function( cISADesigns, cEnrolledPats,  vISAStatus,  vISAAnalysisIndx )
{
    nQtyISA <- length( cISADesigns )

    lResISA <- list()

    bISAAnalysisRun <- FALSE
    vTimeFinalAnalysis <- rep( -1, nQtyISA)
    for( nISA in 1:nQtyISA)
    {
        lISARes <- list()

        if( vISAStatus[ nISA ] == 2 )
        {

            #print( paste( ".....ISA ", nISA, " FA - Case 1"))
            #Figure out when the FA for this ISA is
            dAnayTime       <- GetFinalISAAnalysisTime( cISADesigns, nISA, cEnrolledPats )
            cISAAnalysis    <- cISADesigns[[ nISA ]]$cISAAnalysis
            lDataAna        <- CreateDataSetForAnalysis( cEnrolledPats, dAnayTime, vISAStatus  )
            class( lDataAna ) <- class( cISAAnalysis)

            lDataTmp        <- SubsetData( lDataAna, nISA ) # cISAAnalysis$nISA )
            lISARes         <- RunISAAnalysis( cISAAnalysis, lDataTmp, vISAAnalysisIndx[ nISA], TRUE )

            bISAAnalysisRun <- TRUE
            vISAAnalysisIndx[ nISA ] <- vISAAnalysisIndx[ nISA ] + 1

            vTimeFinalAnalysis[ nISA ] <- dAnayTime
        }
        else if(  vISAStatus[ nISA ] <= 1  )
        {

            bISAAnalysisRun <- FALSE
            #print( paste( "......ISA ", nISA, " FA - Case 2 ****************** THIS CASE SHOULD NOT OCCUR"))

            lISARes <- list( nGo = 1, nNoGo = 0, nPause = 0)

        }
        else
        {

            bISAAnalysisRun <- FALSE


            #print( paste( ".....ISA ", nISA, " FA - Case 3"))
        }

        lISARes$bISAAnalysisRun <- bISAAnalysisRun

        lResISA[[paste("lResISA", nISA, sep="")]] <- lISARes



    }
    lRet <- list( lResISA = lResISA, vISAAnalysisIndx = vISAAnalysisIndx, vTimeFinalAnalysis = vTimeFinalAnalysis )
    return( lRet )

}

