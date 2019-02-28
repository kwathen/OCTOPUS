##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @export
MakeTrialDecision <- function( cISADesigns, lResAnalysis,  vISAStatus,  vIsFinalISAAnalysis )
{
    UseMethod( "MakeTrialDecision", cISADesigns )

}

#' @name MakeTrialDecision
#' @title MakeTrialDecision
#' @description {This function determines the status of a trial (all ISAs) based on the input. Each ISA
#' can be in one of eight states. The following list provides the details of the returned vISAStatus and what
#' they mean.
#' \describe{
#' \item{Status 0}{The ISA is not open, the virtual trial is at a point prior to the ISA opening.}
#' \item{Status 1}{The ISA is open for enrollment.}
#' \item{Status 2}{The ISA has met the maximum enrollment.}
#' \item{Status 3}{The ISA was closed with a Go BEFORE Final Analysis (FA).  This can be a Final status for an ISA because it was closed.}
#' \item{Status 4}{The ISA was closed with a No Go BEFORE FA.  This can be a Final status for an ISA because it was closed.}
#' \item{Status 5}{The ISA was closed with a Go at FA.  This can be a Final status for an ISA because it was closed.}
#' \item{Status 6}{The ISA was closed with a No Go at FA.  This can be a Final status for an ISA because it was closed.}
#' \item{Status 7}{The ISA was closed with a Pause at FA.  This can be a Final status for an ISA because it was closed.}
#' }
#' The Status options of 3 or 4 indicate the ISA was closed early, before the FA, either for a Go or a No Go.  The options 5-7
#' all were closed at the FA.
#' }
#the return vISAStatus has the following options
#   0 = ISA not open;
#   1 = ISA open,
#   2 = met max enrollment,
#   3 = Closed with a Go before FA,
#   4 = Closed - No Go before FA
#   5 = closed - Go at the FA
#   6 = Closed - No Go at the FA
#   7 = Closed - Pause at the FA
#' @export
MakeTrialDecision.default <- function( cISADesigns, lResAnalysis,  vISAStatus,  vIsFinalISAAnalysis  )
{

    nQtyISA <- length( cISADesigns )
    lDecRet <- list()

    for( nISA in 1:nQtyISA )
    {
        lDec <- list()

        lDecision <- cISADesigns[[ nISA ]]$lDecision
        #

        if( vISAStatus[ nISA ] <= 2 && lResAnalysis[[ nISA ]]$bISAAnalysisRun )
        {
            lDec <- MakeDecision( lDecision, lResAnalysis[[ nISA ]], vIsFinalISAAnalysis[ nISA ] )

            if( !vIsFinalISAAnalysis[ nISA ] )
            {
                if( lDec$nGo == 1  )
                {
                    vISAStatus[ nISA ] <- 3  #Closed and a Go before FA
                }
                else if( lDec$nNoGo == 1 )
                {
                    vISAStatus[ nISA ] <- 4 # Closed and a No Go before FA
                }
            }
            else
            {
                #print( sprintf( "Final Analysis for ISA  %f", nISA ))
                if( lDec$nGo == 1  )
                {
                    vISAStatus[ nISA ] <- 5 #Closed and a Go at the FA
                }
                else if( lDec$nNoGo == 1 )
                {
                    vISAStatus[ nISA ] <- 6 # Closed and a No Go at the FA
                }
                else if( lDec$nPause == 1)
                {
                    vISAStatus[ nISA ] <- 7

                }
            }




        }
        else if( vISAStatus[ nISA ] <= 1 )
        {
            lDec <- list( nGo = 0, nNoGo = 0, nPause = 0 )
        }

        lDec$bISAAnalysisRun <- lResAnalysis[[ nISA ]]$bISAAnalysisRun


        lDecRet[[ paste("lDecISA", nISA, sep="") ]] <- lDec
    }

    return( list( lDecRet = lDecRet, vISAStatus = vISAStatus)  )

}
