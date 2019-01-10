##################################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
##################################################################################################################

#' @export
MakeDecision<- function( lDecision, lResAnalysis, bFinalAnalysis )
{
    UseMethod( "MakeDecision", lDecision )
}


#' @name MakeDecision.TwoOutcomeOption1
#' @title MakeDecision.TwoOutcomeOption1
#' @description {This two outcome decision options utilizes Moddified OR between two outcomes.
#' For each outcome you make a decision Go (G), Pause (P) or No Go (NG) and when the two outcomes are
#' combined the following tabled describes how decisions will be made.
#' If either outcome is a G then then the decision is a Go, if it is NOT a Go then if either is a NG
#' the decision is a NG.  If both are a P then the decision is a P.
#' \tabular{ccccc}{
#'             \tab G |  \tab G \tab G  \tab G \cr
#'   Outcome 2 \tab P | \tab NG \tab P  \tab G \cr
#'             \tab NG| \tab NG \tab NG \tab G \cr
#'             \tab --- \tab--- \tab --- \tab--- \cr
#'             \tab     \tab NG \tab P  \tab G \cr
#'             \tab     \tab    \tab Outcome 1 \tab \cr
#' }
#' }
# Easier to read in this file
#                    Outcome 1
#               G | G   G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  NG  G
#                   ---------
#                   NG  P   G
#                    Outcome 1
#' @export
MakeDecision.TwoOutcomeOption1 <- function( lDecision, lResAnalysis, bFinalAnalysis )
{
    #print( paste( "MakeDecision.TwoOutcomeOption1"))
    nGo <- nNoGo <- nPause <- 0
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]

    if( lResAnalysis1$nGo == 1 || lResAnalysis2$nGo == 1)
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1 || lResAnalysis2$nNoGo == 1 )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }
    return( list( nGo = nGo, nNoGo = nNoGo, nPause = nPause ))

}
