##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#############################################################################################################################.
#   Description                                                                                                         #####
#   This file contains the MakeDecision generic function. Several options are available.
#   For Trial decisions see the DecisionsFunctions.R
#
#   The return vISAStatus has the following options
#   0 = ISA not open;
#   1 = ISA open,
#   2 = Closed with a Go before FA,
#   3 = Closed - No Go before FA
#   4 = closed - Go at the FA
#   5 = Closed - No Go at the FA
#

#This file contains the decision options for:
#   1. MakeDecision.General - Will call MakeDecision and allows different options during the evolution analysis and final analysis
#   2. Combines 2 outcomes in several ways using the results from a each individual outcome
#
#####   Developer(s): J. Kyle Wathen, PhD                                                                               #####
#############################################################################################################################.


#' @name MakeDecision
#' @title MakeDecision
#' @description {This function is a generic function. Several options are available. }
#' @return A list like the following:  list( nGo = nGo, nNoGo = nNoGo, nPause = nPause ) with only one of the element = 1.
#'
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision<- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    UseMethod( "MakeDecision", lDecision )
}


#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.default<- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    if( length( lResAnalysis ) == 2 )  #There is just one outcome so return it.  Checking if 2 because the length( lResAnalysis) = # outcomes + 1 (for binary if analysis is run)
    {
        lRet <- list( nGo = lResAnalysis[[1]]$nGo, nNoGo = lResAnalysis[[1]]$nNoGo, nPause = lResAnalysis[[1]]$nPause )
        lRet$cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
        #lRet$cRandomizer <- lResAnalysis[[1]]$cRandomizer
        return( lRet  )
    }
    else
    {
        stop(paste( "ERROR: The default MakeDecision is not defined, class(lDecision) = ", class( lDecision)))
    }

}

#' @name MakeDecision.General
#' @title MakeDecision.General
#' @description {This is the General version of Make decisions that allows for one approach at IAs (IA)
#' and another (which could be the same) at the Final Analysis (FA).  You must specify
#' lDecision$strApproachIA, method used during the IAs, and lDecision$strApproachFA, method used at the FA.
#' The options for strApproachFA and strApproachIA are any of the MakeDecision options like MakeDecision.TwoOutcomesOption1 or
#' MakeDecision.TwoOutcomeOptions2.
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.General <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    #print( paste( "MakeDecision.General"))

    if( bFinalAnalysis )
        strApproach <- lDecision$strApproachFA
    else
        strApproach <- lDecision$strApproachIA

    #print( paste( "MakeDecision ", strApproach))
    lDecTmp          <- structure( lDecision, class = strApproach )
    lRet             <- MakeDecision( lDecTmp, lResAnalysis, bFinalAnalysis, cRandomizer )
    #lRet$cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis )
    return(  lRet )

}


MakeDecision.GeneralDoses <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{

    #print( "MakeDecision.GeneralDoses")

    if( bFinalAnalysis )
        strApproach <- lDecision$strApproachFA
    else
        strApproach <- lDecision$strApproachIA

    if( lResAnalysis$bISAAnalysisRun == TRUE )
    {


        lAnalysisTmp <- lResAnalysis[[1]][ names(lResAnalysis[[1]]) != "cRandomizer"]
        nQtyAnalysis <- length( lAnalysisTmp )
        lDecTmp <- vector( "list", length = nQtyAnalysis)
        for( i in 1:nQtyAnalysis )
        {

            lDecTmp[[ i ]] <- lResAnalysis[[1]][[i]]
        }
        class( lDecTmp ) <- strApproach
    }



    if( exists("gDebug") == TRUE )  #Need to make sure it exists
    {
        if( gDebug == TRUE)
            browser()
    }
    #print( paste( ".....MakeDecision ", strApproach))
    lRet <- MakeDecisionDoses( lDecTmp )
    lRet$cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer)

    #print( paste( ".....MakeDecision ", strApproach, " ",lRet))
    return( lRet )


}


#' @name MakeDecision.Outcome1Only
#' @title MakeDecision.Outcome1Only
#' @description {This option ONLY uses the decision from Outcome 1.
#' \tabular{ccccc}{
#'             \tab G |  \tab  NG \tab P   \tab G \cr
#'   Outcome 2 \tab P |  \tab  NG \tab P   \tab G \cr
#'             \tab NG|  \tab  NG \tab P   \tab G \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.Outcome1Only <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer)
{
    lRet  <- lResAnalysis[[1]]
    lRet$cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer)
    return( lRet )

}


#' @name MakeDecision.Outcome2Only
#' @title MakeDecision.Outcome2Only
#' @description {This option ONLY uses the decision from Outcome 2.
#' \tabular{ccccc}{
#'             \tab G |  \tab  G  \tab G   \tab G \cr
#'   Outcome 2 \tab P |  \tab  P  \tab P   \tab P \cr
#'             \tab NG|  \tab  NG \tab NG  \tab NG \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.Outcome2Only<- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{


    lRet  <- lResAnalysis[[2]]
    lRet$cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    return( lRet )


}

#' @name MakeDecision.TwoOutcomeOption1
#' @title MakeDecision.TwoOutcomeOption1
#' @description {This two outcome decision options utilizes modified OR between two outcomes.
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
#                   ---------     .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption1 <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
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


    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}


#' @name MakeDecision.TwoOutcomeOption2
#' @title MakeDecision.TwoOutcomeOption2
#' @description {This two outcome decision options utilizes an AND between two outcomes.
#' For each outcome you make a decision Go (G), Pause (P) or No Go (NG) and when the two outcomes are
#' combined the following tabled describes how decisions will be made.
#' \tabular{ccccc}{
#'             \tab G |  \tab P \tab P \tab G \cr
#'   Outcome 2 \tab P | \tab P  \tab P \tab P \cr
#'             \tab NG| \tab NG \tab P \tab P \cr
#'             \tab --- \tab--- \tab --- \tab--- \cr
#'             \tab     \tab NG \tab P \tab G \cr
#'             \tab     \tab    \tab Outcome 1 \tab \cr
#' }
#' }
# Easier to read in this file
#               G | P   P   G
#  Outcome 2    P | P   P   P
#               NG| NG  P   P
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption2 <- function(  lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    #print( paste( "MakeDecision.TwoOutcomeOption2"))
    nGo <- nNoGo <- nPause <- 0
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]

    if( lResAnalysis1$nGo == 1 && lResAnalysis2$nGo == 1)
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1 && lResAnalysis2$nNoGo == 1 )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }

    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}


#' @name MakeDecision.TwoOutcomeOption3
#' @title MakeDecision.TwoOutcomeOption3
#' @description {This two outcome decision options makes if GO if either outcome is a Go and makes a No Go
#' if BOTH outcomes are a No Go, otherwise a Pause is made.
#' For each outcome you make a decision Go (G), Pause (P) or No Go (NG) and when the two outcomes are
#' combined the following tabled describes how decisions will be made.
#' \tabular{ccccc}{
#'             \tab G |  \tab G \tab G \tab G \cr
#'   Outcome 2 \tab P | \tab P  \tab P \tab G \cr
#'             \tab NG| \tab NG \tab P \tab G \cr
#'             \tab --- \tab--- \tab --- \tab--- \cr
#'             \tab     \tab NG \tab P \tab G \cr
#'             \tab     \tab    \tab Outcome 1 \tab \cr
#' }
#' }
# Easier to read in this file
#               G | G   G   G
#  Outcome 2    P | P   P   G
#               NG| NG  P   G
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption3 <- function( lDecision,lResAnalysis, bFinalAnalysis, cRandomizer  )
{
    #print( paste( "MakeDecision.TwoOutcomeOption3"))
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]
    nGo <- nNoGo <- nPause <- 0

    if( lResAnalysis1$nGo == 1 || lResAnalysis2$nGo == 1)
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1 && lResAnalysis2$nNoGo == 1 )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }

    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}

#' @name MakeDecision.TwoOutcomeOption4
#' @title MakeDecision.TwoOutcomeOption4
#' @description {This option is as follows.
#' \tabular{ccccc}{
#'             \tab G |  \tab  NG \tab G   \tab G \cr
#'   Outcome 2 \tab P |  \tab  NG \tab P   \tab G \cr
#'             \tab NG|  \tab  NG \tab NG  \tab G \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#               G | NG  G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  NG  G
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption4 <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]
    nGo <- nNoGo <- nPause <- 0


    if( lResAnalysis1$nGo == 1 || (lResAnalysis2$nGo == 1 && lResAnalysis1$nNoGo != 1) )
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1 || ( lResAnalysis2$nNoGo == 1 && lResAnalysis1$nGo != 1) )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }

    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}
#' @name MakeDecision.TwoOutcomeOption5
#' @title MakeDecision.TwoOutcomeOption5
#' @description {This option is as follows.
#' \tabular{ccccc}{
#'             \tab G |  \tab  NG \tab G   \tab G \cr
#'   Outcome 2 \tab P |  \tab  NG \tab P   \tab G \cr
#'             \tab NG|  \tab  NG \tab P   \tab G \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#               G | NG  G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  P   G
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption5 <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]
    nGo <- nNoGo <- nPause <- 0

    if( lResAnalysis1$nGo == 1 || (lResAnalysis2$nGo == 1 && lResAnalysis1$nNoGo != 1) )
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1  )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }

    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}



#' @name MakeDecision.TwoOutcomeOption7
#' @title MakeDecision.TwoOutcomeOption7
#' @description {This two outcome decision options priority on a Go for Outcome 1
#' \tabular{ccccc}{
#'             \tab G |  \tab  P  \tab G   \tab G \cr
#'   Outcome 2 \tab P |  \tab  NG \tab P   \tab G \cr
#'             \tab NG|  \tab  NG \tab P   \tab G \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption7 <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]
    nGo <- nNoGo <- nPause <- 0

    if( lResAnalysis1$nGo == 1  || (lResAnalysis1$nPause == 1 && lResAnalysis2$nGo == 1 ))
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nNoGo == 1 && lResAnalysis2$nGo == 0 )
    {
        nNoGo <- 1
    }
    else
    {
        nPause <- 1
    }

    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}






#' @name MakeDecision.TwoOutcomeOption11
#' @title MakeDecision.TwoOutcomeOption11
#' @description {This option is as follows.
#' \tabular{ccccc}{
#'             \tab G |  \tab  P  \tab G   \tab G \cr
#'   Outcome 2 \tab P |  \tab  P  \tab P   \tab G \cr
#'             \tab NG|  \tab  NG \tab P   \tab P \cr
#'             \tab ---  \tab---  \tab --- \tab--- \cr
#'             \tab      \tab  NG \tab P   \tab G \cr
#'             \tab      \tab     \tab Outcome 1 \tab \cr
#' }
#' }
#               G | P   G   G
#  Outcome 2    P | P   P   G
#               NG| NG  P   P
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecision.R}{View Code on GitHub} }
#' @export
MakeDecision.TwoOutcomeOption11 <- function( lDecision, lResAnalysis, bFinalAnalysis, cRandomizer )
{
    #print( paste( "MakeDecision.TwoOutcomeOption11"))

    lResAnalysis1  <- lResAnalysis[[1]]
    lResAnalysis2  <- lResAnalysis[[2]]
    nGo <- nNoGo <- nPause <- 0

    if( lResAnalysis1$nNoGo == 1 && lResAnalysis2$nGo == 1)
    {
        nPause <- 1
    }
    else if(lResAnalysis1$nPause == 1 && lResAnalysis2$nGo == 1  )
    {
        nGo <- 1
    }
    else if(lResAnalysis1$nGo == 1 && lResAnalysis2$nGo == 1  )
    {
        nGo <- 1
    }
    else if( lResAnalysis1$nNoGo == 1 && lResAnalysis2$nPause == 1)
    {
        nPause <- 1
    }
    else if(lResAnalysis1$nPause == 1 && lResAnalysis2$nPause == 1  )
    {
        nPause <- 1
    }
    else if(lResAnalysis1$nGo == 1 && lResAnalysis2$nPause == 1  )
    {
        nGo <- 1
    }
    else if( lResAnalysis1$nNoGo == 1 && lResAnalysis2$nNoGo == 1)
    {
        nNoGo <- 1
    }
    else if(lResAnalysis1$nPause == 1 && lResAnalysis2$nNoGo == 1  )
    {
        nPause <- 1
    }
    else if(lResAnalysis1$nGo == 1 && lResAnalysis2$nNoGo == 1  )
    {
        nPause <- 1
    }
    cRandomizer <- UpdateRandomizer( lDecision, lResAnalysis, cRandomizer )
    lRet        <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, cRandomizer = cRandomizer )
    return( lRet )

}



