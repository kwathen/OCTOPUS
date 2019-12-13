##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name MakeDecisionBasedOnPostProb
#' @title MakeDecisionBasedOnPostProbb
#' @description {This function makes a Go/No Go decision based on posterior probabilities.   }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecisionBasedOnPostProb.R}{View Code on GitHub} }
#' @export
MakeDecisionBasedOnPostProb <- function( cAnalysis, lCalcs )
{
    UseMethod( "MakeDecisionBasedOnPostProb", cAnalysis )
}


#' @name MakeDecisionBasedOnPostProb.default
#' @title MakeDecisionBasedOnPostProb.default
#' @description {This function will make the Go/NoGo decision based on two posterior probabilities
#' Pr( \eqn{\theta} > MAV |data ) and Pr( \eqn{\theta} > TV | data ) where \eqn{\theta} is the
#' parameter of interest.
#' \tabular{cccc}{
#'           \tab Pr( \eqn{\theta} > MAV ) > dMAVCutoff \tab | \tab Pr( \eqn{\theta} > MAV) <= dMAVCutoff \cr
#'           \tab ------------------------------------- \tab | \tab -------------------------------------\cr
#'     Pr( \eqn{\theta} > TV ) > dTVCutoff   \tab Go    \tab | \tab  Pause \cr
#'           \tab ------------------------------------- \tab | \tab -------------------------------------\cr
#'     Pr( \eqn{\theta} > TV ) <= dTVCutoff  \tab Pause \tab | \tab No Go \cr
#' }
#' }
#' @param cAnalysis The analysis object.
#' @param  lCalcs a list with dPrGrtTV, dPrGrtMAV, dMAVCutoff, dTVCutoff
#' @return
#' Return List of ( nGo, nNoGo, nPause ).  Only one of the elements should be 1 to reflect the decision.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecisionBasedOnPostProb.R}{View Code on GitHub} }
#' @export
MakeDecisionBasedOnPostProb.default <- function( cAnalysis, lCalcs )
{
    dPrGrtTV    <- lCalcs$dPrGrtTV
    dPrGrtMAV   <- lCalcs$dPrGrtMAV
    dMAVCutoff  <- lCalcs$dMAVCutoff
    dTVCutoff   <- lCalcs$dTVCutoff

    nGo <- nNoGo <- nPause <- 0


    if( dPrGrtMAV > dMAVCutoff && dPrGrtTV > dTVCutoff )
        nGo <- 1
    else if( dPrGrtMAV < dMAVCutoff && dPrGrtTV < dTVCutoff   )
        nNoGo <- 1
    else
        nPause <- 1

    return(  list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, dPrGrtMAV = dPrGrtMAV, dPrGrtTV = dPrGrtTV ) )

}

# This funciton contains the common code for MakeDecisionBasedOnPostProb.MAVOnly and MakeDecisionBasedOnPostProb.MAVTarget
# A much better approach would be to define a constructor that would have MAVTarget inherit from MAVOnly
MakeDecisionBasedOnPostProbMAVCommon <- function(  cAnalysis, lCalcs)
{
    dPrGrtMAV       <- lCalcs$dPrGrtMAV
    dPUpperCutoff   <- lCalcs$dPUpperCutoff
    dPLowerCutoff   <- lCalcs$dPLowerCutoff

    nGo <- nNoGo <- nPause <- 0


    if( dPrGrtMAV > dPUpperCutoff )
        nGo <- 1
    else if( dPrGrtMAV < dPLowerCutoff   )
        nNoGo <- 1
    else
        nPause <- 1

    return(  list( nGo = nGo, nNoGo = nNoGo, nPause = nPause, dPrGrtMAV = dPrGrtMAV) )

}


#' @name MakeDecisionBasedOnPostProb.MAVOnly
#' @title MakeDecisionBasedOnPostProb.MAVOnly
#' @description {This function will make the Go/NoGo decision based on one posterior probability,
#' Pr( \eqn{\theta} > MAV |data ) where \eqn{\theta} is the
#' parameter of interest. Based on this probability the following decisions are made
#' \tabular{lc}{
#' \tab Decision \cr
#' Pr( \eqn{\theta} > MAV |data ) > dPUpperCutoff     \tab Go   \cr
#' Pr( \eqn{\theta} > MAV |data ) < dLowerCutoff      \tab No Go \cr
#' Otherwise                                          \tab Pause \cr
#' }
#' }
#' @param cAnalysis The analysis object.
#' @param  lCalcs a list with  dPrGrtMAV, dPUpperCutoff, dLowerCutoff
#' @return
#' Return List of ( nGo, nNoGo, nPause ).  Only one of the elements should be 1 to reflect the decision.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecisionBasedOnPostProb.R}{View Code on GitHub} }
#' @export
MakeDecisionBasedOnPostProb.MAVOnly <- function( cAnalysis, lCalcs )
{
    return( MakeDecisionBasedOnPostProbMAVCommon( cAnalysis, lCalcs ) )
}




#' @name MakeDecisionBasedOnPostProb.MAVTarget
#' @title MakeDecisionBasedOnPostProb.MAVTarget
#' @description {This function will make the Go/NoGo decision based on one posterior probability,
#' Pr( \eqn{\theta} > MAV |data ) where \eqn{\theta} is the
#' parameter of interest. Based on this probability the following decisions are made
#' \tabular{lc}{
#' \tab Decision \cr
#' Pr( \eqn{\theta} > MAV |data ) > dPUpperCutoff     \tab Go   \cr
#' Pr( \eqn{\theta} > MAV |data ) < dLowerCutoff      \tab No Go \cr
#' Otherwise                                          \tab Pause \cr
#' }
#' }
#' @param cAnalysis The analysis object.
#' @param  lCalcs a list with  dPrGrtMAV, dPUpperCutoff, dLowerCutoff
#' @return
#' Return List of ( nGo, nNoGo, nPause ).  Only one of the elements should be 1 to reflect the decision.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/MakeDecisionBasedOnPostProb.R}{View Code on GitHub} }
#' @export
MakeDecisionBasedOnPostProb.MAVTarget <- function( cAnalysis, lCalcs )
{

    return( MakeDecisionBasedOnPostProbMAVCommon( cAnalysis, lCalcs ) )

}

