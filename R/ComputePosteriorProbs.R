##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.



#' @name ComputePosteriorProbs
#' @title ComputePosteriorProbs
#' @export
ComputePosteriorProbs <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples )
{
    UseMethod( "ComputePosteriorProbs", cAnalysis )
}



#' @name ComputePosteriorProbs.default
#' @title ComputePosteriorProbs.default
#' @description {Used in a trial with a Bayesian analysis and parameter of interest \eqn{\theta}, where
#' the decisions are based two posterior probabilities Pr( \eqn{\theta} > MAV |data ) > vMAVCutoff
#' and Pr( \eqn{\theta} > TV | data ) > vTVCutoff.
#' This function returns a list with dMAVCutoff and dTVCutoff.  0 <= dMAVCutoff and dTVCutoff <= 1}
#' @param cAnalysis$vMAVCutoff A vector of cutoffs for the MAV at each analysis.
#' @param cAnalysis$vTVCutoff A vector of cutoffs for the TV.
#' @param nISAAnalysisIndx A the index of the analysis
#' @param bIsFinalISAAnalysis TRUE or FALSE to indicate if this is the final analysis. Typically, used
#' in a Bayesian design such that the final analysis can have different cutoff values.
#' @return List with two valued dMAVCutoff and dTVCutoff
#' @param lSamples List with two vectors, vPostSampPlac and vPostSampTrt, that are samples of the posterior of
#' \eqn{\theta} for the placebo and treatment, respectively.
#' @return List with four values dPrGrtMAV, dPrGrtTV, dMAVCutoff and dTVCutoff
#' @export
ComputePosteriorProbs.default <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples )
{


    lCutoff  <- GetBayesianCutoffs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dMAV     <- cAnalysis$dMAV
    dTV      <- cAnalysis$dTV

    if( cAnalysis$bPlacMinusTrt == TRUE)
    {

        dPrGrtMAV  <- mean( ifelse( lSamples$vPostSampPlac - lSamples$vPostSampTrt  > dMAV, 1, 0) )
        dPrGrtTV   <- mean( ifelse( lSamples$vPostSampPlac - lSamples$vPostSampTrt  > dTV, 1, 0 ) )
        #print( paste(  "Pr( Plac - Trt > MAV ) ", dPrGrtMAV))
        #print( paste(  "Pr( Plac - Trt > TV ) ", dPrGrtTV))
    }
    else
    {

        dPrGrtMAV  <- mean( ifelse( lSamples$vPostSampTrt - lSamples$vPostSampPlac < dMAV, 1, 0) )
        dPrGrtTV   <- mean( ifelse( lSamples$vPostSampTrt - lSamples$vPostSampPlac < dTV, 1, 0 ) )
        #print( paste(  "Pr( Trt - Plac > MAV ) ", dPrGrtMAV))
        #print( paste(  "Pr( Trt - Plac > TV ) ", dPrGrtTV))

    }

    lCalcs <- list( dPrGrtTV    = dPrGrtTV,
                    dPrGrtMAV   = dPrGrtMAV,
                    dMAVCutoff  = lCutoff$dMAVCutoff,
                    dTVCutoff   = lCutoff$dTVCutoff )

    lRet <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )
    return( lRet )

}







#' @name ComputePosteriorProbs.MAVOnly
#' @title ComputePosteriorProbs.MAVOnly
#' @description {Used in a trial with a Bayesian analysis and parameter of interest \eqn{\theta}, where
#' the decisions are based one posterior probabilities Pr( \eqn{\theta_P} - \eqn{\theta_T} > MAV |data ).
#' This function returns a list with dPrGrtMAV, dMAVCutoff and dTVCutoff.  0 <= dMAVCutoff and dTVCutoff <= 1}
#' @param cAnalysis$vMAVCutoff A vector of cutoffs for the MAV at each analysis.
#' @param cAnalysis$vTVCutoff A vector of cutoffs for the TV.
#' @param nISAAnalysisIndx A the index of the analysis
#' @param bIsFinalISAAnalysis TRUE or FALSE to indicate if this is the final analysis. Typically, used
#' in a Bayesian design such that the final analysis can have different cutoff values.
#' dTVCutoff
#' @param lSamples List with two vectors, vPostSampPlac and vPostSampTrt, that are samples of the posterior of
#' \eqn{\theta} for the placebo and treatment, respectively.
#' @return List with three values dPrGrtMAV,  dMAVCutoff and dTVCutoff
#' @export
ComputePosteriorProbs.MAVOnly <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples )
{

    lCutoff  <- GetBayesianCutoffs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dMAV     <- cAnalysis$dMAV

    if( cAnalysis$bPlacMinusTrt == TRUE)
    {

        dPrGrtMAV  <- mean( ifelse( lSamples$vPostSampPlac - lSamples$vPostSampTrt  > dMAV, 1, 0) )
        #print( paste(  "Pr( Plac - Trt > MAV ) ", dPrGrtMAV))
    }
    else
    {

        dPrGrtMAV  <- mean( ifelse( lSamples$vPostSampTrt - lSamples$vPostSampPlac > dMAV, 1, 0) )
        #print( paste(  "Pr( Trt - Plac > MAV ) ", dPrGrtMAV))

    }

    lCalcs <- list( dPrGrtMAV         = dPrGrtMAV,
                    dPUpperCutoff    = lCutoff$dPUpperCutoff,
                    dPLowerCutoff    = lCutoff$dPLowerCutoff )

    lRet <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )

    return( lRet )



}



#' @name ComputePosteriorProbs.MAVTarget
#' @title ComputePosteriorProbs.MAVTarget
#' @description {Used in a trial with a Bayesian analysis and parameter of interest \eqn{\theta}, where
#' the decisions are based one posterior probabilities Pr(  \eqn{\theta_T} > MAV |data ).
#' This function returns a list with dPrGrtMAV, dMAVCutoff and dTVCutoff.  0 <= dMAVCutoff and dTVCutoff <= 1}
#' @param cAnalysis$vMAVCutoff A vector of cutoffs for the MAV at each analysis.
#' @param cAnalysis$vTVCutoff A vector of cutoffs for the TV.
#' @param nISAAnalysisIndx A the index of the analysis
#' @param bIsFinalISAAnalysis TRUE or FALSE to indicate if this is the final analysis. Typically, used
#' in a Bayesian design such that the final analysis can have different cutoff values.
#' dTVCutoff
#' @param lSamples List with vPostSamp that are samples of the posterior of
#' \eqn{\theta} the parameter of interest.
#' @return List with three values dPrGrtMAV,  dMAVCutoff and dTVCutoff
#' @export
ComputePosteriorProbs.MAVTarget <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples )
{

    lCutoff  <- GetBayesianCutoffs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dMAV     <- cAnalysis$dMAV

    dPrGrtMAV  <- mean( ifelse( lSamples$vPostSamp  > dMAV, 1, 0) )


    lCalcs <- list( dPrGrtMAV         = dPrGrtMAV,
                    dPUpperCutoff    = lCutoff$dPUpperCutoff,
                    dPLowerCutoff    = lCutoff$dPLowerCutoff )


    lRet <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )

    return( lRet )



}




