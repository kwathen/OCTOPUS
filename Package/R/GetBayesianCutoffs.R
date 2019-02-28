##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @export
GetBayesianCutoffs <- function(  cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis )
{
    UseMethod( "GetBayesianCutoffs", cAnalysis )

}



#' @name GetBayesianCutoffs.default
#' @title GetBayesianCutoffs.default
#' @description {Used in a trial with a Bayesian analysis and parameter of interest \eqn{\theta}, where
#' the decisions are based two posterior probabilities Pr( \eqn{\theta} > MAV |data ) > vMAVCutoff
#' and Pr( \eqn{\theta} > TV | data ) > vTVCutoff.
#' This function returns a list with dMAVCutoff and dTVCutoff.  0 <= dMAVCutoff and dTVCutoff <= 1.
#' Typically, this function is not called directly but as part of the ComputePosteriorProb function.}
#' @param cAnalysis$vMAVCutoff A vector of cutoffs for the MAV at each analysis.
#' @param cAnalysis$vTVCutoff A vector of cutoffs for the TV.
#' @param nISAAnalysisIndx An integer with the index of the analysis
#' @param bIsFinalISAAnalysis TRUE or FALSE to indicate if this is the final analysis. Typically, used
#' in a Bayesian design such that the final analysis can have different cutoff values.
#' @return List with two values dMAVCutoff and dTVCutoff.
#' @export
GetBayesianCutoffs.default <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis  )
{

    if( bIsFinalISAAnalysis == FALSE  )
    {
        nIndx<- nISAAnalysisIndx
        if( nISAAnalysisIndx > length( cAnalysis$vMAVCutoff) )
        {
            nIndx <- length( cAnalysis$vMAVCutoff )
        }
        dMAVCutoff        <- cAnalysis$vMAVCutoff[nIndx]
        dTVCutoff         <- cAnalysis$vTVCutoff[nIndx]

    }
    else
    {
        dMAVCutoff        <- cAnalysis$dFinalMAVCutoff
        dTVCutoff         <- cAnalysis$dFinalTVCutoff
    }

    return( list( dMAVCutoff = dMAVCutoff, dTVCutoff = dTVCutoff ) )
}


#' @name GetBayesianCutoffs.MAVOnly
#' @title GetBayesianCutoffs.MAVOnly
#' @description {Used in a Bayesian analysis when you have interest in determining if a single posterior probability, eg probability a
#' parameter is greater than MAV,
#' being greater than an upper cutoff or below a lower cutoff.  In particular, you must
#' define vPUpper, vPLower, dFinalPUpper, dFinalPLower as part of your cAnalysis object.
#' This function returns a list with dPUpperCutoff and dPLowerCutoff.  0 <= dPUpperCutoff <= dPLowerCutoff <= 1}
#' @param  cAnalysis$vPUpper Vector of upper cutoffs, typically used in the context of success if a posterior probability > vPUpper,
#' 0 <=  vPLowerCutoff <= vPUpperCutoff <= 1
#' @param  cAnalysis$vPLower Vector of lower cutoffs, typically used in the context of failure if a posterior probability < vPLower,
#' 0 <=  vPLowerCutoff <= vPUpperCutoff <= 1
#' @param  cAnalysis$dFinalPUpper Value of upper cutoff used at the FINAL analysis, typically used in the context of success if a posterior probability > dFinalPUpper,
#' 0 <= dPLowerCutoff <= dPUpperCutoff <=  1
#' @param  cAnalysis$dFinalPLower Value of lower cutoff used at the FINAL analysis, typically used in the context of failure if a posterior probability < dFinalPLower,
#' 0 <= dPLowerCutoff <= dPUpperCutoff <=  1
#' @export
GetBayesianCutoffs.MAVOnly <- function( cAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis  )
{

    if( bIsFinalISAAnalysis == FALSE  )
    {
        nIndx<- nISAAnalysisIndx
        if( nISAAnalysisIndx > length( cAnalysis$vPUpper) )
        {
            nIndx <- length( cAnalysis$vPUpper )
        }
        dPUpper     <- cAnalysis$vPUpper[nIndx]
        dPLower     <- cAnalysis$vPLower[nIndx]

    }
    else
    {
        dPUpper     <- cAnalysis$dFinalPUpper
        dPLower     <- cAnalysis$dFinalPLower
    }

    return( list( dPUpperCutoff = dPUpper, dPLowerCutoff = dPLower) )
}
