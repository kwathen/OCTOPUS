##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

############### Outcome Level Analysis  ##################################################################################################
#  Run an analysis on a particular outcome
#  For Trial Level Analysis eg RunTrialAnalysis see TrialAnalysis.R
#  For ISA level analysis eg RunISAAnalysis see ISAAnalysis.R



#This file contains all of the code for running the analysis
#Each Analysis must return a list with at least the following
#  list( nGo = nGo, nNoGo= nNoGo,  nPause = nPause)
#   The list is a list of indicator that only one of which = 1, the others are 0
# For example, if you did a Go/NoGo with a CI see the results of MakeDecisionBasedOnCI()
# If the analysis return more info that is fine.


#' @name RunAnalysis
#' @title RunAnalysis
#' @description {This is the method used for running the analysis.  There are several options available for different types
#' of data.}
#' @export
RunAnalysis <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    UseMethod( "RunAnalysis", cAnalysis )
}


#' @name RunAnalysis.default
#' @title RunAnalysis.default
#' @description {This is the method used for running the analysis as a default.  This method is not defined and is
#' in place in-case someone provides an invalid class type for RunAnalysis.  This version will stop execution.  }
#' @export
RunAnalysis.default <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    #print(paste( "ERROR: The default RunAnalysis is not defined.  Class name= ", class(cAnalysis)))
    stop( paste( "ERROR: The default RunAnalysis is not defined.  Class name=", class(cAnalysis)))
}









