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
#' of data.
#' Inputs:
#'     cAnalysis - The class( cAnalysis ) determines the specific version of RunAnalysis that is called. It contains
#'                 the details about the analysis such as the priors, MAV, TV, decsiion cutoff boundaries.
#'     lDataAna  - The data that is used int he analysis.  Typically contains vISA (the ISA for the patient),
#'                 vTrt (treatment for each patient), vOut (the outcome for each patient)
#'
#' Return:   Each Analysis must return a list with at least the following
#'  list( nGo = nGo, nNoGo= nNoGo,  nPause = nPause)
#'   The list is a list of indicator that only one of which = 1, the others are 0
#' For example, if you did a Go/NoGo with a CI see the results of MakeDecisionBasedOnCI()
#' If the analysis return more info that is fine.}
#' @param cAnalysis The class( cAnalysis ) determines the specific version of RunAnalysis that is called. It contains
#'                 the details about the analysis such as the priors, MAV, TV, decsiion cutoff boundaries.
#' @param lDataAna The data that is used int he analysis.  Typically contains vISA (the ISA for the patient),
#'                 vTrt (treatment for each patient), vOut (the outcome for each patient)
#' @param nISAAnalysisIndx  index of the analysis used for changing boundaries)
#' @param bIsFinaISAAnalysis TRUE or FALSE, often we change the value of the cutoff at the final analysis for an ISA
#' @param cRandomizer The randomizer, mainly used for cases with covariates. For most cases without covariates,
#' simply add cRandomizer = cRandomizer to the return list.
#' @return Each Analysis must return a list with at least the following list( nGo = nGo, nNoGo= nNoGo,  nPause = nPause, cRandomizer = cRandomizer ).
#' The list is a list of indicator that only one of which = 1, the others are 0
#' For example, if you did a Go/NoGo with a CI see the results of MakeDecisionBasedOnCI()
#' If the analysis return more info that is fine.
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









