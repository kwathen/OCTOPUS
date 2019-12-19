##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


##### TODO(Kyle) - This file need more documentation


#The cSimOutcomes is a class of type Independent or Correlated
# It should contain a list of outcomes to simulate
#
# 2 possibilities - 1) 1 outcome, 2) more than 1 outcome
# Option 1) 1 outcome
#  cSimOutcomes <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(1,2,3)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#
# Option 2) m outcome - mMeanNull1
#  cSimOutcome1 <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(1,2,3)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#  cSimOutcome2 <- structure( list(  mMean = mMeanNull1, mVarCov = mVarCov1, vObsTime = vObsTime, vColIndex = c(4,5,6)),
#                           class=c("SimulateMVN","ProcessReptMeasChngBaseline"))
#  cSimOutcomes <- structure( list( cSimOutcome1 = cSimOutcome1,
#                                   cSimOutcome2 = cSimOutcome2), class= "Correlated")
##
#
#
#

#' @name SimPatientOutcomes
#' @title SimPatientOutcomes
#' @description SimPatientOutcomes {This function is intended to simulate the outcomes for a given ISA.}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimPatientOutcomes.R}{View Code on GitHub} }
#' @export
SimPatientOutcomes <- function( cSimOutcomes, cISADesign, dfPatCovISA )
{
    UseMethod( "SimPatientOutcomes", cSimOutcomes )
}

#' @title SimPatientOutcomes.default
#' @describeIn SimPatientOutcomes { This function is intended to simulate the outcomes for a given ISA.
#' Because several options are provided and there in no well defined default
#' an stop error occurs if you call the default method. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimPatientOutcomes.R}{View Code on GitHub} }
#' @export
SimPatientOutcomes.default <- function( cSimOutcomes,  cISADesign, dfPatCovISA )
{
    stop(print("ERROR: The default sim patient outcomes is not defined.", class( cSimOutcomes)))
}








