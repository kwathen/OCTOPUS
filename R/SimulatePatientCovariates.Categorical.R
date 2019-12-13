##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @title SimulatePatientCovariates.Categorical
#' @describeIn SimulatePatientCovariates  { This function is used to simulate the patient covariate values.
#' This function is intended to do one covariate, for all patients.  The sampled covaraites values will be 1,2,..., length( cSimCovariate$vProbs ).}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulatePatientCovariates.Categorical.R}{View Code on GitHub} }
#' @export
SimulatePatientCovariates.Categorical <- function(  cSimCovariate,  nQtyOfPatients )
{
    if( sum( cSimCovariate$vProbs ) != 1.0 )
        stop( "The sum(  cSimCovariate$vProbs ) != 1, please correct and try again. ")

    nQtyLevels     = length( cSimCovariate$vProbs )
    vRetCovariates = sample.int( n = nQtyLevels, size = nQtyOfPatients, prob = cSimCovariate$vProbs,  replace = TRUE )

    return( vRetCovariates )
}
