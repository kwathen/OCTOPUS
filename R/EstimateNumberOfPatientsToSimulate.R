##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name EstimateNumberOfPatientToSimulate
#' @title  EstimateNumberOfPatientToSimulate
#' @description EstimateNumberOfPatientToSimulate {}
#' @export
EstimateNumberOfPatientsToSimulate <- function( cSimCovariates, nMaxQtyPats )
{
    UseMethod( "EstimateNumberOfPatientsToSimulate", cSimCovariates )
}

#' @name EstimateNumberOfPatientToSimulate.default
#' @title EstimateNumberOfPatientToSimulate.default
#' @description EstimateNumberOfPatientsToSimulate.default{ Call the EstimateNumberOfPatientsToSimulate for each covaiate and returns the maximum of the results.}
#' @export
EstimateNumberOfPatientsToSimulate.default <- function( cSimCovariates, nMaxQtyPats  )
{
    vRet <- mapply(FUN = EstimateNumberOfPatientsToSimulate,  cSimCovariates, nMaxQtyPats = nMaxQtyPats )
    if( all( vRet <= 1.0 ) )
    {
        #Returned the min probability of each covaraite, so the smallest group is the produce of the vector * nMaxQtyPats
        nMax <- ceiling( nMaxQtyPats / (min( prod( vRet )*2, 1.0) ))
    }
    else
    {
        nMax <- max( vRet )
    }

    return( nMax )
}

#' @name EstimateNumberOfPatientToSimulate.NULL
#' @title EstimateNumberOfPatientToSimulate.NULL
#' @description EstimateNumberOfPatientToSimulate.NULL {This opion is used when no covariates are in a design. }
#' @export
EstimateNumberOfPatientsToSimulate.NULL <- function( cSimCovariates,  nMaxQtyPats   )
{

    return( nMaxQtyPats )
}

#' @name EstimateNumberOfPatientToSimulate.Categorical
#' @title EstimateNumberOfPatientToSimulate.Categorical
#' @description EstimateNumberOfPatientToSimulate.Categorical {returns nMaxQtyPats/min(vProbs)}
#' @export
EstimateNumberOfPatientsToSimulate.Categorical <- function( cSimCovariates, nMaxQtyPats  )
{
    dMinProb       <- min( cSimCovariates$vProbs )
    return( dMinProb )
}


