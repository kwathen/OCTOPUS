##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name CopyCovariates
#' @title CopyCovariates
#' @description {This function will copy elements  lSimSource$vCov1,..., lSimSource$vCovXX to lSimDet and return the updated lSimDet.
#'   The names vCov1, vCov2,... must be consecutive and cannot skip eg, vCov1, vCov3 will the function will stop after copying vCov1.}
#' @export
CopyCovariates <- function( lSimDest, lSimSource)
{
    UseMethod( "CopyCovariates", lSimDest )
}

#' @name CopyCovariates.default
#' @title CopyCovariates.default
#' @description  This function will copy elements  lSimSource$vCov1,..., lSimSource$vCovXX to lSimDet and return the updated lSimDet.
#'   The names vCov1, vCov2,... must be consecutive and cannot skip eg, vCov1, vCov3 will the function will stop after copying vCov1.
#'
#' @export
CopyCovariates.default <- function( lSimDest, lSimSource)
{
    strCov      <- "vCov"
    iCov        <- 1
    strCovName  <- paste( strCov, iCov, sep="" )

    while( strCovName %in% names( lSimSource ) )
    {
        lSimDest[[ strCovName ]] <- lSimSource[[ strCovName ]]
        #if there was a vKeep vector in lSimDest then we need to drop the vKeep = FALSe
        if( "vKeep" %in% names( lSimDest ) )
        {
            lSimDest[[ strCovName ]] <- lSimDest[[ strCovName ]][ lSimDest$vKeep ==TRUE]
            nRep  <- length( lSimDest$vIND ) / lSimDest$nQtyPats
            lSimDest[[ strCovName ]] <- rep( lSimDest[[ strCovName ]], nRep )

        }
        iCov        <- iCov + 1
        strCovName  <- paste( strCov, iCov, sep="" )
    }
    return( lSimDest )
}
