##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name CopyControlCovariates
#' @title  CopyControlCovariates
#' @description {Used to include control data with covaraites}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CopyControlCovariates.R}{View Code on GitHub} }
#' @export
CopyControlCovariates <- function( cDestData, cSourceData,  vSubsetCtrl )
{
    strCov      <- "vCov"
    iCov        <- 1
    strCovName  <- paste( strCov, iCov, sep="" )

    while( strCovName %in% names( cSourceData ) )
    {
        cDestData[[ strCovName ]] <- c( cDestData[[ strCovName ]], cSourceData[[ strCovName ]][ vSubsetCtrl ] )
        iCov        <- iCov + 1
        strCovName  <- paste( strCov, iCov, sep="" )
    }
    return( cDestData )
}
