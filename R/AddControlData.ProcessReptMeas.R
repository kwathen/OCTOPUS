##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name AddControlData.ProcessReptMeas
#' @title AddControlData.ProcessReptMeasChngBaseline
#' @description{ Control data when processed as a Repeated measure change from outcome.}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/AddControlData.ProcessReptMeas.R}{View Code on GitHub} }
#' @export
AddControlData.ProcessReptMeas <- function( cISAData, cSourceData, nISA )
{
    #print( "SubsetData.ProcessReptMeasChngBaseline ")
    vSubsetCtrl       <- cSourceData$vTrt == 1  #1 indicates the control

    cRetData          <- cISAData
    nMaxIND           <- max( cISAData$vIND )  # Need max to offset the IDN for what we are adding


    cRetData$vOut      <- c( cRetData$vOut,       cSourceData$vOut[  vSubsetCtrl ] )
    cRetData$vTime     <- c( cRetData$vTime,      cSourceData$vTime[  vSubsetCtrl ] )
    cRetData$vTrt      <- c( cRetData$vTrt,       cSourceData$vTrt[  vSubsetCtrl ] )
    cRetData$vIND      <- c( cRetData$vIND,       ( cSourceData$vIND[  vSubsetCtrl ] +nMaxIND) )
    cRetData$vISA      <- c( cRetData$vISA,       rep( nISA, length( cSourceData$vTrt[  vSubsetCtrl ]) ))

    cRetData           <- CopyControlCovariates( cRetData, cSourceData, vSubsetCtrl )
    cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )

    return( cRetData )

}
