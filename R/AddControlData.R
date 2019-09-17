##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name AddControlData
#' @title  AddControlData
#' @description {Used to include control data.}
#' @export
AddControlData <- function( cISAData, cSourceData, nISA )
{
    UseMethod( "AddControlData", cISAData )
}



#' @name AddControlData.ProcessReptMeasChngBaseline
#' @title AddControlData.ProcessReptMeasChngBaseline
#' @description{ Control data when processed as a Repeated measure change from outcome.}
#' @export
AddControlData.ProcessReptMeasChngBaseline <- function( cISAData, cSourceData, nISA )
{
    #print( "SubsetData.ProcessReptMeasChngBaseline ")
    vSubsetCtrl       <- cSourceData$vTrt == 1  #1 indicates the control

    cRetData          <- cISAData
    nMaxIND           <- max( cISAData$vIND )  # Need max to offset the IDN for what we are adding


    cRetData$vOut      <- c( cRetData$vOut,       cSourceData$vOut[  vSubsetCtrl ] )
    cRetData$vTime     <- c( cRetData$vTime,      cSourceData$vTime[  vSubsetCtrl ] )
    cRetData$vTrt      <- c( cRetData$vTrt,       cSourceData$vTrt[  vSubsetCtrl ] )
    cRetData$vIND      <- c( cRetData$vIND,       ( cSourceData$vIND[  vSubsetCtrl ] +nMaxIND) )
    cRetData$vBaseline <- c( cRetData$vBaseline,  cSourceData$vBaseline[  vSubsetCtrl ] )
    cRetData$vISA      <- c( cRetData$vISA,       rep( nISA, length( cSourceData$vTrt[  vSubsetCtrl ]) ))
    cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )
    return( cRetData )

}


#' @name AddControlData.ProcessReptMeas
#' @title AddControlData.ProcessReptMeasChngBaseline
#' @description{ Control data when processed as a Repeated measure change from outcome.}
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
    cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )
    return( cRetData )

}


#' @name AddControlData.ProcessSingleTimeOutcome
#' @title AddControlData.ProcessSingleTimeOutcome
#' @description{ Control data when processed as a single time point.}
#' @export
AddControlData.ProcessSingleTimeOutcome <- function( cISAData, cSourceData, nISA )
{
    #print( "SubsetData.ProcessSingleTimeOutcome")


    vSubsetCtrl       <- cSourceData$vTrt == 1  #1 indicates the control

    cRetData          <- cISAData
    nMaxIND           <- max( cISAData$vIND )  # Need max to offset the IDN for what we are adding


    cRetData$vOut      <- c( cRetData$vOut,       cSourceData$vOut[  vSubsetCtrl ] )
    cRetData$vTime     <- c( cRetData$vTime,      cSourceData$vTime[  vSubsetCtrl ] )
    cRetData$vTrt      <- c( cRetData$vTrt,       cSourceData$vTrt[  vSubsetCtrl ] )
    cRetData$vIND      <- c( cRetData$vIND,       ( cSourceData$vIND[  vSubsetCtrl ] +nMaxIND) )
    cRetData$vISA      <- c( cRetData$vISA,       rep( nISA, length( cSourceData$vTrt[  vSubsetCtrl ]) ))
    cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )
    return( cRetData )

}
