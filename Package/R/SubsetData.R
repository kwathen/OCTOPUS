##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name SubsetData
#' @title SubsetData
#' @description {This function is used to subset data.}
#' @export
SubsetData <- function( cSimDataAna, nISA )
{
    UseMethod( "SubsetData", cSimDataAna )
}

#cSimDataAna <- lDataAna, nISA
#This version will go through all the outcomes in lSimDataAna (eg trial data) and subset

#' @export
SubsetData.TrialData <- function( cSimDataAna, nISA )
{
    #print( "SubsetData.TrialData")
    lISAData      <- cSimDataAna[[nISA]]
    #nQtyOutcomes <- length( lISAData )
    #lRetData        <- structure( list(), class="ISAData")

    #iOut <- 1
    #for( iOut in 1:nQtyOutcomes )
    #{
        #cData <- SubsetData( cSimDataAna[[ iOut ]], nISA )

     #   lRetData[[paste("lDataOut", iOut, sep="")]] <- lISAData[[ paste( "mOut", iOut,sep="") ]]
    #}
    return( lISAData )

}

#This version will go through all the outcomes in lSimDataAna (eg trial data) and subset

#' @export
SubsetData.NoBorrowing <- function( cSimDataAna, nISA )
{
    #print( "SubsetData.TrialData")
    cISAData      <- cSimDataAna[[nISA]]
    #nQtyOutcomes <- length( lISAData )
    #lRetData        <- structure( list(), class="ISAData")

    #iOut <- 1
    #for( iOut in 1:nQtyOutcomes )
    #{
    #cData <- SubsetData( cSimDataAna[[ iOut ]], nISA )

    #   lRetData[[paste("lDataOut", iOut, sep="")]] <- lISAData[[ paste( "mOut", iOut,sep="") ]]
    #}
    return( cISAData )

}

#This version will go through all the outcomes in lSimDataAna (eg trial data) and subset

#' @name SubsetData.AllControls
#' @title SubsetData.AllControls
#' @description{ Include all control data}
#' @export
SubsetData.AllControls <- function( cSimDataAna, nISA )
{
    #First include all patients that are in the desired ISA
    cISAData      <- cSimDataAna[[nISA]]

    #cISAData$vISA <- rep( nISA, length( cISAData$vTrt) )

    #2nd get the control patients for all other ISAs
    nQtyISA <-  length( cSimDataAna )
    vISA <- 1:nQtyISA
    vISA <- vISA[ vISA != nISA ]


    nQtyOut <- length( cISAData )


    for( iOut in 1:nQtyOut )
    {

        cISAData[[iOut]]$vISA <- rep( nISA, length(cISAData[[iOut]]$vTrt ))

    }


    for( iISA in vISA )
    {
        cSourceISA <- cSimDataAna[[ iISA]]
        for( iOut in 1:nQtyOut )
        {
            if( length( cSourceISA) > 0 )  #=0 means there is no data to add.
                cISAData[[iOut]] <- AddControlData( cISAData[[iOut]], cSourceISA[[iOut]], iISA )

        }
    }

    return( cISAData )

}

#This function takes the data created with  CreateDataForAnalysis and an ISA and returns data for only that ISA



#cSimDataAna <- lDataAna, nISA
#This version will go through all the outcomes in lSimDataAna (eg trial data) and subset
#'
#' #' @export
#' SubsetDataAllControls <- function( cSimDataAna, nISA )
#' {
#'     #First include all patients that are in the desired ISA
#'     cISAData      <- cSimDataAna[[nISA]]
#'
#'     #cISAData$vISA <- rep( nISA, length( cISAData$vTrt) )
#'
#'     #2nd get the control patients for all other ISAs
#'     nQtyISA <-  length( cSimDataAna )
#'     vISA <- 1:nQtyISA
#'     vISA <- vISA[ vISA != nISA ]
#'
#'
#'     nQtyOut <- length( cISAData )
#'
#'
#'     for( iOut in 1:nQtyOut )
#'     {
#'
#'         cISAData[[iOut]]$vISA <- rep( nISA, length(cISAData[[iOut]]$vTrt ))
#'
#'     }
#'
#'
#'     for( iISA in vISA )
#'     {
#'         cSourceISA <- cSimDataAna[[ iISA]]
#'         for( iOut in 1:nQtyOut )
#'         {
#'
#'             cISAData[[iOut]] <- AddControlData( cISAData[[iOut]], cSourceISA[[iOut]], iISA )
#'
#'         }
#'     }
#'
#'     #nQtyOutcomes <- length( lISAData )
#'     #lRetData        <- structure( list(), class="ISAData")
#'
#'     #iOut <- 1
#'     #for( iOut in 1:nQtyOutcomes )
#'     #{
#'     #cData <- SubsetData( cSimDataAna[[ iOut ]], nISA )
#'
#'     #   lRetData[[paste("lDataOut", iOut, sep="")]] <- lISAData[[ paste( "mOut", iOut,sep="") ]]
#'     #}
#'     return( cISAData )
#'
#' }
#'

#'
#' SubsetData.ProcessReptMeasChngBaseline <- function( cSimDataAna, nISA )
#' {
#'     #print( "SubsetData.ProcessReptMeasChngBaseline ")
#'
#'
#'     cRetData        <- cSimDataAna
#'
#'     cRetData$vOut      <- cRetData$vOut[  cRetData$vISA == nISA ]
#'     cRetData$vTime     <- cRetData$vTime[ cRetData$vISA == nISA ]
#'     cRetData$vTrt      <- cRetData$vTrt[  cRetData$vISA == nISA ]
#'     cRetData$vIND      <- cRetData$vIND[  cRetData$vISA == nISA ]
#'     cRetData$vBaseline <- cRetData$vBaseline[  cRetData$vISA == nISA ]
#'     cRetData$vISA      <- cRetData$vISA[  cRetData$vISA == nISA ]
#'     cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )
#'     return( cRetData )
#'
#' }
#' SubsetData.ProcessSingleTimeOutcome <- function( cSimDataAna, nISA )
#' {
#'     #print( "SubsetData.ProcessSingleTimeOutcome")
#'
#'
#'     cRetData        <- cSimDataAna
#'
#'     cRetData$vOut      <- cRetData$vOut[  cRetData$vISA == nISA ]
#'     cRetData$vTime     <- cRetData$vTime[ cRetData$vISA == nISA ]
#'     cRetData$vTrt      <- cRetData$vTrt[  cRetData$vISA == nISA ]
#'     cRetData$vIND      <- cRetData$vIND[  cRetData$vISA == nISA ]
#'     cRetData$vISA      <- cRetData$vISA[  cRetData$vISA == nISA ]
#'     cRetData$nQtyPats  <- length( unique( cRetData$vIND ) )
#'     return( cRetData )
#'
#' }
#'

