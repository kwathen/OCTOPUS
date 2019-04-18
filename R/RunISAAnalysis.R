##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

############### ISA Level Analysis  ##################################################################################################
#  ISA level analysis will go through each ISA and run all the analysis

#' @export
RunISAAnalysis <- function( cISAAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis )
{
    UseMethod( "RunISAAnalysis", cISAAnalysis )
}




#' @name RunISAAnalysis.default
#' @title RunISAAnalysis.default
#' @description  The cISAAnalysis object has a list of Analysis...
#' @export
RunISAAnalysis.default <- function( cISAAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis  )
{
    #print( "RunISAAnalysis.Independent")

    nQtyAna <- length( cISAAnalysis$vAnalysis )
    iAna    <- 1
    lResAna <- list()
    repeat
    {

        #print( paste( "Analysis for outcome ", iAna))
        lTmpAna      <- cISAAnalysis$vAnalysis[[ iAna ]]

        lTmpData     <- lDataAna[[ iAna ]]
        # lArgs        <- list( cAnalysis = lTmpAna, lDataAna = lTmpData, nFinalAnalysis = nFinalAnalysis )

        #print( paste( "Independent bIsFinalAnalysis ",bIsFinalISAAnalysis, sep =""))
        lAnaRes      <- RunAnalysis( cAnalysis=lTmpAna, lDataAna = lTmpData, nISAAnalysisIndx = nISAAnalysisIndx,  bIsFinalISAAnalysis= bIsFinalISAAnalysis )
        #lAnaRes      <- do.call( lTmpAna$strAnalysisMethod, lArgs )
        lResAna[[paste("lAnalysis", iAna, sep="")]] <- lAnaRes

        if( iAna == nQtyAna )
            break
        iAna <- iAna + 1
    }

    #Write tests for this function
    return(lResAna )
}


RunISAAnalysis.DoseRanging <- function( cISAAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis  )
{
    #print( "RunISAAnalysis.DoseRanging")

    #This is just an example to show that we could first do a dose rangign type analysis then the decide how/when
    #to do the primary analysis
    nQtyAna <- length( cISAAnalysis$vAnalysis )
    iAna    <- 1
    lResAna <- list()
    repeat
    {
        lTmpAna      <- cISAAnalysis$vAnalysis[[ iAna ]]
        lTmpData     <- lDataAna[[ iAna ]]
        # lArgs        <- list( cAnalysis = lTmpAna, lDataAna = lTmpData, nFinalAnalysis = nFinalAnalysis )
        lAnaRes      <- RunAnalysis( cAnalysis=lTmpAna, lDataAna = lTmpData,nISAAnalysisIndx = nISAAnalysisIndx,  bIsFinalISAAnalysis= bIsFinalISAAnalysis  )
        #lAnaRes      <- do.call( lTmpAna$strAnalysisMethod, lArgs )
        lResAna[[paste("lAnalysis", iAna, sep="")]] <- lAnaRes

        if( iAna == nQtyAna )
            break
        iAna <- iAna + 1
    }

    #Write tests for this function
    return(lResAna )
}
