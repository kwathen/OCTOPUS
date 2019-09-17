##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

############### ISA Level Analysis  ##################################################################################################
#  ISA level analysis will go through each ISA and run all the analysis

#' @name RunISAAnalysis
#' @title RunISAAnalysis
#' @description {Generic method called to run the ISA analysis.
#' Because the decision is tied to the analysis, each RunISAAnalysis should contain the elements nGo, nNoGo, nPause as part ot the list retured.}
#' @export
RunISAAnalysis <- function( cISAAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    UseMethod( "RunISAAnalysis", cISAAnalysis )
}




#' @name RunISAAnalysis.default
#' @title RunISAAnalysis.default
#' @description  The cISAAnalysis object has a list of Analysis
#' @export
RunISAAnalysis.default <- function( cISAAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer  )
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
        lAnaRes      <- RunAnalysis( cAnalysis=lTmpAna, lDataAna = lTmpData, nISAAnalysisIndx = nISAAnalysisIndx,  bIsFinalISAAnalysis= bIsFinalISAAnalysis, cRandomizer )

        #TODO(Covs) - Assuming the RunAnalysis return an object with lAnaRes$cRandomizer and there are multiple outcomes how do we want to combine the different decisions,
        # for example outcome 1 says to stop group 1, outcome 2 does not what do we want to do with the cRanomzier then


        #TODO(Covs) - Working here, at this point we have the cRandomizer in lAnaRes for each analysis and then copy it to the the return object
        #but dont remove it.   Once the are combined then the lAnaRs$cRandomizer should be remove -- current problem here
        lResAna[[paste("lAnalysis", iAna, sep="")]] <- lAnaRes

        if( iAna == nQtyAna )
            break
        iAna <- iAna + 1
    }
    # if( nQtyAna == 1 )
    # {
    #     lResAna <- lResAna[ names( lResAna ) != "cRandomizer"]   #Dropping the randomzier in th lResAna becuase it will be coppied to the return part
    # }

    #TODO(Covs) - for now assuming 1 outcome and will copy the lAnaRes to the return
    #lResAna$cRandomizer <- lAnaRes$cRandomizer


    #Write tests for this function
    return(lResAna )
}


#' @name RunISAAnalysis.DoseRanging
#' @title RunISAAnalysis.DoseRanging
#' @description {Generic method called to run the ISA analysis.  }
#' @export
RunISAAnalysis.DoseRanging <- function( cISAAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer  )
{
    #print( "RunISAAnalysis.DoseRanging")

    #This is just an example to show that we could first do a dose ranging type analysis then the decide how/when
    #to do the primary analysis
    nQtyAna <- length( cISAAnalysis$vAnalysis )
    iAna    <- 1
    lResAna <- list()
    repeat
    {
        lTmpAna      <- cISAAnalysis$vAnalysis[[ iAna ]]
        lTmpData     <- lDataAna[[ iAna ]]
        # lArgs        <- list( cAnalysis = lTmpAna, lDataAna = lTmpData, nFinalAnalysis = nFinalAnalysis )
        lAnaRes      <- RunAnalysis( cAnalysis=lTmpAna, lDataAna = lTmpData,nISAAnalysisIndx = nISAAnalysisIndx,  bIsFinalISAAnalysis= bIsFinalISAAnalysis, cRandomizer  )
        #lAnaRes      <- do.call( lTmpAna$strAnalysisMethod, lArgs )
        lResAna[[paste("lAnalysis", iAna, sep="")]] <- lAnaRes

        if( iAna == nQtyAna )
            break
        iAna <- iAna + 1
    }

    #Write tests for this function
    return(lResAna )
}
