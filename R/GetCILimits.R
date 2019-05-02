##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name GetCILimits
#' @title GetCILimits
#' @description { This function used to obtain the CI limits based on the analysis index and/or if it is the final analysis for an ISA. }
#' @export
GetCILimits <- function( lAnalysis,  nISAAnalysisIndx, bIsFinalISAAnalysis  )
{

    ###################################################################################.
    #  Warning: This is a test case to see if we can allow for the cutoffs to change per the nIA
    #  THIS NEEDS TO BE GENERALIZED FOR MULTIPLE ISAs THIS SETS ALL ISAS AT THE SAME IA
    #############################################################################################.

    #    dUpperCI        <- lAnalysis$vUpperCI[nFinalAnalysis]
    #    dLowerCI        <- lAnalysis$vLowerCI[nFinalAnalysis]

    #   print( paste( "nFinalISA ", nFinalAnalysis, " Lower ", dLowerCI, " Upper IA ", dUpperCI))
    if(  bIsFinalISAAnalysis == FALSE  )
    {
        if( nISAAnalysisIndx > length( lAnalysis$vUpperCI ) )
        {
            nIndx <- length( lAnalysis$vUpperCI )
        }
        else
        {
            nIndx <- nISAAnalysisIndx
        }
        dUpperCI        <- lAnalysis$vUpperCI[ nIndx ]
        dLowerCI        <- lAnalysis$vLowerCI[ nIndx ]
    }
    else
    {
        dUpperCI        <- lAnalysis$dFinalUpperCI
        dLowerCI        <- lAnalysis$dFinalLowerCI
    }


    return( list( dLowerCI = dLowerCI, dUpperCI = dUpperCI) )
}
