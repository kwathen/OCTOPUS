##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name RunAnalysis.Ranked
#' @title RunAnalysis.Ranked
#' @description {This is the method used for running the Ranked analysis. Specifically, wilcox.test with exact = TRUE"  }
#' @export
RunAnalysis.Ranked <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{


    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis)
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    nTrt        <- vUniqT[ vUniqT != 1 ]

    # print( paste( "RunAnalysis.Ranked CI ", dLowerCI, " ", dUpperCI))
    if( dLowerCI  == 1 - dUpperCI )  #Symmetrical CI so only need to do the test once to get the desired CI
    {
        dCILevel <- dUpperCI - dLowerCI
        wt       <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
        dLower   <- wt$conf.int[[1]]
        dUpper   <- wt$conf.int[[2]]
    }
    else
    {
        #The desired CI is not symmetrical so we need to do the test twice

        #Get the CI for the Lower Limit
        dCILevel <- 1 - 2*dLowerCI
        wt       <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
        dLower   <- wt$conf.int[[1]]

        #Get the Upper Limit
        dCILevel <- 1 - 2*(1 - dUpperCI)
        wt       <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
        dUpper   <- wt$conf.int[[2]]
    }

    lRet <- MakeDecisionBasedOnCI( dLower, dUpper, cAnalysis )

    #print( paste( "CI ", dLower, " ", dUpper, " TV ", lAnalysis$dTV, " ", nSuccess, " ", nFutility, " ", nPause ))

    return( list(nGo = lRet$nGo, nNoGo = lRet$nNoGo, nPause = lRet$nPause,
                 dEstimate = wt$estimate[[1]], dCILow= wt$conf.int[1], dCIUp= wt$conf.int[2], cRandomizer = cRandomizer))
    return( lRet )

}
