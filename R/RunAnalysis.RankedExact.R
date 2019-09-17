##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name RunAnalysis.RankedExact
#' @title RunAnalysis.RankedExact
#' @description {This is the method used for running the Ranked analysis using wilcox_test from the coin package.
#' Specifically, wilcox_test (DO NO CONFUSE with wilcox.test) with exact = TRUE"  }
#' @export
RunAnalysis.RankedExact <- function( cAnalysis, lDataAna,   nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{


    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI

    # print( paste( "RunAnalysis.Ranked CI ", dLowerCI, " ", dUpperCI))

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    nTrt        <- vUniqT[ vUniqT != 1 ]
    if( dLowerCI  == 1 - dUpperCI )
    {
        dCILevel <- dUpperCI - dLowerCI
        #wt       <- wilcox_test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )
        wt        <- wilcox_test( lDataAna$vOut ~ as.factor(lDataAna$vTrt), conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )
        dEstimate <- statistic(wt)[1]
        dLower    <- confint(wt)$conf.int[1]
        dUpper    <- confint(wt)$conf.int[2]

    }
    else
    {
        #The desired CI is not symmetrical so we need to do the test twice

        #Get the CI for the Lower Limit
        dCILevel  <- 1 - 2*dLowerCI
        #wt       <- wilcox_test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )
        wt        <- wilcox_test( lDataAna$vOut ~ as.factor(lDataAna$vTrt), conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )

        dEstimate <- statistic(wt)[1]
        dLower    <- confint(wt)$conf.int[1]

        #Get the Upper Limit
        dCILevel <- 1 - 2*(1 - dUpperCI)
        # wt       <- wilcox_test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==nTrt ], conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )
        wt       <- wilcox_test( lDataAna$vOut ~ as.factor(lDataAna$vTrt),  conf.int=TRUE, conf.level = dCILevel,  distribution='exact' )

        dUpper   <- confint(wt)$conf.int[2]

    }
    lRet <- MakeDecisionBasedOnCI( dLower, dUpper, cAnalysis )



    return( list(nGo = lRet$nGo, nNoGo = lRet$nNoGo, nPause = lRet$nPause,
                 dEstimate = dEstimate, dCILow= dLower, dCIUp= dUpper, cRandomizer = cRandomizer))


}
