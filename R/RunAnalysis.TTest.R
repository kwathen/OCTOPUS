##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name RunAnalysis.TTest
#' @title RunAnalysis.TTest
#' @description {This is the method used for running the Repeated Measure Analysis .
#' Specifically,
#' t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("two.sided"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)"  }
#' @export
RunAnalysis.TTest <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis )
{


    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI
    if( dLowerCI  == 1 - dUpperCI )  #Symmetrical CI so only need to do the test once to get the desired CI
    {
        dCILevel <- dUpperCI - dLowerCI


        lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("two.sided"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
        Means   <- lTTest["estimate"]$estimate[[1]] - lTTest["estimate"]$estimate[[2]] #pbo- trt
        dLower  <- lTTest["conf.int"]$conf.int[[1]]
        dUpper  <- lTTest["conf.int"]$conf.int[[2]]
    }
    else
    {
        #The desired CI is not symmetrical so we need to do the test twice

        #Get the CI for the Lower Limit
        dCILevel <- 1 - 2*dLowerCI
        lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("two.sided"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
        Means   <- lTTest["estimate"]$estimate[[1]] - lTTest["estimate"]$estimate[[2]] #pbo- trt
        dLower  <- lTTest["conf.int"]$conf.int[[1]]


        dCILevel <- 1 - 2*(1 - dUpperCI)
        lTTest  <- t.test(lDataAna$vOut ~ as.factor(lDataAna$vTrt), alternative = c("two.sided"),mu = 0, exact = NULL, correct = TRUE,  conf.int = TRUE, conf.level = dCILevel)
        dUpper  <- lTTest["conf.int"]$conf.int[[2]]

    }

    lRet <- MakeDecisionBasedOnCI( dLower, dUpper, cAnalysis )


    return( lRet )

}
