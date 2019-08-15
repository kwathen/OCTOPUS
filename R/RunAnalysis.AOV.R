##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name RunAnalysis.AOV
#' @title RunAnalysis.AOV
#' @description {This is the method used for running the Analysis of Variance (AOV) .
#' Specifically, lm( vOut ~ vBaseline + vTrt,na.action = na.omit)"  }
#' @export
RunAnalysis.AOV <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{

    #print( "RunAnalysis.AOV")

    bPlacMinusTrt   <- cAnalysis$bPlacMinusTrt
    lCI             <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI        <- lCI$dLowerCI
    dUpperCI        <- lCI$dUpperCI

    #print( paste( "RunAnalysis.AOV CI ", dLowerCI, " ", dUpperCI))
    dMAV            <- cAnalysis$dMAV
    dTV             <- cAnalysis$dTV

    vOut            <- lDataAna$vOut
    vBaseline       <- lDataAna$vBaseline
    vTrt            <- as.factor(lDataAna$vTrt)
    vTime           <- as.factor(lDataAna$vTime)
    vIND            <- lDataAna$vIND

    fit             <- lm( vOut ~ vBaseline + vTrt,na.action = na.omit)
    vCoeff          <- coef( fit )

    # Assuming 1 is the control treatment
    vUniqT          <- unique( lDataAna$vTrt )
    nTrt            <- vUniqT[ vUniqT != 1 ]
    strTrt          <- paste( "vTrt", nTrt, sep="")

    dEst            <- sum( vCoeff[c(strTrt)])
    nDOF            <- fit$df.residual
    vNames          <- c("(Intercept)", strTrt)
    dSE             <-  sqrt(sum(vcov(fit)[ strTrt, strTrt ]))

    if( bPlacMinusTrt )
        dEst <- dEst*-1

    dTStat      <- dEst/dSE
    dPVal       <- pt( dTStat, nDOF )
    dCILow <- dEst - abs(qt(dLowerCI, nDOF)) * dSE

    if( dUpperCI < 1.0 )
    {
        dCIUp <- dEst + qt(dUpperCI, nDOF) * dSE
        lRet  <- MakeDecisionBasedOnCI( dCILow, dCIUp, lAnalysis = list( dMAV = dMAV, dTV = dTV ))
    }
    else #The Upper CI = 1 --> only use the lower limit compared to MAV
    {
        dCIUp <- 99999
        nGo <- nNoGo <- nPause <- 0
        if( dCILow > dMAV )
            nGo   <- 1
        else
            nNoGo <- 1

        lRet <- list( nGo = nGo, nNoGo = nNoGo, nPause = nPause)
    }

    vRet <- list(dEstimate = dEst, dStdErr = dSE, dTStat = dTStat, dPVal = dPVal, nDOF = nDOF,
                 dCILow = dCILow, dCIUp = dCIUp, nGO = lRet$nGo, nNoGo = lRet$nNoGo,
                 nPause = lRet$nPause,  cRandomizer = cRandomizer )
    names(vRet)[5:10] <- c("nDOF", "dCILow", "dCIUp", "nGo", "nNoGo","nPause")
    return( vRet )



}
