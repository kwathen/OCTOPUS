##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @export
RunAnalysis.AOVPerDose <- function( cAnalysis, lDataAna, nISAAnalysisIndx, bIsFinalISAAnalysis  )
{

    #print( "RunAnalysis.AOV")

    bPlacMinusTrt   <- cAnalysis$bPlacMinusTrt
    lCI             <- GetCILimits(  cAnalysis,nISAAnalysisIndx, bIsFinalISAAnalysis  )
    dLowerCI        <- lCI$dLowerCI
    dUpperCI        <- lCI$dUpperCI
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
    vUniqT          <- sort(unique( lDataAna$vTrt ))
    vTrt            <- vUniqT[ vUniqT != 1 ]
    nQtyDoses       <- length( vTrt )

    lAllRet <- structure( list(), class=class( cAnalysis))
    lRet2   <- list()  #List of detailed info to return
    for( iDose in 1:nQtyDoses )
    {
        strTrt          <- paste( "vTrt", vTrt[iDose], sep="")

        dEst            <- sum( vCoeff[c(strTrt)])
        nDOF            <- fit$df.residual
        vNames          <- c("(Intercept)", strTrt)
        dSE             <-  sqrt(sum(vcov(fit)[ vNames, vNames ]))

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
        lRet2[[ paste( "lRet", iDose,sep="") ]] <- list( dEst = dEst, dCILow = dCILow, dCIUpper = dCIUp )
        lAllRet[[ paste( "lRet", iDose,sep="")]] <- lRet
    }
    lRet <- MakeDecisionDoses( lAllRet )



    if(!is.null(cAnalysis$nVerboseOutput) && cAnalysis$nVerboseOutput== 1)
    {
        lRet[["lRet2"]] <- lRet2
    }

    return( lRet )



}

