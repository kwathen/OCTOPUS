##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.
# cAnalysis must have cAnalysis$vDose for class of type MCPMod
#' @export
#'
RunAnalysis.ReptMeasAnalysisPerDose <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer  )
{
    #print( "RunAnalysis.ReptMeasAnalysisPerDose ")

    #print( "RunAnalysis.ReptMeasAnalysis ")
    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis  )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI
    dMAV     <- cAnalysis$dMAV
    dTV      <- cAnalysis$dTV

    vOut        <- lDataAna$vOut
    vBaseline   <- lDataAna$vBaseline
    vTrt        <- as.factor(lDataAna$vTrt)
    vTime       <- as.factor(lDataAna$vTime)
    vIND        <- lDataAna$vIND

    # Assuming 1 is the control treatment
    #lDataAna$vTrt[1:3] <- c(3,1,2)
    vUniqT      <- sort( unique( lDataAna$vTrt ) )
    nTrt        <- vUniqT[ vUniqT != 1 ]

    fit <-   gls(vOut ~ vBaseline + vTrt + vTime + vTrt:vTime,
                 weights = varIdent(form = ~1|vTime),
                 correlation = corSymm(form=~1|vIND), na.action = na.omit ) #, silent=TRUE) #corr = corSymm(form = ~as.integer(Time)|IDN)), silent = TRUE )
    nQtyTimePts <- length( lDataAna$vObsTime )

    bPlacMinusTrt <- cAnalysis$bPlacMinusTrt

    lRet <- structure( list(), class=class( cAnalysis))
    for( i in 1:length( nTrt ) )
        lRet[[paste("lRet", nTrt[i],sep="")]] <-  c(GetLSDiffGLS( fit, nTrt[i], lDataAna$vObsTime[ nQtyTimePts], dMAV, dTV, dLowerCI, dUpperCI,bPlacMinusTrt ), nAnalysisUsed = 5)
    #lRet <<- lRet


    lRetObj <- MakeDecisionDoses( lRet )


    if(!is.null(cAnalysis$nVerboseOutput) && cAnalysis$nVerboseOutput== 1)
    {
        lRetObj[["lRet2"]] <- lRet
    }
    #print( lRetComb )
    #lRet2 <-  c(GetLSDiffGLS( fit, nTrt[2], lDataAna$vObsTime[ nQtyTimePts], dMAV, dTV, dLowerCI, dUpperCI,bPlacMinusTrt ), nAnalysisUsed = 5)
    #lRet2 <<- lRet2
    return( lRetObj )
    #
    # #print( lDataAna )
    # bPlacMinusTrt <- cAnalysis$bPlacMinusTrt
    #
    # lCI      <- GetCILimits(  cAnalysis, bFinalAnalysis )
    #
    # dLowerCI      <- lCI$dLowerCI
    # dUpperCI      <- lCI$dUpperCI
    #
    #
    # vDoseIndx <-  match( lDataAna$vTrt, cAnalysis$vTrtLab)
    # vPatDose  <- cAnalysis$vDose[ vDoseIndx ]
    #
    #
    # vUniqueTrt <- unique( lDataAna$vTrt )
    # #Now we want to fit the repeated measure on each dose
    #
    # resp     <- lDataAna$vOut[ lDataAna$vTime == cAnalysis$dTimePoint ]
    # vPatDose <- vPatDose[ lDataAna$vTime == cAnalysis$dTimePoint ]
    # data     <- data.frame( dose= vPatDose, resp = resp )
    # dose     <- cAnalysis$vDose
    # mcpFit   <- FitMCPMod(dose =dose, resp = resp, data = data, cAnalysis$lModels,mod.sel=1, bPlacMinusTrt, dLowerCI, dUpperCI )
    # mcpFit   <<- mcpFit
    # plot( dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlac, type ='l', lwd=2, xlab="Dose", ylab="Delta from Placebo",
    #       ylim= c(min( mcpFit$lCIDiff$vPredDiffPlacCILow), max(mcpFit$lCIDiff$vPredDiffPlacCIUp) ))
    # lines(dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlacCILow, lty=2)
    # lines(dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlacCIUp, lty=2)
    #
    # title( paste( "N=", lDataAna$nQtyPats), outer=TRUE, line=-2)
    # nQtyEst  <- length( mcpFit$lCIDiff$vPredDiffPlac )
    # lDoseDec <- structure(list(), class=class(cAnalysis))
    # for( i in 1:nQtyEst )
    #     lDoseDec[[i]] <- MakeDecisionBasedOnCI(mcpFit$lCIDiff$vPredDiffPlacCILow[ i ], mcpFit$lCIDiff$vPredDiffPlacCIUpper[ i ], cAnalysis)
    # lDoseDec <<- lDoseDec
    # print( mcpFit$fitModel )
    # lRet <- MakeDecisionMCPMod( lDoseDec )
    #
    # return( lRet )

}
