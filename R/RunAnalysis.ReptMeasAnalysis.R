##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name RunAnalysis.ReptMeasAnalysis
#' @title RunAnalysis.ReptMeasAnalysis
#' @description {This is the method used for running the Repeated Measure Analysis .
#' Specifically, gls(vOut ~ vBaseline + vTrt + vTime + vTrt:vTime,
#' weights = varIdent(form = ~1|vTime),
#' correlation = corSymm(form=~1|vIND), na.action = na.omit )"  }
#' @export
RunAnalysis.ReptMeasAnalysis <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer)
{

    #print( "RunAnalysis.ReptMeasAnalysis ")
    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI

    #print( paste( "RunAnalysis.ReptMeas CI ", dLowerCI, " ", dUpperCI))

    dMAV     <- cAnalysis$dMAV
    dTV      <- cAnalysis$dTV

    vOut        <- lDataAna$vOut
    vBaseline   <- lDataAna$vBaseline
    vTrt        <- as.factor(lDataAna$vTrt)
    vTime       <- as.factor(lDataAna$vTime)
    vIND        <- lDataAna$vIND

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    nTrt        <- vUniqT[ vUniqT != 1 ]

    fit <-   gls(vOut ~ vBaseline + vTrt + vTime + vTrt:vTime,
                 weights = varIdent(form = ~1|vTime),
                 correlation = corSymm(form=~1|vIND), na.action = na.omit ) #, silent=TRUE) #corr = corSymm(form = ~as.integer(Time)|IDN)), silent = TRUE )
    nQtyTimePts <- length( lDataAna$vObsTime )

    bPlacMinusTrt <- cAnalysis$bPlacMinusTrt
    lRet <- c(GetLSDiffGLS( fit, nTrt[1], lDataAna$vObsTime[ nQtyTimePts], dMAV, dTV, dLowerCI, dUpperCI,bPlacMinusTrt ), nAnalysisUsed = 5)
    lRet$cRandomizer <-cRandomizer
    return( lRet  )


}
