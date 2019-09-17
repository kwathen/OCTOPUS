##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#Analysis model for MCPMod
# cAnalysis must have cAnalysis$vDose for class of type MCPMod
#' @export
RunAnalysis.MCPModAnalysis <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    print( "RunAnalysis.MCPMod ")
    #print( lDataAna )
    bPlacMinusTrt <- cAnalysis$bPlacMinusTrt


    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI

    vDoseIndx <-  match( lDataAna$vTrt, cAnalysis$vTrtLab)
    vPatDose  <- cAnalysis$vDose[ vDoseIndx ]


    resp     <- lDataAna$vOut[ lDataAna$vTime == cAnalysis$dTimePoint ]
    vPatDose <- vPatDose[ lDataAna$vTime == cAnalysis$dTimePoint ]
    data     <- data.frame( dose= vPatDose, resp = resp )
    dose     <- cAnalysis$vDose
    mcpFit   <- FitMCPMod(dose =dose, resp = resp, data = data, cAnalysis$lModels,mod.sel=1, bPlacMinusTrt, dLowerCI, dUpperCI )

    plot( dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlac, type ='l', lwd=2, xlab="Dose", ylab="Delta from Placebo",
          ylim= c(min( mcpFit$lCIDiff$vPredDiffPlacCILow), max(mcpFit$lCIDiff$vPredDiffPlacCIUp) ))
    lines(dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlacCILow, lty=2)
    lines(dose[ dose != 0 ], mcpFit$lCIDiff$vPredDiffPlacCIUp, lty=2)

    title( paste( "N=", lDataAna$nQtyPats), outer=TRUE, line=-2)
    nQtyEst  <- length( mcpFit$lCIDiff$vPredDiffPlac )
    lDoseDec <- structure(list(), class=class(cAnalysis))
    for( i in 1:nQtyEst )
        lDoseDec[[i]] <- MakeDecisionBasedOnCI(mcpFit$lCIDiff$vPredDiffPlacCILow[ i ], mcpFit$lCIDiff$vPredDiffPlacCIUpper[ i ], cAnalysis)

    #print( mcpFit$fitModel )
    lRet <- MakeDecisionMCPMod( lDoseDec )
    #print( paste("Data for MCPMod"))
    #print( vPatOut )
    #print( vPatDose)
    #mcpFit <-  FitMCPMod(dose =cAnalysis$vDose, vPatDose = vPatDose,resp= vPatOut,cAnalysis$lModels,mod.sel=1)
    #print( mcpFit )

    lRet$cRandomizer <- cRandomizer
    return( lRet )

}
