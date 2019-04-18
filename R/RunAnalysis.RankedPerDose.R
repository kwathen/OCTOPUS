##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @export
RunAnalysis.RankedPerDose <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis  )
{
    #print( "RunAnalysis.RankedPerDose ")

    lCI      <- GetCILimits(  cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis  )
    dLowerCI <- lCI$dLowerCI
    dUpperCI <- lCI$dUpperCI

    dCILevel <- dUpperCI - dLowerCI


    # Assuming 1 is the control treatment
    #vUniqT      <- sort(unique( lDataAna$vTrt ))
    vTrt        <- sort( cAnalysis$vTrtLab )
    vTrt        <- vTrt[ vTrt != 1 ]
    #vTrt        <- vUniqT[ vUniqT != 1 ]

    nQtyDoses <- length( vTrt )
    lAllRet   <-  structure( list(), class=class( cAnalysis))

    lDoseDec    <- structure(list(), class=class(cAnalysis))
    for( iTrt in vTrt  )
    {
        nQty0 <- length( lDataAna$vOut[lDataAna$vTrt==1] )
        nQty1 <- length( lDataAna$vOut[lDataAna$vTrt==iTrt] )

        #print( paste( "n placebo: ", nQty0, ", n trt ", iTrt, ", ", nQty1))
        if( nQty0 >5 && nQty1 > 5 )
        {
            if( dLowerCI  == 1 - dUpperCI )  #Symmetrical CI so only need to do the test once to get the desired CI
            {
                wt <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==iTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
                dLower <-  wt$conf.int[[1]]
                dUpper <- wt$conf.int[[2]]
                dEst   <- wt$estimate
            }
            else
            {

                #Get the CI for the Lower Limit
                dCILevel <- 1 - 2*dLowerCI
                wt       <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==iTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
                dLower   <- wt$conf.int[[1]]
                dEst   <- wt$estimate

                #Get the Upper Limit
                dCILevel <- 1 - 2*(1 - dUpperCI)
                wt       <- wilcox.test( lDataAna$vOut[lDataAna$vTrt==1],  lDataAna$vOut[lDataAna$vTrt==iTrt ], conf.int=TRUE, conf.level = dCILevel, exact=TRUE )
                dUpper   <- wt$conf.int[[2]]
            }

            lRet <- MakeDecisionBasedOnCI( dLower, dUpper, cAnalysis )

            lAllRet[[ paste( "lRet", iTrt,sep="")]] <- lRet
        }
        else
        {
            dEst <- NA
            dLower <- NA
            dUpper <- NA
            lRet <- list( nGo =0, nNoGo = 0, nPause = 1)
        }

        lDoseDec[[paste("lDec",iTrt, sep="")]] <- list( nGo = lRet$nGo, nNoGo = lRet$nNoGo, nPause = lRet$nPause, dEst = dEst, dCILower = dLower, dCIUpper = dUpper)
        #lRet2[[paste( "lRet", iTrt,sep="")]] <- list( dEst = dEst, dCILower = dLower, dCIUpper = dUpper )
        #lRet2[[paste( "lDoseDec", iTrt,sep="")]] <- list(   )

    }
    print( paste( "Outcome Dec ", lAllRet))
    lRetObj <- MakeDecisionDoses( lAllRet )
    print( paste( "lRetOb ", lRetObj))

    if(!is.null(cAnalysis$nVerboseOutput) && cAnalysis$nVerboseOutput== 1)
    {
        lRet2 <- list( lDoseDec = lDoseDec)
        lRetObj[["lRet2"]] <- lRet2
    }

    #print( paste( "CI ", dLower, " ", dUpper, " TV ", lAnalysis$dTV, " ", nSuccess, " ", nFutility, " ", nPause ))

    #return( list( nSuccess = nSuccess, nFutility = nFutility, nPause = nPause,
    #              dPlac12 = lTTest$estimate[[1]], dTrt12 = lTTest$estimate[[2]],
    #              dCILow = dCILow, dCIUp = dCIUp))
    return( lRetObj )

}
