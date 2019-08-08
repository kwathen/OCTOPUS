##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @export
RunTrialAnalysis.IndependentISA <- function( cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime,  vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
{
    nQtyISA <- length( cISADesigns )

    if( all( vRunISAAnalysis  == 0 ) )
    {
        #None of the ISAs are ready or need an analysis run
        for( iISA in 1:nQtyISA )
        {
            lISARes <- list( nGo = 1, nNoGo = 0, nPause = 0)
        }

    }
    lDataAna <-  CreateDataSetForAnalysis( cEnrolledPats, dCurrentTime, vISAStatus )

    #vMinMet     <- ifelse( dCurrentTime >= vStartISAAnalysis & vStartISAAnalysis >0, TRUE, FALSE )

    #print("RunTrialAnalysis.IndependentISA")

    lResISA <- list()
    nISA <- 1

    for( nISA in 1:nQtyISA)
    {
        lISARes <- list()
        #print( paste( "RunTrialAna - ISA ", nISA,  " vISAStatus", vISAStatus[nISA]))

        #TODO: Should we run the Analysis for an ISA anytime the vRunISAAnalysis = 1?


        if( vRunISAAnalysis[ nISA ] == 1 &&  vISAStatus[ nISA ] <=2 )   #If the minimum is met and the ISA is open then do the analysis
        {
            #print( paste( ".....Case 1 - Running analysis for ISA ", nISA, " ISAStatus ", vISAStatus[ nISA ]))
            cISAAnalysis <- cISADesigns[[ nISA ]]$cISAAnalysis
            #print( paste( "Data for trial "))
            #print( lDataAna )
            lDataTmp <- SubsetData( lDataAna, nISA ) #cISAAnalysis$nISA )
            #lDataTmp<<- lDataTmp
            #print( paste( "Data for ISA ", cISAAnalysis$nISA ))
            #print( lDataTmp)
            #if( vIsFinalISAAnalysis[ nISA] )
            #    print( sprintf( "FINAL ANALYSIS for ISA %f ", nISA))


            ####  TODO(Kyle) - This use to have vIsFinalISA which has been replaced by index but may not do all that is necessaru #####

            #print( paste( ".....IndependentISA vISAAnalysisIndx ", vISAAnalysisIndx[ nISA], " IsFinalISA ", vIsFinalISAAnalysis[ nISA ], sep =""))
            lISARes <- RunISAAnalysis( cISAAnalysis, lDataTmp, vISAAnalysisIndx[ nISA],  vIsFinalISAAnalysis[ nISA ]  )

            vISAAnalysisIndx[ nISA ] <- vISAAnalysisIndx[ nISA ] + 1

        }
        else if(  vISAStatus[ nISA ] <= 1  )
        {
            #print( ".....Case 2 - Analysis not being run for ISA ", nISA )

            cISAAnalysis <- cISADesigns[[ nISA ]]$cISAAnalysis

            nQtyAna <- length( cISAAnalysis$vAnalysis )
            iAna    <- 1
            lISARes <- list()
            repeat
            {
                lISARes[[paste("lAnalysis", iAna, sep="")]] <- list( nGo = 0, nNoGo = 0, nPause = 1)

                if( iAna == nQtyAna )
                    break
                iAna <- iAna + 1
            }

        }
        else
        {

            #print( "Case 3")
        }
        lISARes$bISAAnalysisRun <- (vRunISAAnalysis[ nISA ] == 1)
        lResISA[[paste("lResISA", nISA, sep="")]] <- lISARes



    }
    lRet <- list( lResISA = lResISA, vISAAnalysisIndx = vISAAnalysisIndx )
    return( lRet )

}
