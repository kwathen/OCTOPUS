##### COPYRIGHT #############################################################################################################.
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


############### Trial Level Analysis ##################################################################################################.
#  Trial level analysis - will go through and conduct the analysis for each ISA.
#  This version is sent all the available data in lDataAna then for each ISA gets the subset
#   that is specific to that ISA.  If we want to do a version that borrows control info
#   a new version could be adapted.
#
#    The cEnrolledPats  is all the data in the trial.  Thus, when sending to an ISA analysis it should first be subset
# it should have the following elements
# $vStartTimes
# $vTrt
# $vISA
# $lPatOut
# $lPatOut$mSimOut1
# [,1]     [,2]    [,3]     [,4]     [,5]
# [1,] 236.0638 211.916 136.4621 218.7731 206.6974
# attr(,"class")
#
# $lPatOut$mSimOut2
# [,1]
# [1,] -1
# attr(,"class")
#
#
# $vCurrentQtyPatsISA
# [1] 0 1
#
# $vTrtLab
# [1] 1 2 3 4 1 2 3 4
#
# $vISALab  #
# [1] 1 1 1 1 2 2 2 2
#
# $vQtyPatsArmISA (this is the vector of # patients in ISA1, ISA2,...)
#
#
# $vObsTime1
# $vObsTime2
#################################################################################################################.

#' @name RunTrialAnalysis
#' @title RunTrialAnalysis
#' @description { RunTrialAnalysis is a generic method to execute the trial level analysis, eg execute all ISA analysis.
#' Any implementation of RunTrialAnalysis must increment vISAAnalysisIndx if an analysis is run for an ISA   }
#' @export
RunTrialAnalysis <- function( cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime,  vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
{
    UseMethod( "RunTrialAnalysis", cISADesigns )
}



# The cEnrolledPats  is all the data in the trial.  Thus, when sending to an ISA analysis it should first be subset
# it should have the following elements
# $vStartTimes
# $vTrt
# $vISA
# $lPatOut
# $lPatOut$mSimOut1
# [,1]     [,2]    [,3]     [,4]     [,5]
# [1,] 236.0638 211.916 136.4621 218.7731 206.6974
# attr(,"class")
#
# $lPatOut$mSimOut2
# [,1]
# [1,] -1
# attr(,"class")
#
#
# $vCurrentQtyPatsISA
# [1] 0 1
#
# $vTrtLab
# [1] 1 2 3 4 1 2 3 4
#
# $vISALab  #
# [1] 1 1 1 1 2 2 2 2
#
# $vQtyPatsArmISA (this is the vector of # patients in ISA1, ISA2,...)
#
#
# $vObsTime1
#$vObsTime2


#' @name RunTrialAnalysis
#' @title RunTrialAnalysis
#' @description { RunTrialAnalysis is a generic method to execute the trial analysis.
#' Any implementation of RunTrialAnalysis must increment vISAAnalysisIndx if an analysis is run for an ISA.
#' Trial level analysis - will go through and conduct the analysis for each ISA.
#'  This version is sent all the available data in lDataAna then for each ISA gets the subset
#'   that is specific to that ISA.  If we want to do a version that borrows control info
#'   a new version could be adapted.
#'   Because the decision is tied to the analysis, each ISA analysis should contain the elementsnGo, nNoGo, nPause as part ot the list retured.
#'  }
#' @export
RunTrialAnalysis.default <- function( cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime,  vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
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
            # print( paste( ".....Case 1 - Running analysis for ISA ", nISA, " ISAStatus ", vISAStatus[ nISA ]))
            cISAAnalysis <- cISADesigns[[ nISA ]]$cISAAnalysis
            #print( paste( "Data for trial "))
            #print( lDataAna )

            class( lDataAna ) <- class( cISAAnalysis)
            lDataTmp <- SubsetData( lDataAna, nISA ) #cISAAnalysis$nISA )

            ###########################
            #  TODO - This use to have vIsFinalISA which has been replaced by index but may not do all that is necessar

            #TODO(Covs) - Copyithe returned randomizre   cRandomizer[[ nISA]]$dfSubGroupEnrollmentStatus
            lISARes <- RunISAAnalysis( cISAAnalysis, lDataTmp, vISAAnalysisIndx[ nISA],  vIsFinalISAAnalysis[ nISA ], cRandomizer[[ nISA ]]  )
            #cRandomizer[[ nISA ]]$dfSubGroupEnrollmentStatus <- lISARes$cRandomizer$dfSubGroupEnrollmentStatus

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
                lISARes[[paste("lAnalysis", iAna, sep="")]] <- list( nGo = 0, nNoGo = 0, nPause = 1, cRandomizer =  cRandomizer[[ nISA ]])

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
        lResISA[[paste("lResISA", nISA, sep="")]] <- lISARes #[ names( lISARes ) != "cRandomizer" ]



    }
    lRet <- list( lResISA = lResISA, vISAAnalysisIndx = vISAAnalysisIndx ) #, cRandomizer = cRandomizer )
    return( lRet )

}




