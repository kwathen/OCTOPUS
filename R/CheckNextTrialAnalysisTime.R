##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name CheckNextTrialAnalysisTime
#' @title CheckNextTrialAnalysisTime
#' @description {This function will Check the monitoring rule and return the time of next Analysis for the trial}
#' @export
CheckNextTrialAnalysisTime<- function(  cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
{
    UseMethod( "CheckNextTrialAnalysisTime", cISADesigns )

}


#' @name CheckNextTrialAnalysisTime.default
#' @title CheckNextTrialAnalysisTime.default
#' @description {This function will Check the monitoring rule and return the time of next Analysis for the trial}
#' @export
CheckNextTrialAnalysisTime.default <- function( cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
{
    #This function will Check the monitoring rule and return the time of next Analysis for the trial
    ##### TODO(Bug) If all ISAs are closed and other ISAs have yet to open then #####
    nQtyISAs            <- length( cISADesigns )
    vRunISAAnalysis     <- rep( 0, nQtyISAs )
    vIsFinalISAAnalysis <- rep( FALSE, nQtyISAs )
    vCase               <- rep( 0, nQtyISAs)
    vPreviousIATimeOrig <- vPreviousIATime
    vTimeOfNextIA       <- rep( -1, nQtyISAs)

    dNextISATime        <- -1  # By default there is not another analysis for any of the Max Enrolled ISAs
    iISA <- 1
    for( iISA in 1:nQtyISAs )
    {
        if( vISAStatus[ iISA ] == 2 )  #Met the max enrollment and need to calculate the time of the next IA for this ISA.
        {

            nLen          <- length( cISADesigns[[ iISA ]]$vMinQtyPats )
            nQtyPatsInISA <- lEnrolledPats$vCurrentQtyPatsISA[ iISA ]


            nAnalysisIndx   <- min( vISAAnalysisIndx[ iISA ], nLen )
            nNextQtyPats    <- cISADesigns[[ iISA ]]$vMinQtyPats[ nAnalysisIndx  ]
            dAddFU          <- cISADesigns[[ iISA ]]$vMinFUTime[ nAnalysisIndx  ]
            dQtyMonthsBtw   <- cISADesigns[[ iISA ]]$dQtyMonthsBtwIA

            vISAStartTimes  <- lEnrolledPats$lPatOut[[iISA]]$vStartTimes




            if( dQtyMonthsBtw == 0 )
            {
                #Option 1 - monitor based on number of patients + FU
                vCase[ iISA ] <- 1
                if(  nQtyPatsInISA >= nNextQtyPats )
                {
                    dMinTimeForIA   <- vISAStartTimes[ nNextQtyPats ] + dAddFU
                    vTimeOfNextIA[ iISA ] <- dMinTimeForIA

                }

            }
            else if( length( cISADesigns[[iISA]]$vMinQtyPats ) == 2 && dQtyMonthsBtw >= 0 )
            {
                #option 2 - monitor based on the first IA + dQtyMonthsBtw

                vCase[ iISA ]  <- 2
                if( nQtyPatsInISA >= nNextQtyPats ||  vPreviousIATime[ iISA ] > 0 )
                {



                    #Check if this is the final analysis
                    nQtyPatsFinal <- cISADesigns[[ iISA ]]$vMinQtyPats[ 2  ]
                    dFinalIATime  <- vISAStartTimes[ nQtyPatsFinal ] + cISADesigns[[ iISA ]]$vMinFUTime[ 2  ]


                    #Now check if the analysis needs to be done
                    if( vPreviousIATime[ iISA ] > 0 )
                    {
                        #Have already done in IA for this ISA so need to see if the dQtyMonthsBtw has passed since previous IA

                        dNextIATime <- vPreviousIATime[ iISA ] + dQtyMonthsBtw


                    }
                    else
                    {   #There has not been an IA run yet so need to check if it is time to run one
                        dNextIATime <- vISAStartTimes[ nNextQtyPats ] + dAddFU

                    }

                    vTimeOfNextIA[ iISA ] <- min(dFinalIATime, dNextIATime, na.rm=TRUE )

                }
            }




            else
            {
                #THERE IS AN ERROR
                stop( paste( "ERROR In the configuration for ISA ", iISA, ", please check vMinQtyPats, dMinFUTime and vQtyMonthsBtwIA for ISA ", iISA, sep =""))

            }
        }

    }
    if( any( vTimeOfNextIA > 0 ) )
    {
        dNextISATime <- min( vTimeOfNextIA[ vTimeOfNextIA > 0 ])
    }
    return( dNextISATime )


}
