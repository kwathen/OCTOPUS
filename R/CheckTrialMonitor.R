##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @title Check Trial Monitor
#' @name CheckTrialMonitor
#' @description  {CheckTrialMonitor An ISA monitoring plan consists of the specifying a minimum number of patients and follow-up (FU) time.
#' You must specify the minimum number of patients to perform an analysis and the final analysis FU time.
#' This is done by specifying the elements vMinQtyPats and  vMinFUTime  with at least 2 elements.
#' The first elements define when to
#' start monitoring based on a number of patients enrolled with additional FU (in months) and the last elements in the vectors
#' define when to do the final analysis.
#'
#' To increase flexibility there are two options for adding additional IAs.  Which option is used is based on dQtyMonthsBtwIA ==0  or > 0
#'       \enumerate{
#'       \item \strong{Option 1}:  vMinQtyPats and vMinFUTime are of equal length with length > 2 then the trial would consist of length( vMinQtyPats )-1 IAs and 1 FA.  See Example 1.
#'
#'       \item \strong{Option 2}: You MUST have the lengths of vMinQtyPats and vMinFUTime equal to 2.  See Example 3
#'         }
#' }
#'
#' @return {Return: list( vRunISAAnalysis, vPreviousIATime )
#'               vRunISAAnalysis = 1 if the ISA needs to have the IA run and 0 otherwise
#'               vPreviousIATime > 0 for the option when vQtyMonthsBtw[1] > 0 so we can track when the IA is done and know when the next one is to be done.
#'
#'       In both cases, vIsFinalISAAnalysis = TRUE if dCurrentTime >= time of the FA as defined above; otherwise it is set to FALSE
#'
#'       If vISAStatus != 1 then for the corresponding ISA elements in the return list this function returns
#'           vRunISAAnalysis = 0, vPreviousIATime = vPreviousIATime , vIsFinalISAAnalysis = FALSE, vCase = corresponding case )}
#'
#' @examples
#' \dontrun{
#' # Example 1 :   Perform  IA 1 at 3 months after the 30th patient enrolled, IA 2 at 3 months after the 90th patient enrolled, IA 3 when the 150th patient enrolled,
#' #               (not since the 3rd element of vMinFUTime = 0 IA 3 is performed when the 150th patient is enrolled) and the FA is performed 3 months after the 200th patient.
#'
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 30, 90, 150, 200 )
#'      vMinFUTime      <- c( 3,  3,  0,    3)
#'
#' #Example 2 - 1 Interim analysis (IA) at 30 patients with 3 months of FU
#' #            No additional IA
#' #            Final Analysis (FA) at 200 patients with 3 months FU
#'
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 30, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' #Example 3 - Start the IA 30 patients with 3 months of FU, continue
#' #            Perform additional analysis every 2 months after the first IA
#' #            Final Analysis (FA) at 200 patients with 3 months FU
#'
#'      dQtyMonthsBtwIA <- 2
#'      vMinQtyPats     <- c( 30, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' #Example 4 - No IA becasue the IA is done at the same time as the FA
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 200, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CheckTrialMonitor.R}{View Code on GitHub} }
#' @export
CheckTrialMonitor <- function(  cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
{
    UseMethod( "CheckTrialMonitor", cISADesigns )

}

#' @title Check Trial Monitor
#' @name CheckTrialMonitor.default
#' @description  {CheckTrialMonitor An ISA monitoring plan consists of the specifying a minimum number of patients and follow-up (FU) time.
#' You must specify the minimum number of patients to perform an analysis and the final analysis FU time.
#' This is done by specifying the elements vMinQtyPats and  vMinFUTime  with at least 2 elements.
#' The first elements define when to
#' start monitoring based on a number of patients enrolled with additional FU (in months) and the last elements in the vectors
#' define when to do the final analysis.
#'
#' To increase flexibility there are two options for adding additional IAs.  Which option is used is based on dQtyMonthsBtwIA ==0  or > 0
#'       \enumerate{
#'       \item \strong{Option 1}:  vMinQtyPats and vMinFUTime are of equal length with length > 2 then the trial would consist of length( vMinQtyPats )-1 IAs and 1 FA.  See Example 1.
#'
#'       \item \strong{Option 2}: You MUST have the lengths of vMinQtyPats and vMinFUTime equal to 2.  See Example 3
#'         }
#' }
#'
#' @return {Return: list( vRunISAAnalysis, vPreviousIATime )
#'               vRunISAAnalysis = 1 if the ISA needs to have the IA run and 0 otherwise
#'               vPreviousIATime > 0 for the option when vQtyMonthsBtw[1] > 0 so we can track when the IA is done and know when the next one is to be done.
#'
#'       In both cases, vIsFinalISAAnalysis = TRUE if dCurrentTime >= time of the FA as defined above; otherwise it is set to FALSE
#'
#'       If vISAStatus != 1 then for the corresponding ISA elements in the return list this function returns
#'           vRunISAAnalysis = 0, vPreviousIATime = vPreviousIATime , vIsFinalISAAnalysis = FALSE, vCase = corresponding case )}
#'
#' @examples
#' \dontrun{
#' # Example 1 :   Perform  IA 1 at 3 months after the 30th patient enrolled, IA 2 at 3 months after the 90th patient enrolled, IA 3 when the 150th patient enrolled,
#' #               (not since the 3rd element of vMinFUTime = 0 IA 3 is performed when the 150th patient is enrolled) and the FA is performed 3 months after the 200th patient.
#'
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 30, 90, 150, 200 )
#'      vMinFUTime      <- c( 3,  3,  0,    3)
#'
#' #Example 2 - 1 Interim analysis (IA) at 30 patients with 3 months of FU
#' #            No additional IA
#' #            Final Analysis (FA) at 200 patients with 3 months FU
#'
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 30, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' #Example 3 - Start the IA 30 patients with 3 months of FU, continue
#' #            Perform additional analysis every 2 months after the first IA
#' #            Final Analysis (FA) at 200 patients with 3 months FU
#'
#'      dQtyMonthsBtwIA <- 2
#'      vMinQtyPats     <- c( 30, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' #Example 4 - No IA becasue the IA is done at the same time as the FA
#'      dQtyMonthsBtwIA <- 0
#'      vMinQtyPats     <- c( 200, 200 )
#'      vMinFUTime      <- c( 3,   3)
#'
#' }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CheckTrialMonitor.R}{View Code on GitHub} }
#' @export
CheckTrialMonitor.default <- function(  cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
{
    #This function will Check the monitoring rule and set a variable if the ISAs should have the analysis run on them
    #eg is it time for an analysis on each ISA?

    nQtyISAs            <- length( cISADesigns )
    vRunISAAnalysis     <- rep( 0, nQtyISAs )
    vIsFinalISAAnalysis <- rep( FALSE, nQtyISAs )
    vCase               <- rep( 0, nQtyISAs)
    vPreviousIATimeOrig <- vPreviousIATime

    iISA <- 1
    for( iISA in 1:nQtyISAs )
    {

        nLen          <- length( cISADesigns[[ iISA ]]$vMinQtyPats )
        nQtyPatsInISA <- lEnrolledPats$vCurrentQtyPatsISA[ iISA ]

        CheckISAForErrors(cISADesigns, iISA, nQtyPatsInISA, vISAStatus )

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

                if(  dCurrentTime >=  dMinTimeForIA )
                {
                    vRunISAAnalysis[ iISA ] <- 1

                    vPreviousIATime[ iISA ] <- dCurrentTime

                    if( nAnalysisIndx == nLen )  #This is the final analysis
                    {
                        vIsFinalISAAnalysis[ iISA ] <- TRUE
                    }
                }

            }

        }
        else if( length( cISADesigns[[iISA]]$vMinQtyPats ) == 2 && dQtyMonthsBtw >= 0 )
        {
            #option 2 - monitor based on the first IA + dQtyMonthsBtw

            vCase[ iISA ]  <- 2
            if( nQtyPatsInISA >= nNextQtyPats ||  vPreviousIATime[ iISA ] > 0 )
            {

                #Check if this is the final analysi
                nQtyPatsFinal <- cISADesigns[[ iISA ]]$vMinQtyPats[ 2  ]

                dFinalIATime <- vISAStartTimes[ nQtyPatsFinal ] + cISADesigns[[ iISA ]]$vMinFUTime[ 2  ]
                if( nQtyPatsInISA >= nQtyPatsFinal )
                {
                    if( dCurrentTime >= dFinalIATime )
                    {
                        vIsFinalISAAnalysis[ iISA ] <- TRUE

                    }
                }

                #Now check if the analysis needs to be done
                if( vPreviousIATime[ iISA ] > 0 )
                {
                    #Have already done in IA for this ISA so need to see if the dQtyMonthsBtw has passed since previous IA
                    if( dCurrentTime >= min(dFinalIATime, ( vPreviousIATime[ iISA ] + dQtyMonthsBtw ), na.rm=TRUE ))
                    {
                        #Time to do the next IA
                        vRunISAAnalysis[ iISA ] <- 1
                        vPreviousIATime[ iISA ] <- dCurrentTime
                    }

                }
                else
                {   #There has not been an IA run yet so need to check if it is time to run one
                    dMinTimeForIA   <- vISAStartTimes[ nNextQtyPats ] + dAddFU
                    if(  dCurrentTime >=  dMinTimeForIA )
                    {
                        vRunISAAnalysis[ iISA ] <- 1
                        vPreviousIATime[ iISA ] <- dCurrentTime
                    }
                }

            }


        }
        else
        {
            #THERE IS AN ERROR
            stop( paste( "ERROR In the configuration for ISA ", iISA, ", please check vMinQtyPats, dMinFUTime and vQtyMonthsBtwIA for ISA ", iISA, sep =""))

        }



        #If the ISA is not open prior to the start of the ISA or it is closed for some reason (status =1 --> open status ==2 --> closed for max recruitment still could need analysis)
        if( vISAStatus[ iISA ] == 0 || vISAStatus[ iISA ] > 2  )
        {
            vRunISAAnalysis[ iISA ] <- 0
            vIsFinalISAAnalysis[ iISA ] <- FALSE
            vPreviousIATime[ iISA ] <- vPreviousIATimeOrig[ iISA]
        }





    }
    return( list( vRunISAAnalysis = vRunISAAnalysis, vPreviousIATime = vPreviousIATime, vIsFinalISAAnalysis = vIsFinalISAAnalysis, vCase = vCase ) )
}


CheckISAForErrors <- function(cISADesigns, iISA, nQtyPatsInISA, vISAStatus )
{
    nLen            <- length( cISADesigns[[ iISA ]]$vMinQtyPats )
    dQtyMonthsBtw   <- cISADesigns[[ iISA ]]$dQtyMonthsBtwIA
    if( length( cISADesigns[[ iISA ]]$vMinQtyPats) != length( cISADesigns[[ iISA ]]$vMinFUTime ) )
    {
        stop( paste( "In cISADesign[[ ", iISA, " ]], the length of vMinQtyPats and vMinFUTime are not the equal and they must be equal."))
    }

    if( length( cISADesigns[[ iISA ]]$vMinQtyPats) == length( cISADesigns[[ iISA ]]$vMinFUTime ) && nLen == 1 && dQtyMonthsBtw != 0)
    {

        stop( paste( "In cISADesign[[ ", iISA, " ]], the length( vMinQtyPats ) = length( vMinFUTime ) = 1 and dQtyMonthsBtw != 0. If length = 1 then dQtyMonthsBetween must equal 1."))

    }

    if( nQtyPatsInISA > 0 && vISAStatus[ iISA ] == 0 )
    {
        stop( paste( "In cISADesign[[ ", iISA, " ]], patients have been enrolled by the vISAStatus is set to open"))

    }

}


