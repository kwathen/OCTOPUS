##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name CheckTrialMonitor
#' @title Check Trial Monitor
#' @description {An ISA monitoring plan consists of the specifying a minimum number of patients and follow-up (FU) time
#' You must specify the minimum and the final analysis FU time.  This is done by specifying the
#' elements vMinQtyPats and  vMinFUTime  with at least 2 elements.   The first elements define when to
#' start monitoring based on a number of patients enrolled with additional FU and the last elements in the vectors
#' define when to do the final analysis .  For example, if an ISA will enroll up to 200 patients and  we define the elements in the following way
#' vMinQtyPats <- c( 30, 200 )
#' vMinFUTime  <- c( 3,  3)
#' would start monitoring patients 3 months after the 30th patient enrolled and the final analysis (FA) would be done 3 months after the 200th (last patient) enrolled
#'
#'   To increase flexibility there are two options for adding additional IAs.  Which option is used is based on dQtyMonthsBtwIA ==0  or > 0
#'       Option 1:  the vMinQtyPats and vMinFUTime are of equal length with length > 2 then the trial would consist of length( vMinQtyPats )-1 IAs and 1 FA
#'           Example:    dQtyMonthsBtwIA  <-  0
#'                       vMinQtyPats     <- c( 30, 90, 150, 200 )
#'                       vMinFUTime      <- c( 3,  3,  0,    3)
#'               Would perform  IA 1 at 3 months after the 30th patient enrolled, IA 2 at 3 months after the 90th patient enrolled, IA 3 when the 150th patient enrolled,
#'               (not since the 3rd element of vMinFUTime = 0 IA 3 is performed when the 150th patient is enrolled) and the FA is performed 3 months after the 200th patient
#'               NOTE: With dQtyMonthsBtwIA > 0 and length( vMinQtyPats ) > 2 --> ERROR and stops running
#'
#'
#'       Option 2: You MUST have the lengths of vMinQtyPats and vMinFUTime equal to 2.
#'           Example:    dQtyMonthsBtwIA <-  2
#'                       vMinQtyPats     <- c( 30, 200 )
#'                       vMinFUTime      <- c( 3,   3)
#'                       This option will run the first IA 3 months after the 30th patient.  After that, the ISA is monitored every 2 months (vQtyMonthsBtwIA[1])
#'                       and the FA is 3 months after the 200th patient
#'
#'
#'
#'        Return: list( vRunISAAnalysis, vPreviousIATime )
#'               vRunISAAnalysis = 1 if the ISA needs to have the IA run and 0 otherwise
#'               vPreviousIATime > 0 for the option when vQtyMonthsBtw[1] > 0 so we can track when the IA is done and know when the next one is to be done.
#'
#'       In both cases, vIsFinalISAAnalysis = TRUE if dCurrentTime >= time of the FA as defined above; otherwise it is set to FALSE
#'
#'       If vISAStatus != 1 then for the corresponding ISA elements in the return list this function returns
#'           vRunISAAnalysis = 0, vPreviousIATime = vPreviousIATime , vIsFinalISAAnalysis = FALSE, vCase = corresponding case )}
#' @export
CheckTrialMonitor <- function(  cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
{
    UseMethod( "CheckTrialMonitor", cISADesigns )

}
#' @name CheckTrialMonitor.default
#' @title Check Trial Monitor
#' @description {An ISA monitoring plan consists of the specifying a minimum number of patients and follow-up (FU) time
#' You must specify the minimum and the final analysis FU time.  This is done by specifying the
#' elements vMinQtyPats and  vMinFUTime  with at least 2 elements.   The first elements define when to
#' start monitoring based on a number of patients enrolled with additional FU and the last elements in the vectors
#' define when to do the final analysis .  For example, if an ISA will enroll up to 200 patients and  we define the elements in the following way
#' vMinQtyPats <- c( 30, 200 )
#' vMinFUTime  <- c( 3,  3)
#' would start monitoring patients 3 months after the 30th patient enrolled and the final analysis (FA) would be done 3 months after the 200th (last patient) enrolled
#'
#'   To increase flexibility there are two options for adding additional IAs.  Which option is used is based on dQtyMonthsBtwIA ==0  or > 0
#'       Option 1:  the vMinQtyPats and vMinFUTime are of equal length with length > 2 then the trial would consist of length( vMinQtyPats )-1 IAs and 1 FA
#'           Example:    dQtyMonthsBtwIA  <-  0
#'                       vMinQtyPats     <- c( 30, 90, 150, 200 )
#'                       vMinFUTime      <- c( 3,  3,  0,    3)
#'               Would perform  IA 1 at 3 months after the 30th patient enrolled, IA 2 at 3 months after the 90th patient enrolled, IA 3 when the 150th patient enrolled,
#'               (not since the 3rd element of vMinFUTime = 0 IA 3 is performed when the 150th patient is enrolled) and the FA is performed 3 months after the 200th patient
#'               NOTE: With dQtyMonthsBtwIA > 0 and length( vMinQtyPats ) > 2 --> ERROR and stops running
#'
#'
#'       Option 2: You MUST have the lengths of vMinQtyPats and vMinFUTime equal to 2.
#'           Example:    dQtyMonthsBtwIA <-  2
#'                       vMinQtyPats     <- c( 30, 200 )
#'                       vMinFUTime      <- c( 3,   3)
#'                       This option will run the first IA 3 months after the 30th patient.  After that, the ISA is monitored every 2 months (vQtyMonthsBtwIA[1])
#'                       and the FA is 3 months after the 200th patient
#'
#'
#'
#'        Return: list( vRunISAAnalysis, vPreviousIATime )
#'               vRunISAAnalysis = 1 if the ISA needs to have the IA run and 0 otherwise
#'               vPreviousIATime > 0 for the option when vQtyMonthsBtw[1] > 0 so we can track when the IA is done and know when the next one is to be done.
#'
#'       In both cases, vIsFinalISAAnalysis = TRUE if dCurrentTime >= time of the FA as defined above; otherwise it is set to FALSE
#'
#'       If vISAStatus != 1 then for the corresponding ISA elements in the return list this function returns
#'           vRunISAAnalysis = 0, vPreviousIATime = vPreviousIATime , vIsFinalISAAnalysis = FALSE, vCase = corresponding case )
#'
#'       vCase is included for testing purposes and not intended for use outside of testing. }
#'       cISADesigns <- cTrialDesign$cISADesigns, lEnrolledPats <-cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
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
    nLen          <- length( cISADesigns[[ iISA ]]$vMinQtyPats )
    if( length( cISADesigns[[ iISA ]]$vMinQtyPats) != length( cISADesigns[[ iISA ]]$vMinFUTime ) ||
        nLen < 2 )
    {
        stop( paste( "In cISADesign[[ ", iISA, " ]], the length of vMinQtyPats and vMinFUTime are not the same and at least 2 elements"))
    }

    if( nQtyPatsInISA > 0 && vISAStatus[ iISA ] == 0 )
    {
        stop( paste( "In cISADesign[[ ", iISA, " ]], patients have been enrolled by the vISAStatus is set to open"))

    }

}


