##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @export
SimulateSingleTrial <- function( cScen, cTrialDesign )
{
    UseMethod( "SimulateSingleTrial", cScen )

}


#' @name SimulateSingleTrial
#' @title SimulateSingleTrial
#' @description {This function simulates 1 virtual trial based on cScen and cTrialDesign.  This is the main function for running
#' the simulation.}
#' @export
SimulateSingleTrial.default <- function( cScen, cTrialDesign  )
{
    #TODO: In validating the  cTrialDesign$cISADesigns need to make sure the ProcessDataXXX class type is the same in all ISAs
    if( cScen$nPrintDetail >= 100 )
        print( paste( "SimulateSingleTrial.default - Updated "))

    #Initialize Variables
    nQtyISA             <- cTrialDesign$nQtyISAs
    vStartISAAnalysis   <- rep( -1, nQtyISA )
    vISAPauseEnroll     <- rep( 0, nQtyISA)         # Did enrollment have to pause due to this ISA not being started
    vISAAnalysisIndx    <- rep( 1, nQtyISA )        # For each ISA we need to know which IA we are at, 0 --> have not done one
    vPreviousIATime     <- rep( 0, nQtyISA )        # This will store when the last IA was done for each ISA, used only when the ISA is monitored based on the number of months between
    nMaxQtyPats         <- cTrialDesign$nMaxQtyPats

    #Set the max number of patient in each ISA
    vMaxPatsInISA       <- cTrialDesign$vMaxQtyPatsInISA
    vCurrentQtyPatsISA  <- rep( 0, nQtyISA )
    vTimeFinalAnalysis  <- rep( 0, nQtyISA )
    vTimeFinalEnrollment<- rep( 0, nQtyISA )

    vTimeStartAnalysis  <- rep( 0, nQtyISA )

    vISAStatus          <- rep( 0, nQtyISA)  #All ISA are not open at the beginning, 0 is not open, 1 is open, 2 is close when it hit the max enrollment for a ISA

    # This function only sets the vISAStatus to 0,1,2 but when the MakeTrialDecision is called it could be changed as follows
    #   0 = ISA not added to trial;
    #   1 = ISA open,
    #   2 = met max enrollment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #TODO:  This should be somewhere else to be used by other functions as needed
    vstrStatus <- c("ISA not added to trial", "ISA Open", "Met Max Enrollment",
                    "Closed - Early Go", "Closed - Early No Go", "Closed - Go at FA", "Closed - No Go at FA", "Closed - Pause at FA")

    cEnrolledPats   <- InitializePatientList( cTrialDesign )                    #This will keep track of the current patients enrolled in the trial
    vISAStart       <- SimulateAllISAStartTimes( cScen$cISADesigns )            # Simulate the times the ISAs start

    vStartTimes     <- SimulateArrivalTimes( cScen$cAcc )
    lPatOut         <- SimulateAllPatientOutcomes( cScen,  cTrialDesign)
    cRandomizer     <- InitializeTrialRandomizer( cTrialDesign, vISAStartTime )

    iPat <- 1
    lResAnaFinal <- list()

    for( iPat in 1:(nMaxQtyPats) )
    {

        dCurrentTime <- vStartTimes[ iPat ]
        vISAStatus   <- ifelse( dCurrentTime > vISAStart & vISAStatus < 2, 1, vISAStatus   )

        cRandUpdate  <- Randomize( cRandomizer, vISAStatus, dCurrentTime  )
        cRandomizer  <- cRandUpdate$cRandomizer

        lRet         <- AddPatient( lPatOut, dCurrentTime,   cRandUpdate$nISA, cRandUpdate$nTrt, cEnrolledPats, nPrintDetail = cScen$nPrintDetail )

        cEnrolledPats<- lRet$cEnrolledPats
        lPatOut      <- lRet$lPatOut

        vTimeFinalEnrollment <- ifelse( cEnrolledPats$vCurrentQtyPatsISA == vMaxPatsInISA & vISAStatus < 2, dCurrentTime, vTimeFinalEnrollment )
        vISAStatus   <- ifelse( cEnrolledPats$vCurrentQtyPatsISA == vMaxPatsInISA & vISAStatus < 2, 2, vISAStatus )

        lMonitor            <- CheckTrialMonitor(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
        vPreviousIATime     <- lMonitor$vPreviousIATime
        vRunISAAnalysis     <- lMonitor$vRunISAAnalysis
        vIsFinalISAAnalysis <- lMonitor$vIsFinalISAAnalysis
        if( cScen$nPrintDetail >= 15 )
        {
            strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
            print( paste( "Patient ", iPat, " Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
        }

        if( any( vRunISAAnalysis == 1))
        {

            if( cScen$nPrintDetail >= 15 )
                print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))
            lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis  )

            lResAna             <- lRet$lResISA

            lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis)

            vTimeStartAnalysis  <- ifelse( vRunISAAnalysis == 1 & vISAAnalysisIndx == 1, dCurrentTime, vTimeStartAnalysis )
            #If the ISA was open and this analysis closed it then we need to keep the result of this ISA analysis
            vFinalAnalysis      <- ( vRunISAAnalysis == 1 & lDecision$vISAStatus > 2 )
            #print( paste( "vFinalAnalysis ", paste( vFinalAnalysis, collapse=", ")))

            vTimeFinalAnalysis[ vFinalAnalysis ]  <- dCurrentTime
            lResAnaFinal[ vFinalAnalysis ]        <- lResAna[ vFinalAnalysis ]

            vISAAnalysisIndx    <- lRet$vISAAnalysisIndx
            vISAStatus          <- lDecision$vISAStatus
            lDecision           <- lDecision$lDecRet
        }


        if( any( vISAStatus == 0 ) && all(vISAStatus >= 2 | vISAStatus == 0))
        {
            if( cScen$nPrintDetail >= 15 )
            {
                print( paste( "At time = ", dCurrentTime , " there are no ISAs open for enrollment and at least one ISA remains to be added to the platform."))
                print( paste( "...Checking if additional analysis need to run prior to the start of the next ISA starting."))
            }
            #This is the case where an ISA has not opened but all other ISAs have opened and closed, eg nothing currently open

            #However, for multiple ISAs there could be analysis run before the next ISA opens so we need to run those
            dNextISAStart           <- min( vISAStart[ vISAStatus == 0 ] )
            dNextISAAnalysis        <- CheckNextTrialAnalysisTime(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

            vIndex                  <- 1:nQtyISA
            nIndex                  <- vIndex[ vISAStart == dNextISAStart ]

            dTimeDelayForISA        <- dNextISAStart - dCurrentTime
            vISAPauseEnroll[ nIndex]<- dTimeDelayForISA

            vIndex                  <- (iPat+1):(length( vStartTimes ))
            vStartTimes[ vIndex ]   <- vStartTimes[ vIndex ] + dTimeDelayForISA

            #Need to loop to go over the the IA times that occur between the current time and next ISA opening
            #if dNextISAAnalysis == -1 then no analysis occur between current time and next ISA start time
            while( dNextISAAnalysis <= dNextISAStart & dNextISAAnalysis > 0 )
            {
                #Need to run the IAs that occur before the next ISA starts
                dCurrentTime        <- dNextISAAnalysis + 0.00001  #Add just to make sure it is past the time for the IA
                lMonitor            <- CheckTrialMonitor(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
                vPreviousIATime     <- lMonitor$vPreviousIATime
                vRunISAAnalysis     <- lMonitor$vRunISAAnalysis
                vIsFinalISAAnalysis <- lMonitor$vIsFinalISAAnalysis

                if( cScen$nPrintDetail >= 15 )
                {
                    strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
                    print( paste( "Patient ", iPat, ", No ISAs open Current Time ", dCurrentTime, ". Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
                }

                if( any( vRunISAAnalysis == 1))
                {
                    vTimeStartAnalysis  <- ifelse( vRunISAAnalysis == 1 & vISAAnalysisIndx == 1, dCurrentTime, vTimeStartAnalysis )

                    if( cScen$nPrintDetail >= 15 )
                        print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))
                    lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis  )

                    lResAna             <- lRet$lResISA

                    lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis)

                    #If the ISA was open and this analysis closed it then we need to keep the result of this ISA analysis
                    vFinalAnalysis      <- ( vRunISAAnalysis == 1 & lDecision$vISAStatus > 2 )
                    #print( paste( "vFinalAnalysis ", paste( vFinalAnalysis, collapse=", ")))

                    vTimeFinalAnalysis[ vFinalAnalysis ]  <- dCurrentTime
                    lResAnaFinal[ vFinalAnalysis ]        <- lResAna[ vFinalAnalysis ]



                    vISAAnalysisIndx    <- lRet$vISAAnalysisIndx
                    vISAStatus          <- lDecision$vISAStatus
                    lDecision           <- lDecision$lDecRet
                }

                dNextISAAnalysis        <- CheckNextTrialAnalysisTime(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )


            }


        }
        if( all( vISAStatus >= 2)) # All ISAs have opened and closed, trial is done
        {
            if( cScen$nPrintDetail >= 1 )
                print( "All ISA Status are closed, ending main loop")
            break
        }

    }



    if( cScen$nPrintDetail >= 15 )
        print( paste( "********************** FINISHED THE MAIN LOOP***********************"))

    #At this point, if you have multiple ISAs the vISAStatus should be >=2 for all of them otherwise we have a problem
    if( !all( vISAStatus >= 2 ))
    {
        print(paste( "After the main trial loop finished there was an ISA was status of 0 or 1. The vISAStatus was as follows:"))
        print(paste( vISAStatus, collapse=", "))
        stop( "Critical Error: SimulateSingleTrial")
    }

    ### Working HERE... I believe that if we reach this point we need to know when the last FA is so we can run the last analysis
    # This is not implemented yet

     # May need vIsFinalISAAnalysis = TRUE for all ISA
    if( any(vISAStatus == 2 ))
    {

        #Need to loop to go over the the IA times that occur between the current time and next ISA opening
        while( any(vISAStatus == 2 ) )
        {
            dNextISAAnalysis        <- CheckNextTrialAnalysisTime(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )


            #Need to run the IAs that occur before the next ISA starts
            dCurrentTime        <- dNextISAAnalysis + 0.00001  #Add just to make sure it is past the time for the IA
            lMonitor            <- CheckTrialMonitor(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
            vPreviousIATime     <- lMonitor$vPreviousIATime
            vRunISAAnalysis     <- lMonitor$vRunISAAnalysis
            vIsFinalISAAnalysis <- lMonitor$vIsFinalISAAnalysis

            if( cScen$nPrintDetail >= 15 )
            {
                strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
                print( paste( "All Patients Enrolled - Patient ", iPat, " Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
            }

            #In the while loop this next if should always be true
            if( any( vRunISAAnalysis == 1))
            {

                vTimeStartAnalysis  <- ifelse( vRunISAAnalysis == 1 & vISAAnalysisIndx == 1, dCurrentTime, vTimeStartAnalysis )

                if( cScen$nPrintDetail >= 15 )
                    print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))
                lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis  )

                lResAna             <- lRet$lResISA

                lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis)

                #If the ISA was open and this analysis closed it then we need to keep the result of this ISA analysis
                vFinalAnalysis      <- ( vRunISAAnalysis == 1 & lDecision$vISAStatus > 2 )
                #print( paste( "vFinalAnalysis ", paste( vFinalAnalysis, collapse=", ")))

                vTimeFinalAnalysis[ vFinalAnalysis ]  <- dCurrentTime
                lResAnaFinal[ vFinalAnalysis ]        <- lResAna[ vFinalAnalysis ]



                vISAAnalysisIndx    <- lRet$vISAAnalysisIndx
                vISAStatus          <- lDecision$vISAStatus
                lDecision           <- lDecision$lDecRet
            }


        }

        # In the block below once all patients were enrolled in the trial the final analysis would occur for each ISA
        # However, there could be IAs planned after enrollment has completed.  Keeping the code here as an example of how to skip to the
        #FA for each ISA in case it is needed.
        #At this point if any of the vISAStatus == 2 then they were closed for max # pats but need the final analysis done
        #
        # lRet        <- RunFinalTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, vISAAnalysisIndx  )
        #
        # lResAna     <- lRet$lResISA
        # vIsFinalISAAnalysis <- rep( TRUE,  nQtyISA)
        #
        # vFinalAnalysis <- ( vISAStatus == 2 )
        # lResAnaFinal[ vFinalAnalysis ]        <- lResAna[ vFinalAnalysis ]
        #
        # lDecision   <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus, vIsFinalISAAnalysis )
        # vISAStatus  <- lDecision$vISAStatus
        # lDecision   <- lDecision$lDecRet
        #
        # vTimeFinalAnalysis <- ifelse( lRet$vTimeFinalAnalysis != -1 & vTimeFinalAnalysis ==0,  lRet$vTimeFinalAnalysis, vTimeFinalAnalysis )
        #
    }



    if( cScen$nPrintDetail >= 15 )
    {
        strStatus <- paste( "Final ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
        print( paste( strStatus) )
    }
    ######

    for( i in 1:nQtyISA )
    {

        vTimeFinalEnrollment[ i ] <-cEnrolledPats$lPatOut[[i]]$vStartTime[ cEnrolledPats$vCurrentQtyPatsISA[i] ]
    }
    names( vISAStatus  ) <- paste( "ISA",unique(cTrialDesign$vISALab), "Status", sep="" )
    vGrpNames <- paste( "ISA",cTrialDesign$vISALab,"Trt",cTrialDesign$vTrtLab, sep="" )
    vQtyPatPerISAPerArm <- cEnrolledPats$vQtyPatsArmISA
    names( vQtyPatPerISAPerArm ) <- vGrpNames
    names(vISAStart ) <- paste( "ISAStart",1:length(vISAStart), sep="")
    names( vISAPauseEnroll ) <-  paste( "ISAPause",1:length(vISAStart), sep="")

    nQtyAnaRes <- length( lResAnaFinal )
    for( nISA in 1:nQtyAnaRes )
    {
        strFileName <- paste( "ISAOut", nISA, "/isaout", cScen$nGridIndex, ".csv", sep="" )
        vISAResult  <- rbind( c(cScen$nGridIndex, cScen$nTrialID, nISA, unlist( lResAnaFinal[[nISA]] )))
        colnames( vISAResult )[1:3] <- c( "GridIndex", "TrialID", "ISA")

        if(  cScen$nGridIndex == 1 && cScen$nTrialID == 1 ){
            strFileName <- paste( "ISAOut", nISA, "/1isaout", cScen$nGridIndex, ".csv", sep="" )
            write.table( vISAResult, strFileName, sep=", ", append=FALSE, col.name=TRUE, row.names = FALSE, quote=FALSE )
        }
        else{
            write.table( vISAResult, strFileName, sep=", ", append=TRUE, col.name=FALSE, row.names=FALSE)
        }

    }
    names( vTimeFinalAnalysis ) <- paste( "FinalAnalysisTimeISA", 1:nQtyISA, sep="")
    names( vTimeFinalEnrollment ) <- paste( "FinalPatientEnrolledTimeISA", 1:nQtyISA, sep="")
    names( vTimeStartAnalysis ) <- paste( "StartIAISA", 1:nQtyISA, sep="")
    lRet <- c(  cScen$nGridIndex, cScen$nTrialID, vISAStatus, CurrentTime = dCurrentTime, vQtyPatPerISAPerArm, vISAStart, vISAPauseEnroll,
                vTimeFinalEnrollment, vTimeStartAnalysis, vTimeFinalAnalysis)
    names( lRet)[c(1,2)]<-c( "GridIndex", "TrialID")
    return( lRet )

}


