##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateSingleTrial.R}{View Code on GitHub} }
#' @export
SimulateSingleTrial <- function( cScen, cTrialDesign )
{
    UseMethod( "SimulateSingleTrial", cScen )

}


#' @name SimulateSingleTrial
#' @title SimulateSingleTrial
#' @description {This function simulates 1 virtual trial based on cScen and cTrialDesign.  This is the main function for running
#' the simulation.}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateSingleTrial.R}{View Code on GitHub} }
#' @export
SimulateSingleTrial.default <- function( cScen, cTrialDesign  )
{    #TODO: In validating the  cTrialDesign$cISADesigns need to make sure the ProcessDataXXX class type is the same in all ISAs
    if( cScen$nPrintDetail >= 100 | gnPrintDetail >= 100 )
        print( paste( "SimulateSingleTrial.default - Updated "))

    #Initialize Variables
    if( gDebug == TRUE   )
        browser()
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
    vISAStartTimes  <- SimulateAllISAStartTimes( cScen$cISADesigns )            # Simulate the times the ISAs start

    vStartTimes     <- SimulateArrivalTimes( cScen$cAcc )
    dfPatCov        <- SimulateAllPatientCovariates( cScen$cSimCovariates, cTrialDesign )  # This will need to go into the next line to simulate pateitn outcomes
    lPatOut         <- SimulateAllPatientOutcomes( cScen,  cTrialDesign, dfPatCov  )
    cRandomizer     <- InitializeTrialRandomizer( cTrialDesign, vISAStartTimes )

    iPat <- 1
    lResAnaFinal <- list()
    #TODO(Covs) When moving to the version with covaraite and allowing stopping of groups within an ISA, we may need to change the for loop
    # and possibly need to generate more patient outcomes.   See Functions->AppendPatientLists to allow more patients to be added to the
    # patient database. To be able to add more start times could simply call SimulateArrivalTimes and add the last start time to the retured
    #start times

    nTmp <- 0
    for( iPat in 1:(nMaxQtyPats) )
    {

        dCurrentTime <- vStartTimes[ iPat ]
        vISAStatus   <- ifelse( dCurrentTime > vISAStartTimes & vISAStatus < 2, 1, vISAStatus   )

        #TODO(Covs) - Check if ISAs/Arms are open based on covs
        #Thinking - Each ISA has a list of covarates with True/false to indicate if the ISA is enrolling for that type of patient
        #This would likely get added to the cRandomzier in the InitialzieTrialRandomizer above and could be modified due to analysis
        #Before a patient is randomized we have to know the covarites so we could use lPatOut to get the values of the covariates
        # remove the ones used, send the selected covs into Randomize and that way when we select the patient based on ISA/Treatment
        #we would also need to select based on covs

        if( is.null( dfPatCov ) )
            dfCov    <- NULL
        else
        {
            iIndx        <- sample( 1:nrow( dfPatCov ), size = 1 )
            dfCov        <- dfPatCov[  iIndx, ]
            dfPatCov     <- dfPatCov[ -iIndx, ]
        }

        #vEnrollmentStatus - Two cases
        # If No covaraite - dfCov = NULL then  vEnrollmentStatus = vISAStatus
        # If Covariates - dfCov != NULL then vEnrollmentStatus = vISAStatus but if an ISA does not enroll for dfCov the value will be
        vEnrollmentStatus  <- CheckISAEnrollmentStatus( cRandomizer, vISAStatus, dfCov )

        #In the event that no ISA is enrolling for the patient that enrolled due to dfCov, then this while loop
        #will keep checking the next patients until one can be enrolled.
        #TODO(Covs) - THis while loop could be refactored, currently leaving it here to complete development on Covs
        while( all( vEnrollmentStatus != 1 ) )
        {
            #print( paste( "Trying next..."))
            # if no element in vISAStatus == 1 --> No ISA is open for the patient with dfCov covariates, remove this patient and go to the next
            # until an ISA is open for the patient
            vStartTimes <- vStartTimes[ -iPat ]
            if( length( vStartTimes ) < nMaxQtyPats ) #Simulate more times
            {
                vStartTimes <- SimulateAdditionalArrivalTimes( cScen$cAcc, nMaxQtyPats, vStartTimes )
            }

            #Restart the loop above
            dCurrentTime <- vStartTimes[ iPat ]
            vISAStatus   <- ifelse( dCurrentTime > vISAStartTimes & vISAStatus < 2, 1, vISAStatus   )

            if( is.null( dfPatCov ) )
                dfCov    <- NULL
            else
            {
                if( nrow( dfPatCov ) == 0 )   #Out of patients need to add more
                {

                    #lPatOut does not contain any simulated pateints in the correct ISA,TRT with dfCov; therefore simualte more patients
                    dfPatCov2   <- SimulateAllPatientCovariates( cScen$cSimCovariates, cTrialDesign )  # This will need to go into the next line to simulate pateitn outcomes
                    lPatOut2    <- SimulateAllPatientOutcomes( cScen,  cTrialDesign, dfPatCov2  )
                    dfPatCov    <- rbind( dfPatCov, dfPatCov2 )
                    lPatOut     <- AppendPatientLists( lPatOut, lPatOut2 )

                }
                iIndx        <- sample( 1:nrow( dfPatCov ), size = 1 )
                #print( paste( "Index ", iIndx, " nrow(dfPatCov) ", nrow( dfPatCov ) ))
                dfCov        <- dfPatCov[  iIndx, ]
                dfPatCov     <- dfPatCov[ -iIndx, ]
            }
            vEnrollmentStatus  <- CheckISAEnrollmentStatus( cRandomizer, vISAStatus, dfCov )

        }
        #print( paste( "Randomzier call ", class( cRandomizer) ) )

        cRandUpdate  <- Randomize( cRandomizer, vEnrollmentStatus, dCurrentTime, dfCov )

        cRandomizer  <- cRandUpdate$cRandomizer


        lRet         <- AddPatient( lPatOut, dCurrentTime,   cRandUpdate$nISA, cRandUpdate$nTrt, dfCov, cEnrolledPats, nPrintDetail = cScen$nPrintDetail )
       # bAddBreak <- FALSE
        while( !is.list( lRet ) && lRet == -1 )
        {
            # Handle the situation when there are no more patients in lPatOut for the given ISA/nTrt/Cov group by simulating more patients
            #TODO(COvs) - This code block could be refactored.

            #lPatOut does not contain any simulated pateints in the correct ISA,TRT with dfCov; therefore simualte more patients
            dfPatCov2   <- SimulateAllPatientCovariates( cScen$cSimCovariates, cTrialDesign )  # This will need to go into the next line to simulate pateitn outcomes
            lPatOut2    <- SimulateAllPatientOutcomes( cScen,  cTrialDesign, dfPatCov2  )
            dfPatCov    <- rbind( dfPatCov, dfPatCov2 )
            lPatOut     <- AppendPatientLists( lPatOut, lPatOut2 )
            lRet        <- AddPatient( lPatOut, dCurrentTime,   cRandUpdate$nISA, cRandUpdate$nTrt, dfCov, cEnrolledPats, nPrintDetail = cScen$nPrintDetail )

        }

        cEnrolledPats<- lRet$cEnrolledPats
        lPatOut      <- lRet$lPatOut

        vTimeFinalEnrollment <- ifelse( cEnrolledPats$vCurrentQtyPatsISA == vMaxPatsInISA & vISAStatus < 2, dCurrentTime, vTimeFinalEnrollment )
        vISAStatus           <- ifelse( cEnrolledPats$vCurrentQtyPatsISA == vMaxPatsInISA & vISAStatus < 2, 2, vISAStatus )

        lMonitor             <- CheckTrialMonitor(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
        vPreviousIATime      <- lMonitor$vPreviousIATime
        vRunISAAnalysis      <- lMonitor$vRunISAAnalysis
        vIsFinalISAAnalysis  <- lMonitor$vIsFinalISAAnalysis
        if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15 )
        {
            strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
            #print( paste( "Patient ", iPat, " Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
        }

        if( any( vRunISAAnalysis == 1))
        {


            if( gDebug == TRUE   )
                browser()

            if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15)
                print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))

            #TODO(Covs) -The RunTrialAnalysis needs cRandomizer info because it will return the nGo, nNoGo, nPause status, possibly for multiple doses, or in this case covariates

            lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
            lResAna             <- lRet$lResISA

            #TODO(Covs) - For covs if lRet object has info for a cov group then make MakeTrialDecision may need to update cRandomizer[[ iISA ]]$dfSubGroupEnrollmentStatus
            #             There is another call to MakeTrialDecision below, make sure to implement changes there as well
            lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis, cRandomizer )

            #if( !is.null( dfPatCov ) )
                cRandomizer         <- lDecision$cRandomizer

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
            #This is the case where an ISA has not opened but all other ISAs have opened and closed, eg nothing currently open

            #However, for multiple ISAs there could be analysis run before the next ISA opens so we need to run those
            if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15 )
            {
                print( paste( "At time = ", dCurrentTime , " there are no ISAs open for enrollment and at least one ISA remains to be added to the platform."))
                print( paste( "...Checking if additional analysis need to run prior to the start of the next ISA starting."))
            }

            dNextISAStart           <- min( vISAStartTimes[ vISAStatus == 0 ] )
            dNextISAAnalysis        <- CheckNextTrialAnalysisTime(  cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

            vIndex                  <- 1:nQtyISA
            nIndex                  <- vIndex[ vISAStartTimes == dNextISAStart ]

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

                if( cScen$nPrintDetail >= 15 | gnPrintDetail >=15 )
                {
                    strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
                    print( paste( "Patient ", iPat, ", No ISAs open Current Time ", dCurrentTime, ". Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
                }

                if( any( vRunISAAnalysis == 1))
                {
                    vTimeStartAnalysis  <- ifelse( vRunISAAnalysis == 1 & vISAAnalysisIndx == 1, dCurrentTime, vTimeStartAnalysis )

                    if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15 )
                        print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))
                    lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
                    #cRandomizer         <- lRet$cRandomizer
                    lResAna             <- lRet$lResISA

                    #TODO(Covs) - For covs if lRet object has info for a cov group then make MakeTrialDecision may need to update cRandomizer[[ iISA ]]$dfSubGroupEnrollmentStatus

                    lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis, cRandomizer)

                    #if( !is.null( dfPatCov ) )
                        cRandomizer         <- lDecision$cRandomizer

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
            if( cScen$nPrintDetail >= 2 | gnPrintDetail >= 2 )
                print( "All ISA Status are closed, ending main loop")
            break
        }

    } #End of main loop



    if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15)
        print( paste( "********************** FINISHED THE MAIN LOOP***********************"))

    #At this point, if you have multiple ISAs the vISAStatus should be >=2 for all of them otherwise we have a problem
    if( !all( vISAStatus >= 2 ))
    {
        print(paste( "After the main trial loop finished there was an ISA was status of 0 or 1. The vISAStatus was as follows:"))
        print(paste( vISAStatus, collapse=", "))
        stop( "Critical Error: SimulateSingleTrial")
    }



    if( gDebug == TRUE   )
        browser()

    # Create the enrollment time, isa ad trt vectors

    vAllPatientStartTimes <- c()
    vAllPatientTreatments <- c()
    vAllPatientISAs       <- c()


    nQtyCovs <- length( cScen$cSimCovariates )
    if( nQtyCovs > 0)
    {
        mAllPatientCovs <- matrix( ncol = nQtyCovs, nrow=0)
        vCovName        <-  paste( "Cov", 1:nQtyCovs, sep="")
    }
    else
    {
        vCovName <- NULL
        mAllPatientCovs <- NULL
    }

    for( i in 1:nQtyISA )
    {
        vPatientStartTimes <- cEnrolledPats$lPatOut[[ i ]]$vStartTimes
        vPatientTreatments <- cEnrolledPats$lPatOut[[ i ]]$vTrt
        mPatientCovs       <- data.frame( cEnrolledPats$lPatOut[[ i ]][ c( paste( "vCov", 1:nQtyCovs, sep="" ) ) ] )
        vAllPatientStartTimes <- c( vAllPatientStartTimes, vPatientStartTimes)
        vAllPatientTreatments <- c( vAllPatientTreatments, vPatientTreatments)
        vAllPatientISAs       <- c( vAllPatientISAs, rep( i, length(  vPatientStartTimes ) ))
        mAllPatientCovs       <- rbind( mAllPatientCovs, mPatientCovs)
    }
    vOrderStart <- order( vAllPatientStartTimes )
    vAllPatientStartTimes <- vAllPatientStartTimes[ vOrderStart ]
    vAllPatientTreatments <- vAllPatientTreatments[ vOrderStart ]
    vAllPatientISAs       <- vAllPatientISAs[ vOrderStart ]
    if( !is.null( mAllPatientCovs ) )
        mAllPatientCovs       <- mAllPatientCovs[ vOrderStart, ]

    mEnrollment <- cbind( cScen$nDesign, cScen$Scen, cScen$nGridIndex, cScen$nTrialID, round(vAllPatientStartTimes,2) , vAllPatientTreatments, vAllPatientISAs, 1:length( vAllPatientISAs), mAllPatientCovs )

    colnames( mEnrollment ) <- c("Design", "Scenario", "GridIndex", "TrialID", "StartTime","Treatment", "ISA", "PatientNumber",vCovName )

    strFileName <- paste( "enrollment/enroll", cScen$nGridIndex, ".csv", sep="" )


    if(  cScen$nGridIndex == 1 && cScen$nTrialID == 1 ){
        strFileName <- paste( "enrollment/1enroll", cScen$nGridIndex, ".csv", sep="" )
        write.table( mEnrollment, strFileName, sep=", ", append=FALSE, col.name=TRUE, row.names = FALSE, quote=FALSE )
    }
    else{
        write.table( mEnrollment, strFileName, sep=", ", append=TRUE, col.name=FALSE, row.names=FALSE)
    }

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

            if( cScen$nPrintDetail >= 15 | gnPrintDetail >= 15)
            {
                strStatus <- paste( "ISA Status ", paste( vstrStatus[ vISAStatus+1 ], collapse=", "), collapse = " " )
                print( paste( "All Patients Enrolled - Patient ", iPat, " Analysis Index ", paste( vISAAnalysisIndx, collapse=", "), strStatus ))
            }

            #In the while loop this next if should always be true
            if( any( vRunISAAnalysis == 1))
            {

                vTimeStartAnalysis  <- ifelse( vRunISAAnalysis == 1 & vISAAnalysisIndx == 1, dCurrentTime, vTimeStartAnalysis )

                if( cScen$nPrintDetail >= 15  | gnPrintDetail >= 15 )
                    print( paste( "Running trial analysis vRunISAAnalysis =", paste( vRunISAAnalysis, collapse  =", ")))
                lRet                <- RunTrialAnalysis( cTrialDesign$cISADesigns, cEnrolledPats,  vISAStatus, dCurrentTime, vRunISAAnalysis, vISAAnalysisIndx, vIsFinalISAAnalysis, cRandomizer  )
                #cRandomizer         <- lRet$cRandomizer
                lResAna             <- lRet$lResISA

                lDecision           <- MakeTrialDecision( cTrialDesign$cISADesigns, lResAna,  vISAStatus,  vIsFinalISAAnalysis, cRandomizer)

                #if( !is.null( dfPatCov ) )
                    cRandomizer         <- lDecision$cRandomizer

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



    if( cScen$nPrintDetail >= 15  | gnPrintDetail >= 15 )
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
    names(vISAStartTimes )       <- paste( "ISAStart",1:length(vISAStartTimes), sep="")
    names( vISAPauseEnroll )     <-  paste( "ISAPause",1:length(vISAStartTimes), sep="")

    nQtyAnaRes <- length( lResAnaFinal )
    for( nISA in 1:nQtyAnaRes )
    {
        strFileName                 <- paste( "ISAOut", nISA, "/isaout", cScen$nGridIndex, ".csv", sep="" )
        vAnaResults                 <- unlist( lResAnaFinal[[nISA]] )
        vNonRandomizerElements      <- grep( "cRandomizer",names( vAnaResults ), invert=TRUE)

        vISAResult                  <- rbind( c(cScen$nGridIndex, cScen$nTrialID, nISA, vAnaResults[ vNonRandomizerElements ] ))
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
    lRet <- c(  cScen$nGridIndex, cScen$nTrialID, vISAStatus, CurrentTime = dCurrentTime, vQtyPatPerISAPerArm, vISAStartTimes, vISAPauseEnroll,
                vTimeFinalEnrollment, vTimeStartAnalysis, vTimeFinalAnalysis)
    names( lRet)[c(1,2)]<-c( "GridIndex", "TrialID")
    return( lRet )

}


