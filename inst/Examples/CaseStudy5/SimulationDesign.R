##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


SetupSimulations <- function( cTrialDesign, nQtyReps  )
{

    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

    nQtyCol <- length( vObsTime1 )


    vMeanR  <- c( 3.5, 3.4, 3.0, 2.5,  2.9, 1.4, 0.9 )
    vMeanNR <- c( 3.5, 3.5, 3.5, 3.5,  3.5, 3.5, 3.5 )

    mVarCovR   <- matrix( c( 0.5, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
                                0.4, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4,
                                0.4, 0.4, 0.5, 0.4, 0.4, 0.4, 0.4,
                                0.4, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4,
                                0.4, 0.4, 0.4, 0.4, 0.5, 0.4, 0.4,
                                0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.4,
                                0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5), ncol=nQtyCol )

    #Please note, you could specify different var-cov matrix for R and NR
    mVarCovNR <- mVarCovR


    vProbGroupCtrl  <- c( 0.02, 0.98 )       #The percentage of R and NR in the ctrl group


    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1

    #############################################.
    # Setup ISA1 cISADesign element          ####
    #############################################.

    cISAStart1   <- structure( list( dParam1=0), class="SetTime" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime


    ### Control Arm
    lSimArm1     <- list( vMean1     = vMeanR,
                          mVarCov1   = mVarCovR,
                          vMean2     = vMeanNR,
                          mVarCov2   = mVarCovNR,
                          vObsTime   = vObsTime1,
                          vProbGroup = vProbGroupCtrl,
                          dMinimum   = 0 )

    ### Treatment - Null case
    lSimArm2     <- list( vMean1     = vMeanR,
                          mVarCov1   = mVarCovR,
                          vMean2     = vMeanNR,
                          mVarCov2   = mVarCovNR,
                          vObsTime   = vObsTime1,
                          vProbGroup = vProbGroupCtrl,
                          dMinimum   = 0 )




    cSimOutcome1 <- structure(list( lSimArm1 = lSimArm1,
                                    lSimArm2 = lSimArm2), class=c("MVNWithCovariate"))


    cISA1Info <- structure( list(cSimOutcomes = cSimOutcome1,
                                 cSimISAStart = cISAStart1 ) )



    ##################################################################################.
    # Setup ISA2 cISADesign element                                               ####
    # Note: For the cISAStart times the class options include SetTime or Uniform
    # See ?SimulateISAStartTime.SetTime or ?SimulateISAStartTime.Uniform
    ##################################################################################.

    # ISA 2 enters the platform at a random time between month 3 and 6.
    cISAStart2   <- structure( list( dParam1=3, dParam2=6), class="Uniform" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA2$cISAAnalysis$vAnalysis[[1]]$vObsTime

    ### Control Arm
    lSimArm1     <- list( vMean1     = vMeanR,
                          mVarCov1   = mVarCovR,
                          vMean2     = vMeanNR,
                          mVarCov2   = mVarCovNR,
                          vObsTime   = vObsTime1,
                          vProbGroup = vProbGroupCtrl,
                          dMinimum   = 0 )

    ### Treatment - Null case
    lSimArm2     <- list( vMean1     = vMeanR,
                          mVarCov1   = mVarCovR,
                          vMean2     = vMeanNR,
                          mVarCov2   = mVarCovNR,
                          vObsTime   = vObsTime1,
                          vProbGroup = vProbGroupCtrl,
                          dMinimum   = 0 )

    ### Note: The addition of arm 3 (dose 2)
    ### Treatment - Null case
    lSimArm3     <- list( vMean1     = vMeanR,
                          mVarCov1   = mVarCovR,
                          vMean2     = vMeanNR,
                          mVarCov2   = mVarCovNR,
                          vObsTime   = vObsTime1,
                          vProbGroup = vProbGroupCtrl,
                          dMinimum   = 0 )




    cSimOutcome1 <- structure(list( lSimArm1 = lSimArm1,
                                    lSimArm2 = lSimArm2,
                                    lSimArm3 = lSimArm3), class=c("MVNWithCovariate"))


    cISA2Info <- structure( list(cSimOutcomes = cSimOutcome1,
                                 cSimISAStart = cISAStart2 ) )

    #############################################.
    # Setup the cISADesign element           ####
    #############################################.


    cISADesigns <- structure( list( cISA1 = cISA1Info,
                                    cISA2 = cISA2Info) )



    #####################################################.
    # Setup the non-ISA specific info and sceanrios ####
    #####################################################.


    nMaxQtyPats <- 0
    i<-1
    if( is.null( cTrialDesign$nMaxQtyPats) )
    {
        for( i in 1:length( cTrialDesign$cISADesigns))
        {
            nMaxQtyPats     <- nMaxQtyPats + sum(  cTrialDesign$cISADesigns[[i]]$vQtyPats)

        }
    }
    else
    {
        nMaxQtyPats <- cTrialDesign$nMaxQtyPats
    }


    ##########################################################################################.
    # Recuitment rates, how many patient are expected to enroll each month in the trial ####
    ##########################################################################################.
    # The infomraiton provided is in terms of the number of patients per month per site and
    #
    vPatsPerMonthPerSite1   <- c(0.1, 0.3, 0.45, 0.5, 0.5, 0.5 )
    vQtyOfSitesPlat         <- c(3,     8,  15,   35,  50, 70)
    vQtyOfPatsPerMonth1     <- vPatsPerMonthPerSite1 * vQtyOfSitesPlat

    ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth1, nMaxQtyPatients = nMaxQtyPats )
    #################################################################################.




    cScen1 <- structure(list(cAcc         = ap,
                             nDesign      = nDesign,
                             Scen         = 1,
                             nQtyReps     = nQtyReps,
                             nPrintDetail = 0,
                             cISADesigns  = cISADesigns))



    lScen <- list( cScen1 = cScen1 )
    vName <- c( "cScen1" )

    # Add the additional scenario - Each scenario would increase the % of patients that are R
    #Scenario 2 -
    cScen          <- cScen1

    #In the next two lines we are changing the $ responders for the treatment arm in BOTH ISA 1 and ISA 2
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.05, 0.95 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.05, 0.95 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.05, 0.95 )
    cScen$Scen     <- 2
    lScen[[ 2 ]]   <- cScen
    vName          <- c( vName, "cScen2" )
    names( lScen ) <- vName

    #Scenario 3
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.10, 0.9 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.10, 0.9 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.10, 0.9 )
    cScen$Scen     <- 3
    lScen[[ 3 ]]   <- cScen
    vName          <- c( vName, "cScen3" )
    names( lScen ) <- vName

    #Scenario 4
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.15, 0.85 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.15, 0.85 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.15, 0.85 )
    cScen$Scen     <- 4
    lScen[[ 4 ]]   <- cScen
    vName          <- c( vName, "cScen4" )
    names( lScen ) <- vName

    #Scenario 5
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.2, 0.8 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.2, 0.8 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.2, 0.8 )
    cScen$Scen     <- 5
    lScen[[ 5 ]]   <- cScen
    vName          <- c( vName, "cScen5" )
    names( lScen ) <- vName

    #Scenario 6
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.3, 0.7 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.3, 0.7 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.3, 0.7 )
    cScen$Scen     <- 6
    lScen[[ 6 ]]   <- cScen
    vName          <- c( vName, "cScen6" )
    names( lScen ) <- vName

    #Scenario 7
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.4, 0.6 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.4, 0.6 )
    cScen$cISADesigns$cISA2$cSimOutcomes$lSimArm3$vProbGroup <- c( 0.4, 0.6 )
    cScen$Scen     <- 7
    lScen[[ 7 ]]   <- cScen
    vName          <- c( vName, "cScen7" )
    names( lScen ) <- vName

    # It would be useful to run more scenarios where the effectiveness of the treatment is not
    # the same for ISA 1 and ISA 2
    cSimulation <- structure( list( lScenarios    = lScen,
                                    cTrialDesign  = cTrialDesign
    ))

    lSimulation <- list( SimDesigns = list(cSimulation))

    return( lSimulation )
}
