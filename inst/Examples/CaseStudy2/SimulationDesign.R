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

    vMeanTrt  <- c( 3.5, 3.4, 3.0, 2.5,  2.9, 1.4, 0.9 )
    vMeanCtrl <- c( 3.5, 3.5, 3.5, 3.5,  3.5, 3.5, 3.5 )

    mVarCov   <- matrix( c( 0.5, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.5, 0.4, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.5, 0.4, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.5, 0.4, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.4,
                            0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5), ncol=nQtyCol )

    mMeanNull <- rbind( vMeanCtrl, vMeanCtrl)
    mMeanAlt  <- rbind( vMeanCtrl, vMeanTrt )

    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1

    #############################################.
    #     Setup ISA1 cISADesign element     #####
    #############################################.

    cISAStart1   <- structure( list( dParam1=0), class="SetTime" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime


    ### Simulate Outcome
    cSimOutcome     <- structure( list( mMean      = mMeanNull,
                                        mVarCov    = mVarCov,
                                        vObsTime   = vObsTime1 ), class= "MVN")




    cISA1Info <- structure( list(cSimOutcomes = cSimOutcome,
                                 cSimISAStart = cISAStart1 ) )


    #############################################.
    # Setup the cISADesign element         ######
    #############################################.

    cISADesigns <- structure( list( cISA1 = cISA1Info ) )



    ##########################################################.
    #      Setup the non-ISA specific info and scenarios #####
    ##########################################################.


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


    ########################################################################################.
    # Recruitment rates, how many patient are expected to enroll each month in the trial ####
    ########################################################################################.

    # The infomraiton provided is in terms of the number of patients per month per site and
    #

    vPatsPerMonthPerSite1   <- c(0.1, 0.3, 0.45, 0.5, 0.5, 0.5 )
    vQtyOfSitesPlat         <- c(3,     8,  15,   35,  50, 70)
    vQtyOfPatsPerMonth1     <- vPatsPerMonthPerSite1 * vQtyOfSitesPlat

    ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth1, nMaxQtyPatients = nMaxQtyPats )
    ########################################################################################.



    ########################################################################################.
    # Setup the scenarios                                                               ####
    ########################################################################################.

    cScen1 <- structure(list(cAcc         = ap,
                             nDesign      = nDesign,
                             Scen         = 1,
                             nQtyReps     = nQtyReps,
                             nPrintDetail = 0,
                             cISADesigns  = cISADesigns))

    # The next block of code could be replaced by a loop and allow the user to send the fraction of the difference
    # but for this example it is just copied for clarity,

    lScen <- list( cScen1 = cScen1 )
    vName <- c( "cScen1" )

    vMeanTrt1   <- vMeanCtrl + 0.1 * (vMeanTrt - vMeanCtrl )
    cScen       <- cScen1
    cScen$Scen  <- 2
    cScen$cISADesigns$cISA1$cSimOutcomes$mMean  <-  rbind( vMeanCtrl, vMeanTrt1 )

    lScen[[ 2 ]] <- cScen
    vName <- c( vName, "cScen2" )
    names( lScen ) <- vName


    vMeanTrt1   <- vMeanCtrl + 0.2 * (vMeanTrt - vMeanCtrl )
    cScen       <- cScen1
    cScen$Scen  <- 3
    cScen$cISADesigns$cISA1$cSimOutcomes$mMean  <-  rbind( vMeanCtrl, vMeanTrt1 )

    lScen[[ 3 ]] <- cScen
    vName <- c( vName, "cScen3" )
    names( lScen ) <- vName


    vMeanTrt1   <- vMeanCtrl + 0.3 * (vMeanTrt - vMeanCtrl )
    cScen       <- cScen1
    cScen$Scen  <- 4
    cScen$cISADesigns$cISA1$cSimOutcomes$mMean  <-  rbind( vMeanCtrl, vMeanTrt1 )

    lScen[[ 4 ]] <- cScen
    vName <- c( vName, "cScen4" )
    names( lScen ) <- vName


    vMeanTrt1    <- vMeanCtrl + 0.4 * (vMeanTrt - vMeanCtrl )
    cScen       <- cScen1
    cScen$Scen  <- 5
    cScen$cISADesigns$cISA1$cSimOutcomes$mMean  <-  rbind( vMeanCtrl, vMeanTrt1 )

    lScen[[ 5 ]] <- cScen
    vName <- c( vName, "cScen5" )
    names( lScen ) <- vName


    vMeanTrt1    <- vMeanCtrl + 1.0 * (vMeanTrt - vMeanCtrl )
    cScen       <- cScen1
    cScen$Scen  <- 6
    cScen$cISADesigns$cISA1$cSimOutcomes$mMean  <-  rbind( vMeanCtrl, vMeanTrt1 )

    lScen[[ 6 ]] <- cScen
    vName <- c( vName, "cScen6" )
    names( lScen ) <- vName

    cSimulation <- structure( list( lScenarios    = lScen,
                                    cTrialDesign  = cTrialDesign
    ))

    lSimulation <- list( SimDesigns = list(cSimulation))

    return( lSimulation )
}
