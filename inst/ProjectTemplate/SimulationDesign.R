##### File Description ######################################################################################################
#  Setup the simulation object. 
#
#############################################################################################################################.

SetupSimulations <- function( cTrialDesign, nQtyReps  )
{

    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

    nQtyCol <- length( vObsTime1 )


    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1

    #############################################.
    # Setup ISA1 cISADesign element          ####
    #############################################.

    cISAStart1   <- structure( list( dParam1=0), class="SetTime" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime


    cSimOutcome1 <- structure(list( vProbResponse = c(0.2, 0.2 )), class=c("Binary"))


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


    cSimOutcome1 <- structure(list( vProbResponse = c(0.2, 0.2 ) ), class=c("Binary"))


    cISA2Info <- structure( list(cSimOutcomes = cSimOutcome1,
                                 cSimISAStart = cISAStart2 ) )
    
    
    ##################################################################################.
    # Setup ISA3 cISADesign element                                               ####
    # Note: For the cISAStart times the class options include SetTime or Uniform
    # See ?SimulateISAStartTime.SetTime or ?SimulateISAStartTime.Uniform
    ##################################################################################.
    
    # ISA 3 enters the platform at a random time between month 12 and 18.
    cISAStart3   <- structure( list( dParam1=12, dParam2=18), class="Uniform" )
    
    
    vObsTime1    <- cTrialDesign$cISADesigns$cISA2$cISAAnalysis$vAnalysis[[1]]$vObsTime
    
    
    cSimOutcome1 <- structure(list( vProbResponse = c(0.2, 0.2 ) ), class=c("Binary"))
    
    
    cISA3Info <- structure( list(cSimOutcomes = cSimOutcome1,
                                 cSimISAStart = cISAStart3 ) )

    #############################################.
    # Setup the cISADesign element           ####
    #############################################.


    cISADesigns <- structure( list( cISA1 = cISA1Info,
                                    cISA2 = cISA2Info,
                                    cISA3 = cISA3Info ) )

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
    cScen$cISADesigns$cISA1$cSimOutcomes$vProbResponse <- c( 0.2, 0.3 )
    cScen$cISADesigns$cISA2$cSimOutcomes$vProbResponse <- c( 0.2, 0.3 )
    cScen$cISADesigns$cISA3$cSimOutcomes$vProbResponse <- c( 0.2, 0.3 )
    cScen$Scen     <- 2
    lScen[[ 2 ]]   <- cScen
    vName          <- c( vName, "cScen2" )
    names( lScen ) <- vName

    #Scenario 3
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$vProbResponse <- c( 0.2, 0.99 )
    cScen$cISADesigns$cISA2$cSimOutcomes$vProbResponse <- c( 0.2, 0.99 )
    cScen$cISADesigns$cISA3$cSimOutcomes$vProbResponse <- c( 0.2, 0.99 )
    cScen$Scen     <- 3
    lScen[[ 3 ]]   <- cScen
    vName          <- c( vName, "cScen3" )
    names( lScen ) <- vName

    # It would be useful to run more scenarios where the effectiveness of the treatment is not
    # the same for ISA 1 and ISA 2
    cSimulation <- structure( list( lScenarios    = lScen,
                                    cTrialDesign  = cTrialDesign
    ))

    lSimulation <- list( SimDesigns = list(cSimulation))

    return( lSimulation )
}
