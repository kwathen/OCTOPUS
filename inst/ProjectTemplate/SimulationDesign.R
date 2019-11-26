##### File Description ######################################################################################################
#  Setup the simulation object.
#  vISAStarTime - A vector of start time, in months, for each ISA to begin enrollment.  By default the first ISA should
#                 have a start time of 0.  If the argument is not provided, all ISAs start at time = 0
#
#   Items below that need to be check.
#       Recruitment times - the recruitment times for each patient are simualted by a Poission process but you should
#       specify the number of patient accrued each month.  Which ca be done directly or by computing as the product of expected
#       number of patients per month per site and the number of sites enroll each month, see below.
#
#       Add addtional scenarios that are for the particualr application.
#############################################################################################################################.

SetupSimulations <- function( cTrialDesign, nQtyReps,
                              strSimPatientOutcomeClass,
                              vISAStartTimes = NULL)
{

    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

    nQtyCol <- length( vObsTime1 )


    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1
    nQtyISAs        <- length( cTrialDesign$cISADesigns)
    if( is.null( vISAStartTimes ) )
        vISAStartTimes <- rep( 0, nQtyISAs )

    #############################################.
    # Setup ISA cISADesign element          ####
    # For each ISA you must specify a start time or provide an interval to simulate the start time for.
    # The first ISA is assumed to start at time 0.  The options are
    # 1. cISAStart   <- structure( list( dParam1 = dStartTime ), class="SetTime" )  # Where dStartTime is the time, in months from trial start that an ISA opens
    # 2. cISAStart   <- structure( list( dParam1=dTime1, dParam2=dTime2), class="Uniform" ) #Simulate uniform time between dTime1 and dTime2
    #    See ?SimulateISAStartTime.SetTime or ?SimulateISAStartTime.Uniform
    #############################################.

    cISADesigns <- list()
    #ReadMe - The example loop below created a list of ISA simulation objects.  Update as needed for particular use case.
    # The goal of the loop is to fill cISADesigns with the info for each ISA.  The elments of cISADesigns should be
    # cISA1, cISA2,...
    for( iISA in 1:nQtyISAs )
    {
        cISAStart    <- structure( list( dParam1 = vISAStartTimes[ iISA ]), class="SetTime" )
        vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime
        cSimOutcome  <- structure(list( vProbResponse = c(0.2, 0.2 )), class=c( strSimPatientOutcomeClass ))
        cISAInfo     <- structure( list(cSimOutcomes = cSimOutcome,  cSimISAStart = cISAStart ) )

        cISADesigns[[ paste( "cISA", iISA, sep="" ) ]] <- cISAInfo


    }



    #####################################################.
    # Setup the non-ISA specific info and sceanrios ####
    #####################################################.

    nMaxQtyPats <- 0
    i<-1
    if( is.null( cTrialDesign$nMaxQtyPats) )
    {
        for( i in 1:nQtyISAs )
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

    # Example of crating a scenario
    cScen1 <- structure(list(cAcc         = ap,
                             nDesign      = nDesign,
                             Scen         = 1,
                             nQtyReps     = nQtyReps,
                             nPrintDetail = 0,
                             cISADesigns  = cISADesigns))



    lScen <- list( cScen1 = cScen1 )
    vName <- c( "cScen1" )

    # Example of updating and adding another scenario.

    # Add the additional scenario - Each scenario would increase the % of patients that are R
    #Scenario 2 -
    cScen          <- cScen1

    #In the next two lines we are changing the $ responders for the treatment arm in BOTH ISA 1 and ISA 2
    cScen$cISADesigns$cISA1$cSimOutcomes$vProbResponse <- c( 0.2, 0.3 )
    cScen$cISADesigns$cISA2$cSimOutcomes$vProbResponse <- c( 0.2, 0.3 )
    cScen$Scen     <- 2
    lScen[[ 2 ]]   <- cScen
    vName          <- c( vName, "cScen2" )
    names( lScen ) <- vName


    # It would be useful to run more scenarios where the effectiveness of the treatment is not
    # the same for ISA 1 and ISA 2
    cSimulation <- structure( list( lScenarios    = lScen,
                                    cTrialDesign  = cTrialDesign
    ))

    lSimulation <- list( SimDesigns = list(cSimulation))

    return( lSimulation )
}
