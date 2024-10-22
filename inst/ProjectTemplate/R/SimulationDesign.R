##### File Description ######################################################################################################
#  Set up the simulation object.
#  cTrialDesign Trial design object often created with SetupTrialDesign()
#  nQtyReps - The number of replications to simulate each scenario
#  vISAStarTime - A vector of start time, in months, for each ISA to begin enrollment.  By default the first ISA should
#                 have a start time of 0.  If the argument is not provided, all ISAs start at time = 0
#   vQtyOfPatsPerMonth - A vector with the expected number of patients enrolled each month for month 1, 2,... length(vQtyOfPatsPerMonth).
#                        The last element will be used as the rate after that time point.  A Poisson process is simulated for the arrival times
#                        using the rates provided.  Example c( 1,3,5, 10 ) would assume on average 1 patient was enrolled in month 1, 3 in month 2,
#                        5 in month 3 and after that, on average 10 patients are enrolled each month
#   nDesign - Integer value with the design ID, IDs should start at 1.
#   dfScenarios - A dataframe with a row for each scenario*ISA. The dataframe must have columns named Scenario and ISA.  Each additional column is added
#                 to the simulation object.  Each scenario must specify a row for each ISA.
#                 Example:
#                   dfScenarios <- data.frame( Scenario = c(1,1,2,2), ISA = c(1,2,1,2), ProbRespCtrl = c(0.2, 0.2, 0.2, 0.2), ProbRespExp =c( 0.2,0.2,0.4,0.4))
#   Would create 2 scenarios where scenario 1 would have th ProbResCtrl =0.2 ProbRespExp = 0.2 for both ISAs and scenario 2 would have ProbResCtrl =0.2 ProbRespExp = 0.4 for both ISAs
#   Items below that need to be check.
#       Recruitment times - the recruitment times for each patient are simulated by a Poisson process but you should
#       specify the number of patient accrued each month.  Which can be done directly or by computing as the product of expected
#       number of patients per month per site and the number of sites enroll each month, see below.
#
#       Add additional scenarios that are for the particular application.
#############################################################################################################################.

SetupSimulations <- function( cTrialDesign,
                              nQtyReps,
                              strSimPatientOutcomeClass,
                              vISAStartTimes = NULL,
                              vQtyOfPatsPerMonth = NULL,
                              nDesign = 1,
                              dfScenarios )
{

    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

    nQtyCol <- length( vObsTime1 )


    #Set up the first Scenario, starting with non-ISA specific information

    nQtyISAs        <- length( cTrialDesign$cISADesigns)

    #TODO(Kyle)- Validate that the dfScenarios either has the same number of ISAs or all ISAs are 1 which then will be applied to all ISAs


    if( is.null( vISAStartTimes ) )
        vISAStartTimes <- rep( 0, nQtyISAs )

    #####################################################.
    # Set up the non-ISA specific info and scenarios ####
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
    # Recruitment rates, how many patient are expected to enroll each month in the trial ####
    ##########################################################################################.
    # The information provided is in terms of the number of patients per month per site and
    #
    if( is.null( vQtyOfPatsPerMonth ) )
    {
        vPatsPerMonthPerSite1   <- c(0.1, 0.3, 0.45, 0.5, 0.5, 0.5 )
        vQtyOfSitesPlat         <- c(3,     8,  15,   35,  50, 70)
        vQtyOfPatsPerMonth      <- vPatsPerMonthPerSite1 * vQtyOfSitesPlat
    }

    ap  <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth, nMaxQtyPatients = nMaxQtyPats )
    #################################################################################.


    #############################################.
    # Set up ISA cISADesign element          ####
    # For each ISA you must specify a start time or provide an interval to simulate the start time for.
    # The first ISA is assumed to start at time 0.  The options are
    # 1. cISAStart   <- structure( list( dParam1 = dStartTime ), class="SetTime" )  # Where dStartTime is the time, in months from trial start that an ISA opens
    # 2. cISAStart   <- structure( list( dParam1=dTime1, dParam2=dTime2), class="Uniform" ) #Simulate uniform time between dTime1 and dTime2
    #    See ?SimulateISAStartTime.SetTime or ?SimulateISAStartTime.Uniform
    #############################################.

    cISADesigns <- list()
    #ReadMe - The example loop below created a list of ISA simulation objects.  Update as needed for particular use case.
    # The goal of the loop is to fill cISADesigns with the info for each ISA.  The elements of cISADesigns should be
    # cISA1, cISA2,...

    vScen      <- unique( dfScenarios$Scenario )
    nScen      <- 1                             # The scenario number since scenarios should be numbered sequentially
    nQtyScen   <- length( vScen )

    lScenarios <- list()
    for( iScen in vScen )
    {
        cISADesigns <- list()
        dfScen <- dfScenarios[ dfScenarios$Scenario== iScen, ]
        for( iISA in 1:nQtyISAs )
        {
            cISAStart    <- structure( list( dParam1 = vISAStartTimes[ iISA ]), class="SetTime" )
            vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

            #Set up the patient simulator - in this example it assume treatment and control both have a response rate of 0.2.  More scenarios are added below
            dfISA        <- dfScen[ dfScen$ISA == iISA, ] %>%  dplyr::select( -c(Scenario, ISA ))
            lISAParams   <- as.list( dfISA )
            cSimOutcome  <- structure( lISAParams, class=c( strSimPatientOutcomeClass ))
            cISAInfo     <- structure( list(cSimOutcomes = cSimOutcome,  cSimISAStart = cISAStart ) )

            cISADesigns[[ paste( "cISA", iISA, sep="" ) ]] <- cISAInfo

        }

        # Example of creating a scenario utilizing cISADesigns created above
        cScen <- structure(list(cAcc          = ap,
                                nDesign      = nDesign,
                                Scen         = nScen,
                                nQtyReps     = nQtyReps,
                                nPrintDetail = 0,
                                cISADesigns  = cISADesigns))
        lScenarios[[ paste0( "cScen", nScen )]] <- cScen
        nScen <- nScen + 1
    }

    # It would be useful to run more scenarios where the effectiveness of the treatment is not
    # the same for ISAs
    cSimulation <- structure( list( lScenarios    = lScenarios,
                                    cTrialDesign  = cTrialDesign ))

    lSimulation <- list( SimDesigns = list(cSimulation))

    return( lSimulation )
}
