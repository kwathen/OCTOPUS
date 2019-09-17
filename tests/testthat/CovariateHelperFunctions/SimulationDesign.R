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


    Group1 = list( Cov1 = 1, Cov2 = 1, vMean=rep( 1, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA )
    Group2 = list( Cov1 = 1, Cov2 = 2, vMean=rep( 2, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA)
    Group3 = list( Cov1 = 1, Cov2 = 3, vMean=rep( 3, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA)
    Group4 = list( Cov1 = 2, Cov2 = 1, vMean=rep( 4, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA)
    Group5 = list( Cov1 = 2, Cov2 = 2, vMean=rep( 5, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA)
    Group6 = list( Cov1 = 2, Cov2 = 3, vMean=rep( 6, 7 ), mVarCov = diag( .1, nrow = 7 ), dMinimum = NA, dMaximum = NA)


    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1

    #############################################.
    # Setup ISA1 cISADesign element          ####
    #############################################.

    cISAStart1   <- structure( list( dParam1=0), class="SetTime" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime

    ### Control Arm
    lSimArm1 = list( Group1, Group2, Group3, Group4, Group5, Group6  )
    names( lSimArm1 ) <- paste( "Group", 1:6, sep="")




    cSimOutcome1 <- structure(list( lSimArm1 = lSimArm1,
                                    lSimArm2 = lSimArm1,
                                    vObsTime = vObsTime1 ), class=c("MVNWithCovariates"))




    cISA1Info <- structure( list(cSimOutcomes = cSimOutcome1,
                                 cSimISAStart = cISAStart1 ) )



    ##################################################################################.
    # Setup ISA2 cISADesign element                                               ####
    # Note: For the cISAStart times the class options include SetTime or Uniform
    # See ?SimulateISAStartTime.SetTime or ?SimulateISAStartTime.Uniform
    ##################################################################################.

    # ISA 2 enters the platform at a random time between month 3 and 6.
    cISAStart2   <-  structure( list( dParam1=0), class="SetTime" ) #structure( list( dParam1=3, dParam2=6), class="Uniform" )


    vObsTime1    <- cTrialDesign$cISADesigns$cISA2$cISAAnalysis$vAnalysis[[1]]$vObsTime




    cSimOutcome1 <- structure(list( lSimArm1 = lSimArm1,
                                    lSimArm2 = lSimArm1,
                                    lSimArm3 = lSimArm1,
                                    vObsTime = vObsTime1), class=c("MVNWithCovariates"))

    cSimOutcome2 <- structure(list( lSimArm1 = lSimArm1,
                                    lSimArm2 = lSimArm1,
                                    lSimArm3 = lSimArm1,
                                    vObsTime = vObsTime1 ), class=c("MVNWithCovariates"))

    cSimOutcomes <- structure( list( cSimOutcome1 = cSimOutcome1 ), class="Independent")


    cISA2Info <- structure( list(cSimOutcomes = cSimOutcomes,
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



    #TODO(Covs) - Make this more generic for builidng the cov simulator
    cCov1 = structure( list( vProbs = c( 0.5, 0.5 ) ), class="Categorical" )
    cCov2 = structure( list( vProbs = c( 0.33, 0.33, 0.34 ) ), class="Categorical" )
    cSimCovariates = structure( list( Cov1 = cCov1, Cov2 = cCov2 ), class="default" )

    cScen1 <- structure(list(cAcc           = ap,
                             nDesign        = nDesign,
                             Scen           = 1,
                             nQtyReps       = nQtyReps,
                             nPrintDetail   = 0,
                             cISADesigns    = cISADesigns,
                             cSimCovariates = cSimCovariates ))



    lScen <- list( cScen1 = cScen1 )
    vName <- c( "cScen1" )

    # Add the additional scenario - Each scenario would increase the % of patients that are R
    #Scenario 2 -
    cScen          <- cScen1

    #In the next two lines we are changing the $ responders for the treatment arm in BOTH ISA 1 and ISA 2

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
