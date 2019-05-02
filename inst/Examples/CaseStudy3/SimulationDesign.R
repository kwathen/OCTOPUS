##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


SetupSimulations <- function( cTrialDesign, nQtyReps  )
{
  
    vObsTime1    <- cTrialDesign$cISADesigns$cISA1$cISAAnalysis$vAnalysis[[1]]$vObsTime
    
    nQtyCol <- length( vObsTime1 )
    
    vMeanR  <- c( 3.5, 3.4163, 2.9882, 2.486, 1.9641, 1.4423, 0.9204 )
    vMeanNR <- c( 3.5, 3.4933, 3.4867, 3.48,  3.4733, 3.4667, 3.46 )
    
    #Var-Cov for the Responders
    mVarCovR  <- matrix( c( 0.4907, 0.5011, 0.5825, 0.5472, 0.4469, 0.3310, 0.2373, 
                            0.5011, 0.5410, 0.6734, 0.6511, 0.6256, 0.4055, 0.3219, 
                            0.5825, 0.6734, 1.0830, 1.1700, 1.1802, 0.9835, 0.7006, 
                            0.5472, 0.6511, 1.1700, 1.5653, 1.5963, 1.5766, 1.3795, 
                            0.4469, 0.6256, 1.1802, 1.5963, 2.0096, 2.0096, 2.0503, 
                            0.3310, 0.4055, 0.9835, 1.5766, 2.0096, 2.4810, 2.5628, 
                            0.2373, 0.3219, 0.7006, 1.3795, 2.0503, 2.5628, 3.2684), ncol=nQtyCol )
    
    mVarCovNR <- matrix( c( 0.49,   0.4116, 0.4116, 0.4116, 0.4116, 0.4116, 0.4116,
                            0.4116, 0.49,   0.4116, 0.4116, 0.4116, 0.4116, 0.4116, 
                            0.4116, 0.4116, 0.4900, 0.4116, 0.4116, 0.4116, 0.4116, 
                            0.4116, 0.4116, 0.4116, 0.4900, 0.4116, 0.4116, 0.4116, 
                            0.4116, 0.4116, 0.4116, 0.4116, 0.4900, 0.4116, 0.4116,
                            0.4116, 0.4116, 0.4116, 0.4116, 0.4116, 0.4900, 0.4116, 
                            0.4116, 0.4116, 0.4116, 0.4116, 0.4116, 0.4116, 0.4900), ncol=nQtyCol )
   
    vProbGroupCtrl  <- c( 0.02, 0.98 )       #The percentage of R and NR in the ctrl group

    
    #Setup the first Scenario, starting with non-ISA specific information
    nDesign         <- 1
    
    #############################################
    # Setup ISA1 cISADesign element
    #############################################
    
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
    
    
    
    #############################################
    # Setup the cISADesign element
    #############################################
    
    cISADesigns <- structure( list( cISA1 = cISA1Info ) )
    
    
    
    #############################################
    # Setup the non-ISA specific info and sceanrios
    #############################################
    
    
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
    
    
    #################################################################################
    #Recuitment rates, how many patient are expected to enroll each month in the trial
    #################################################################################
    
    vPatsPerMonthPerSite1   <- c(0.1,0.3,0.45, rep( 0.5, 1000))[1:100]
    
    vQtyOfSitesPlat        <- c(3,8,15,35,50,70,rep( 70,100))[1:100]
    vQtyOfPatsPerMonth1     <- vPatsPerMonthPerSite1 * vQtyOfSitesPlat
    
    ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth1, nMaxQtyPatients = nMaxQtyPats )
    #################################################################################
    
    
    
    
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
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.05, 0.95 )
    cScen$Scen     <- 2
    lScen[[ 2 ]]   <- cScen
    vName          <- c( vName, "cScen2" )
    names( lScen ) <- vName
    
    #Scenario 3 
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.10, 0.9 )
    cScen$Scen     <- 3
    lScen[[ 3 ]]   <- cScen
    vName          <- c( vName, "cScen3" )
    names( lScen ) <- vName
    
    #Scenario 4 
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.15, 0.85 )
    cScen$Scen     <- 4
    lScen[[ 4 ]]   <- cScen
    vName          <- c( vName, "cScen4" )
    names( lScen ) <- vName
    
    #Scenario 5 
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.2, 0.8 )
    cScen$Scen     <- 5
    lScen[[ 5 ]]   <- cScen
    vName          <- c( vName, "cScen5" )
    names( lScen ) <- vName
    
    #Scenario 6
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.3, 0.7 )
    cScen$Scen     <- 6
    lScen[[ 6 ]]   <- cScen
    vName          <- c( vName, "cScen6" )
    names( lScen ) <- vName
    
    #Scenario 7
    cScen          <- cScen1
    cScen$cISADesigns$cISA1$cSimOutcomes$lSimArm2$vProbGroup <- c( 0.4, 0.6 )
    cScen$Scen     <- 7
    lScen[[ 7 ]]   <- cScen
    vName          <- c( vName, "cScen7" )
    names( lScen ) <- vName
    
    cSimulation <- structure( list( lScenarios    = lScen,
                                    cTrialDesign  = cTrialDesign
    ))
  
    lSimulation <- list( SimDesigns = list(cSimulation))
    
    return( lSimulation )
}
