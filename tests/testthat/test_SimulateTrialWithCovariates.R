##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

remove( list=ls() )


context("Test Simulate Trial With Covariates")
source("TestHelperFunctions.R")
require( "PlatformTrialSimulator" )
Sys.setenv(SGE_TASK_ID=1)


####################################################################################################
#   Source the setup files
####################################################################################################
source( "CovariateHelperFunctions/TrialDesign.R")
source( "CovariateHelperFunctions/SimulationDesign.R")
source( "CovariateHelperFunctions/TrialDesignFunctions.R")

cTrialDesign  <- SetupTrialDesign( )
cSimulation   <- SetupSimulations( cTrialDesign, nQtyReps=5 )

gDebug        <- FALSE
gnPrintDetail <- 0

#Files specific for specific example
source( "CovariateHelperFunctions/RunAnalysis.TestWithCovariates.R")



test_that("RunSimulation - Outputfiles",
{

    #Now perform a few random tests to make sure the randomization to the ISAs are equal
    CleanSimulationDirectories( )
    Sys.setenv( SGE_TASK_ID = 1)

    gDebug       <<- FALSE

    RunSimulation( cSimulation )

    ds1 <- read.table( "enrollment/1enroll1.csv", header=TRUE, sep =",")
    ds2 <- read.table( "enrollment/enroll1.csv", header=FALSE, sep =",")
    colnames( ds2 ) <- colnames( ds1 )
    dsAll <- rbind( ds1, ds2)

    vTrialID <- unique( dsAll$TrialID )
    iTrialID <- vTrialID[1]
    for( iTrialID in vTrialID )
    {

        dsISA1 <- dsAll[ dsAll$TrialID== iTrialID & dsAll$ISA == 1, ]
        dsISA2 <- dsAll[ dsAll$TrialID== iTrialID & dsAll$ISA == 2, ]

        #Check to make sure that starting at the 16th patient no patients with Cov1 = 1 were enrolled to ISA1
        vCov1  <- dsISA1[-c(1:15), ]$Cov1
        expect_true( all( vCov1 == 2 ), label="ISA1 had a Cov1 = 1 after the ISA closed enrollment patients with Cov1 = 1.")

        #Check to make sure no patients with Cov1 = 2 were enrolled to ISA 2
        vCov1 <- dsISA2$Cov1
        expect_true( all( vCov1 == 1 ), label="ISA2 had a Cov1 = 2 and ISA 2 is not open to enroll patients with Cov1 = 2.")

    }
})
