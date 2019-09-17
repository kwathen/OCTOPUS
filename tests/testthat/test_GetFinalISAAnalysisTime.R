##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for GetFinalISAAnalysisTime.R
context("Test GetFinalISAAnalysisTime.R")
source("TestHelperFunctions.R")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#
# GetFinalISAAnalysisTime.IndependentISA(cISADesigns, nISA, lEnrolledPats)
#
#--------------------------------------------------------------------------------------------------------------------------------------------------

test_that("GetFinalISAAnalysisTime - Single ISA",
{
    cTrialDesign     <- SetupTrialDesign1ISA()
    lEnrolledPats    <- InitializePatientList( cTrialDesign )
    vISAStatus       <- c( 1 )
    dCurrentTime     <- 0
    vISAAnalysisIndx <- c( 1 )
    vPreviousIATime  <- c( 0 )
    lEnrolledPats$lPatOut[[1]]$vStartTimes <- 1:90

    dRet    <- GetFinalISAAnalysisTime( cTrialDesign$cISADesigns, 1, lEnrolledPats )

    dRetExp <- 12/52 * 12 +  90.0    #The last patient enrolls at 90 and add the 12 weeks
    expect_equal( dRet, dRetExp )
})


test_that("GetFinalISAAnalysisTime - Single ISA",
          {
              cTrialDesign     <- SetupTrialDesign2ISA( )
              lEnrolledPats    <- InitializePatientList( cTrialDesign )
              vISAStatus       <- c( 1, 1 )
              dCurrentTime     <- 0
              vISAAnalysisIndx <- c( 1 )
              vPreviousIATime  <- c( 0 )
              lEnrolledPats$lPatOut[[1]]$vStartTimes <- 2 * (1:cTrialDesign$vMaxQtyPatsInISA[1])
              lEnrolledPats$lPatOut[[2]]$vStartTimes <- 3 * (1:cTrialDesign$vMaxQtyPatsInISA[2])
              lEnrolledPats$vISA        <- rep( c(1,2), cTrialDesign$nMaxQtyPats/2 )

              dRet    <- GetFinalISAAnalysisTime( cTrialDesign$cISADesigns, 1, lEnrolledPats )
              dRet2   <- GetFinalISAAnalysisTime( cTrialDesign$cISADesigns, 2, lEnrolledPats )


              dRetExp <- 12/52 * 12 +  cTrialDesign$vMaxQtyPatsInISA[1]*2    #The last patient enrolls at 90 and add the 12 weeks
              dRetExp2 <- 12/52 * 12 +  cTrialDesign$vMaxQtyPatsInISA[2]*3   #The last patient enrolls at 90 and add the 12 weeks
              expect_equal( dRet, dRetExp )
              expect_equal( dRet2, dRetExp2 )
          })

