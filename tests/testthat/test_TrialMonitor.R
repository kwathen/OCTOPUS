##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for TrialMonitor.R
context("Trial Monitor")
source("TestHelperFunctions.R")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#
# CheckTrialMonitor.default(  cISADesigns, lEnrolledPats,  vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
#
#--------------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------------
#  Check Trial Monitor Case 1 - Which is based on a number of patients plus follow-up
#--------------------------------------------------------------------------------------------------------------------------------------------------
test_that("No Patient Test - Case 1",
    {
        cTrialDesign     <- SetupTrialDesign1ISA()
        lEnrolledPats    <- InitializePatientList( cTrialDesign )
        vISAStatus       <- c( 1 )
        dCurrentTime     <- 0
        vISAAnalysisIndx <- c( 1 )
        vPreviousIATime  <- c( 0 )

        lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

        lRetExp <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(0), vIsFinalISAAnalysis =c(FALSE),  vCase = c(1) )
        expect_equal( lRet, lRetExp )
    }
)

test_that("Invalid cISADesigns",
    {
        cTrialDesign     <- SetupTrialDesign1ISA()
        cTrialDesignErr  <- cTrialDesign
        cTrialDesignErr$cISADesigns[[1]]$vMinQtyPats        <- c( 10, 20,30,40)
        cTrialDesignErr$cISADesigns[[1]]$vMinFUTime         <- c( 10, 20,30,40)
        cTrialDesignErr$cISADesigns[[1]]$dQtyMonthsBtwIA    <- 2

        expect_error( CheckTrialMonitor( cTrialDesignErr$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  ) )


        cTrialDesignErr$cISADesigns[[1]]$dQtyMonthsBtwIA    <- -2  # Invalid, cannot be < 0
        expect_error( CheckTrialMonitor( cTrialDesignErr$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  ) )


        cTrialDesignErr$cISADesigns[[1]]$vMinQtyPats        <- c( 10, 20 )  #Invalid lengths, they must be the same
        cTrialDesignErr$cISADesigns[[1]]$vMinFUTime         <- c( 10, 20,30,40)
        cTrialDesignErr$cISADesigns[[1]]$dQtyMonthsBtwIA    <- 2
        expect_error( CheckTrialMonitor( cTrialDesignErr$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  ) )


        cTrialDesignErr$cISADesigns[[1]]$vMinQtyPats        <- c( 10 )  #Invalid lengths, they must be the same
        cTrialDesignErr$cISADesigns[[1]]$vMinFUTime         <- c( 10 )
        cTrialDesignErr$cISADesigns[[1]]$dQtyMonthsBtwIA    <- 2
        expect_error( CheckTrialMonitor( cTrialDesignErr$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  ) )

    }
)

test_that("Trial Monitor - Option 1",
{
    cTrialDesign     <- SetupTrialDesign1ISA()
    lEnrolledPats    <- InitializePatientList( cTrialDesign )
    vISAStatus       <- c( 1 )
    dCurrentTime     <- 0
    vISAAnalysisIndx <- c( 1 )
    vPreviousIATime  <- c( 0 )

    #Add patients so the 1st analysis should run
    lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ]
    lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ])
    lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ])
    dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 1 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(1), vPreviousIATime = c(dCurrentTime), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
    expect_equal( lRet, lRetExp, label = "Test 1" )



    #Add patients so the 1st analysis should run and make the status of the ISA Closed for max accrual
    vISAStatus       <- c( 2 )
    lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ]
    lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ])
    lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ])
    dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 2 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(1), vPreviousIATime = c(dCurrentTime), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
    expect_equal( lRet, lRetExp , label = "Test 2")



    ####################################################################################################.
    # lEnrolledPats need to have the $lPatOut[[1]] added below this point
    ####################################################################################################.
    vISAStatus       <- c( 1 )
    #Typical use is to copy out the vPreviousIATime
    vPreviousIATime  <- lRet$vPreviousIATime
    vISAAnalysisIndx <- c( 2 )

    dCurrentTime     <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 1 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run
    vPreviousIATime  <- c( dCurrentTime )

    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(dCurrentTime), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
    expect_equal( lRet, lRetExp, label = "Test 4" )


    #Add patients so the 2nd and final analysis should run
    lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 2 ]
    lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 2 ])
    lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 2 ])
    dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 2 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(1), vPreviousIATime = c(dCurrentTime), vIsFinalISAAnalysis =c(TRUE), vCase = c(1))
    expect_equal( lRet, lRetExp, label = "Test 5" )


    vISAStatus[1]    <- 5 # For a status of >=3 the analysis should not be run
    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(dCurrentTime), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
    expect_equal( lRet, lRetExp, label = "Test 6" )


    #Add patients so the 1st analysis should run and make the status of the ISA Closed for max accrual
    vISAStatus       <- c( 5 )
    lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[2 ]
    lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 2 ])
    lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, cTrialDesign$cISADesigns[[1]]$vMinQtyPats[2 ])

    dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 2 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run
    vPreviousIATime[ 1 ] <- dCurrentTime
    dAnalysisTime <- dCurrentTime + 10
    lRet             <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dAnalysisTime, vISAAnalysisIndx, vPreviousIATime  )
    lRetExp          <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(dCurrentTime ), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
    expect_equal( lRet, lRetExp, label = "Test 3" )




})

#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Check Trial Monitor Case 2 - Which is based on a number of patients for the first eval then a specified time between
#--------------------------------------------------------------------------------------------------------------------------------------------------
test_that("No Patient Test - Case 2",
          {
              cTrialDesign     <- SetupTrialDesign1ISA()
              lEnrolledPats    <- InitializePatientList( cTrialDesign )
              vISAStatus       <- c( 1 )
              dCurrentTime     <- 0
              vISAAnalysisIndx <- c( 1 )
              vPreviousIATime  <- c( 0 )
              dQtyMonthsBtwIA  <- 1


              cTrialDesign$cISADesigns[[1]]$dQtyMonthsBtwIA    <- dQtyMonthsBtwIA  #Setting this to > 0 should result in case 2

              lRet    <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

              lRetExp <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(0), vIsFinalISAAnalysis =c(FALSE), vCase = c(2))
              expect_equal( lRet, lRetExp )


              #Add patients so the 1st analysis should run
              nQtyPats                              <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ]
              lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- nQtyPats
              lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, nQtyPats )
              lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, nQtyPats )
              dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 1 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

              lRet    <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
              lRetExp <- list( vRunISAAnalysis=c(1), vPreviousIATime = c(dCurrentTime ), vIsFinalISAAnalysis =c(FALSE), vCase = c(2))
              expect_equal( lRet, lRetExp )

              vPreviousIATime   <- lRetExp$vPreviousIATime
              dCurrentTime      <- dCurrentTime + dQtyMonthsBtwIA/2  # Adding only half the time should result in not running IA
              lRet              <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
              lRetExp           <- list( vRunISAAnalysis=c(0), vPreviousIATime = lRetExp$vPreviousIATime, vIsFinalISAAnalysis =c(FALSE), vCase = c(2))
              expect_equal( lRet, lRetExp )

              vPreviousIATime   <- lRetExp$vPreviousIATime
              dCurrentTime      <- lRetExp$vPreviousIATime + dQtyMonthsBtwIA   # Adding only half the time should result in not running IA
              lRet              <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
              lRetExp           <- list( vRunISAAnalysis=c(1), vPreviousIATime = dCurrentTime, vIsFinalISAAnalysis =c(FALSE), vCase = c(2))
              expect_equal( lRet, lRetExp )


              #Add patients so the 2st analysis should run, and should return that it is the Final Analysis
              nQtyPats                              <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 2 ]
              lEnrolledPats$vCurrentQtyPatsISA[ 1 ]  <- nQtyPats
              lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, nQtyPats )
              lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, nQtyPats )
              dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 2 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

              vPreviousIATime   <- 0.1
              lRet              <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
              lRetExp           <- list( vRunISAAnalysis=c(1), vPreviousIATime = dCurrentTime, vIsFinalISAAnalysis =c(TRUE), vCase = c(2))
              expect_identical( lRet, lRetExp )
          }
)



#--------------------------------------------------------------------------------------------------------------------------------------------------
#   Check Trial Monitor Case 1 - With dQtyMonthsBtwIA = 0 so only 1 IA
#--------------------------------------------------------------------------------------------------------------------------------------------------
test_that("No Patient Test - Case 3 - No IA, just a FA",
          {
              cTrialDesign     <- SetupTrialDesign1ISA()
              lEnrolledPats    <- InitializePatientList( cTrialDesign )
              vISAStatus       <- c( 1 )
              dCurrentTime     <- 0
              vISAAnalysisIndx <- c( 1 )
              vPreviousIATime  <- c( 0 )
              dQtyMonthsBtwIA  <- 0


              cTrialDesign$cISADesigns[[1]]$dQtyMonthsBtwIA    <- dQtyMonthsBtwIA  #Setting this to  0 should result in ISA with the udpated  info
              cTrialDesign$cISADesigns[[1]]$vMinFUTime         <- c( 1 )
              cTrialDesign$cISADesigns[[1]]$vMinQtyPats        <- c( 90 )

              lRet    <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

              lRetExp <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(0), vIsFinalISAAnalysis =c(FALSE), vCase = c(1))
              expect_equal( lRet, lRetExp )


              #Add patients so the 1st analysis should run
              nQtyPats                              <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ]
              lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- nQtyPats
              lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, nQtyPats )
              lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, nQtyPats )
              dCurrentTime <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 1 ] + 2 #Make sure the current time is past the min FU so Analysis should be run

              lRet    <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime )
              lRetExp <- list( vRunISAAnalysis=c(1), vPreviousIATime = c(dCurrentTime ), vIsFinalISAAnalysis =c(TRUE), vCase = c(1))
              expect_equal( lRet, lRetExp )

          }
)

#--------------------------------------------------------------------------------------------------------------------------------------------------
#    Test the results when ISA is not open or it is closed
#--------------------------------------------------------------------------------------------------------------------------------------------------

test_that("ISA Not Open Test",
{
    #Code Here

    cTrialDesign     <- SetupTrialDesign1ISA()
    lEnrolledPats    <- InitializePatientList( cTrialDesign )
    vISAStatus       <- c( 0 )
    dCurrentTime     <- 0
    vISAAnalysisIndx <- c( 1 )
    vPreviousIATime  <- c( 0 )
    dQtyMonthsBtwIA  <- 1


    cTrialDesign$cISADesigns[[1]]$dQtyMonthsBtwIA    <- dQtyMonthsBtwIA  #Setting this to > 0 should result in case 2

    lRet    <- CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime  )

    lRetExp <- list( vRunISAAnalysis=c(0), vPreviousIATime = c(0), vIsFinalISAAnalysis =c(FALSE), vCase = c(2))
    expect_equal( lRet, lRetExp )



    #Add patients which should not occur since ISA is not open, this should trigger an error
    nQtyPats <- cTrialDesign$cISADesigns[[1]]$vMinQtyPats[ 1 ]
    lEnrolledPats$vCurrentQtyPatsISA[ 1 ] <- nQtyPats
    lEnrolledPats$lPatOut[[1]]$vStartTimes             <- rep( 0, nQtyPats )
    lEnrolledPats$lPatOut[[1]]$vISA                    <- rep( 1, nQtyPats )
    dCurrentTime  <- cTrialDesign$cISADesigns[[1]]$vMinFUTime[ 1 ] + 0.1 #Make sure the current time is past the min FU so Analysis should be run

    expect_error(CheckTrialMonitor( cTrialDesign$cISADesigns, lEnrolledPats, vISAStatus, dCurrentTime, vISAAnalysisIndx, vPreviousIATime ))


})
