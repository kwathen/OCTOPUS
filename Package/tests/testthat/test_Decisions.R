##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#############################################################################################################################.
#   Description                                                                                                         #####
#   This file contains tests for all files in MakeDecision
#####   Developer(s): J. Kyle Wathen, PhD                                                                                  #####
#############################################################################################################################.


## Test script for MakeDecisions.R
context("Test - MakeDecision.R")
source("TestHelperFunctions.R")
##

# Each test will created an lExp that gives the expected outcome of each test.



#----- Test MakeDecision.Outcome1Only -----------------------------------------------------------------------------------------


lDecision       <- structure( list( strApproachFA = "Outcome1Only", strApproachIA= "Outcome1Only"), class="General")

#Outcome 2 = Futility


test_that("MakeDecision.Outcome1Only - Outcome 2 Futility",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

    })

# Outcome 2 = Pause
test_that("TwoOutcomeOption1 - Outcome 2 Pause",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_equal( lRet, lExp )

        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

# Outcome 2 = Go
test_that("TwoOutcomeOption1 - Outcome 2 Go",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )


        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })



#----- Test MakeDecision.Outcome2Only  --------------------------------------------------------------------------------------------

lDecision       <- structure( list( strApproachFA = "Outcome2Only", strApproachIA= "Outcome2Only"), class="General")


# Outcome 2 = Futility

test_that("Outcome2Only - Outcome 2 Futility",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

# Outcome 2 = Pause
test_that("Outcome2Only - Outcome 2 Pause",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )


        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

    #Outcome 2 = Go

test_that("Outcome2Only - Outcome 2 No Go",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )


        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )

        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })


#----- Test MakeDecision.TwoOutcomeOption1 ----------------------------------------------------------------------------------------------


lDecision       <- structure( list( strApproachFA = "TwoOutcomeOption1", strApproachIA= "TwoOutcomeOption1"), class="General")

# Outcome 2 = Futility

test_that("TwoOutcomeOption1 - Outcome 2 Futility",
          {
              lResOut1        <- list( nGo = 0, nNoGo = 1, nPause = 0)
              lResOut2        <- list( nGo = 0, nNoGo = 1, nPause = 0 )
              lExp            <- list( nGo = 0, nNoGo = 1, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nGo = 0, nNoGo = 1, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nGo = 1, nNoGo = 0, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

          })



# Outcome 2 = Pause
test_that("TwoOutcomeOption1 - Outcome 2 Pause",
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nGo = 0, nNoGo = 1, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )


              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nGo = 0, nNoGo = 0, nPause = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nGo = 1, nNoGo = 0, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })



# Outcome 2 = Go
test_that("Outcome 2 = Go",
          {

              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nGo = 1, nNoGo = 0, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nGo = 1 , nNoGo = 0, nPause = 0)
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )

              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nGo = 1, nNoGo = 0, nPause = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })



#----- Test MakeDecision.TwoOutcomeOption2 -----------------------------------------------------------------------------------------

lDecision       <- structure( list( strApproachFA = "TwoOutcomeOption2", strApproachIA= "TwoOutcomeOption2"), class="General")


###
#-----  Outcome 2 = NG

test_that("TwoOutcomeOption2 - Outcome 2 No Go Case 1",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 No Go Case 2",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 No Go Case 3",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

#-  Outcome 2 = Pause
test_that("TwoOutcomeOption2 - Outcome 2 Pause Case 4",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 Pause Case 5",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 Pause Case 6",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })



#Outcome 2 = Go

test_that("TwoOutcomeOption2 - Outcome 2 Go  Case 7",
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 Go Case 8",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that("TwoOutcomeOption2 - Outcome 2 Go Case 9",
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })




#----- Test MakeDecision.TwoOutcomeOption3 -----------------------------------------------------------------------------------------
#               G | G   G   G
#  Outcome 2    P | P   P   G
#               NG| NG  P   G
#                   ---------  .
#                   NG  P   G
#                    Outcome 1

strOptionName   <- "TwoOutcomeOption3"
lDecision       <- structure( list( strApproachFA = strOptionName, strApproachIA= strOptionName), class="General")

#-----  Outcome 2 = NG

test_that(paste( strOptionName, " - Outcome 2 No Go Case 1"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 2"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 3"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

#-  Outcome 2 = Pause
test_that(paste( strOptionName, " - Outcome 2 Pause Case 4"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 5"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " Outcome 2 Pause Case 6"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })



#Outcome 2 = Go

test_that(paste( strOptionName, " - Outcome 2 Go  Case 7"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 8"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 9"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })





#----- Test MakeDecision.TwoOutcomeOption4 -----------------------------------------------------------------------------------------
#
#               G | NG  G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  NG  G
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#-----------------------------------------------------------------------------------------------------------------------------------.


strOptionName   <- "TwoOutcomeOption4"
lDecision       <- structure( list( strApproachFA = strOptionName, strApproachIA= strOptionName), class="General")

#-----  Outcome 2 = NG

test_that(paste( strOptionName, " - Outcome 2 No Go Case 1"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 2"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 3"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

#-  Outcome 2 = Pause
test_that(paste( strOptionName, " - Outcome 2 Pause Case 4"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 5"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " Outcome 2 Pause Case 6"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })



#Outcome 2 = Go

test_that(paste( strOptionName, " - Outcome 2 Go  Case 7"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 8"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 9"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })


#----- Test MakeDecision.TwoOutcomeOption5 -----------------------------------------------------------------------------------------
#
# TwoOutcomeOption5 - D ;  P = Pause; NG = No Go; G = Go
#               G | NG  G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  P   G
#                   --------- .
#                   NG  P   G
#                    Outcome 1
#
#-----------------------------------------------------------------------------------------------------------------------------------.

strOptionName   <- "TwoOutcomeOption5"
lDecision       <- structure( list( strApproachFA = strOptionName, strApproachIA= strOptionName), class="General")


# Outcome 2 = NG

test_that(paste( strOptionName, " - Outcome 2 No Go Case 1"),
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 2"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 3"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

# Outcome 2 = Pause
test_that(paste( strOptionName, " - Outcome 2 Pause Case 4"),
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 5"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 6"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

#Outcome 2 = Go
test_that(paste( strOptionName, " - Outcome 2 Go Case 7"),
    {
        lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 Go Case 8"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })

test_that(paste( strOptionName, " - Outcome 2 Go Case 9"),
    {
        lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
        lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
        expect_true( AreListsEqual( lRet, lExp ) )
    })


#----- Test MakeDecision.TwoOutcomeOption7 -----------------------------------------------------------------------------------------
#
# TwoOutcomeOption5 - D ;  P = Pause; NG = No Go; G = Go
#               G | P   G   G
#  Outcome 2    P | NG  P   G
#               NG| NG  P   G
#                   --------- .
#                   NG  P   G
#                    Outcome 1
#
#-----------------------------------------------------------------------------------------------------------------------------------.

strOptionName   <- "TwoOutcomeOption7"
lDecision       <- structure( list( strApproachFA = strOptionName, strApproachIA= strOptionName), class="General")


# Outcome 2 = NG

test_that(paste( strOptionName, " - Outcome 2 No Go Case 1"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 2"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 3"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

# Outcome 2 = Pause
test_that(paste( strOptionName, " - Outcome 2 Pause Case 4"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 5"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 6"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

#Outcome 2 = Go
test_that(paste( strOptionName, " - Outcome 2 Go Case 7"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 8"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 9"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })




#----- Test MakeDecision.TwoOutcomeOption11 -----------------------------------------------------------------------------------------
#
#               G | P   G   G
#  Outcome 2    P | P   P   G
#               NG| NG  P   P
#                   ---------  .
#                   NG  P   G
#                    Outcome 1
#
#-----------------------------------------------------------------------------------------------------------------------------------.

strOptionName   <- "TwoOutcomeOption11"
lDecision       <- structure( list( strApproachFA = strOptionName, strApproachIA= strOptionName), class="General")


# Outcome 2 = NG

test_that(paste( strOptionName, " - Outcome 2 No Go Case 1"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 2"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 No Go Case 3"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

# Outcome 2 = Pause
test_that(paste( strOptionName, " - Outcome 2 Pause Case 4"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 5"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Pause Case 6"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

#Outcome 2 = Go
test_that(paste( strOptionName, " - Outcome 2 Go Case 7"),
          {
              lResOut1        <- list( nNoGo = 1, nPause = 0, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 8"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 1, nGo = 0 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

test_that(paste( strOptionName, " - Outcome 2 Go Case 9"),
          {
              lResOut1        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lResOut2        <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lExp            <- list( nNoGo = 0, nPause = 0, nGo = 1 )
              lRet            <- MakeDecision( lDecision, list( lResOut1, lResOut2  ), bFinalAnalysis=TRUE  )
              expect_true( AreListsEqual( lRet, lExp ) )
          })

