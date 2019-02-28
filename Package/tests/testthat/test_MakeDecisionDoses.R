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
#   Developer(s): J. Kyle Wathen, PhD                                                                                   #####
#############################################################################################################################.


## Test script for MakeDecisions.R
context("Test - MakeDecisionDoses.R")
source("TestHelperFunctions.R")
##

# Each test will created an lExp that gives the expected outcome of each test.



#----- Test MakeDecisionDoses.AtLeastOne -----------------------------------------------------------------------------------------



MakeDoseDecision <- function( lDose1, lDose2, strClass )
{
    return( structure( list( lDose1, lDose2), class = strClass ))
}

strDoseClass <- "AtLeastOne"
test_that("MakeDecisionDoses.AtLeastOne",
{
    lDose1          <- list( nNoGo = 1, nPause = 0, nGo = 0 )
    lDose2          <- list( nNoGo = 1, nPause = 0, nGo = 0 )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    lRet            <- MakeDecisionDoses( lDoseDec)
    lExp            <- list(  nGo = 0, nNoGo = 1, nPause = 0 )
    expect_true( AreListsEqual( lRet, lExp ) )

    lDose1          <- list( nNoGo = 0, nPause = 1, nGo = 0 )
    lDose2          <- list( nNoGo = 1, nPause = 0, nGo = 0 )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    lRet            <- MakeDecisionDoses( lDoseDec)
    lExp            <- list(  nGo = 0, nNoGo = 0, nPause = 1 )
    expect_true( AreListsEqual( lRet, lExp ) )

    lDose1          <- list( nNoGo = 0, nPause = 1, nGo = 0 )
    lDose2          <- list( nNoGo = 0, nPause = 1, nGo = 0 )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    lRet            <- MakeDecisionDoses( lDoseDec)
    lExp            <- list(  nGo = 0, nNoGo = 0, nPause = 1 )
    expect_true( AreListsEqual( lRet, lExp ) )

    lDose1          <- list( nNoGo = 0, nPause = 0, nGo = 1 )
    lDose2          <- list( nNoGo = 0, nPause = 1, nGo = 0 )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    lRet            <- MakeDecisionDoses( lDoseDec)
    lExp            <- list(  nGo = 1, nNoGo = 0, nPause = 0 )
    expect_true( AreListsEqual( lRet, lExp ) )

    lDose1          <- list( nNoGo = 0, nPause = 0, nGo = 1 )
    lDose2          <- list( nNoGo = 0, nPause = 1, nGo = 1 )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    lRet            <- MakeDecisionDoses( lDoseDec)
    lExp            <- list(  nGo = 1, nNoGo = 0, nPause = 0 )
    expect_true( AreListsEqual( lRet, lExp ) )

    #This test should throw an error
    lDose1          <- list( nNoGo = 0,  nPause = 0, nGo = 1 )
    lDose2          <- list( nNoGo = NA, nPause = NA, nGo = NA )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    expect_error( MakeDecisionDoses( lDoseDec) )

    #This test should throw an error - second dose does not define the outcomes
    lDose1          <- list( nNoGo = 0,  nPause = 0, nGo = 1 )
    lDose2          <- list(  )
    lDoseDec        <- MakeDoseDecision( lDose1, lDose2, strDoseClass )
    expect_error( MakeDecisionDoses( lDoseDec) )


})
