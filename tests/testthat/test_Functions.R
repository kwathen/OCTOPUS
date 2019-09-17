##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for SimulateAllPatientCovariates.R
context("Test - Functions.R")

# source("TestHelperFunctions.R")

# source("TestHelperFunctions.R")
# require( "OCTOPUS" )
#Each test will created an lExp that gives the expected outcome of each test.

##
 # cTrialDesign <- SetupTrialDesign2ISASimple( )
 # nQtyRep      <- 1
 # cSimulation  <- SetupSimulations( cTrialDesign, nQtyRep )
 #
 # lPatOut1         <- SimulateAllPatientOutcomes( cSimulation$SimDesigns[[1]]$lScenarios$cScen1,  cTrialDesign)
 # save( lPatOut1, file="PatOut1.RData" )
 #
 # lPatOut2         <- SimulateAllPatientOutcomes( cSimulation$SimDesigns[[1]]$lScenarios$cScen1,  cTrialDesign)
 # save( lPatOut2, file="PatOut2.RData" )

TestISA = function( lPatOut1, lPatOut2, lFullList, strISALabel = "" )
{

    expect_equal( lFullList$nQtyOut, lPatOut1$nQtyOut, label = paste(strISALabel, " - Outcome 1: nQtyOut") )
    expect_equal( lFullList$vPatTrt, c( lPatOut1$vPatTrt, lPatOut2$vPatTrt), label = paste(strISALabel, "vTreatment List") )

    #### Test Outcome 1   #####
    vDimFull <- dim( lFullList$mSimOut1 )
    vDimL1   <- dim( lPatOut1$mSimOut1 )
    vDimL2   <- dim( lPatOut2$mSimOut1 )
    expect_equal( vDimFull[ 1 ], vDimL1[ 1 ] + vDimL2[ 1 ], label = paste(strISALabel, "Outcome 1 dim 1 test"))
    expect_equal( vDimFull[ 2 ], vDimL1[ 2 ] , label = paste(strISALabel, "- Outcome 1 dim 2 test") )
    expect_equal( lFullList$vObsTime1, lPatOut1$vObsTime1, label = paste(strISALabel, "Outcome 1: vObsTime test" ))

    ##### Test Outcome 2 #####
    vDimFull <- dim( lFullList$mSimOut2 )
    vDimL1   <- dim( lPatOut1$mSimOut2 )
    vDimL2   <- dim( lPatOut2$mSimOut2 )
    expect_equal( vDimFull[ 1 ], vDimL1[ 1 ] + vDimL2[ 1 ], label = paste(strISALabel, "- Outcome 2 dim 1 test") )
    expect_equal( vDimFull[ 2 ], vDimL1[ 2 ], label = paste(strISALabel, "- Outcome 2 dim 2 test"))
    expect_equal( lFullList$vObsTime2, lPatOut1$vObsTime2, label = paste(strISALabel, "Outcome 2: vObsTime test" ) )

}


test_that("AppendPatientLists",
{
    load( "PatOut1.RData")
    load( "PatOut2.RData")

    lFullList <- AppendPatientLists( lPatOut1, lPatOut2 )
    expect_equal( length( lFullList ), length( lPatOut1 ), label = "Full list length = original list length" )

    TestISA( lPatOut1[[ 1 ]], lPatOut2[[ 1 ]], lFullList[[1]], "ISA 1")
    TestISA( lPatOut1[[ 2 ]], lPatOut2[[ 2 ]], lFullList[[2]], "ISA 2")
    #### Test ISA 1 ####

    expect_equal( lFullList[[1]]$nQtyOut, lPatOut1[[ 1 ]]$nQtyOut, label = "ISA 1 - Outcome 1: nQtyOut" )
    expect_equal( lFullList[[1]]$vPatTrt, c( lPatOut1[[1]]$vPatTrt, lPatOut2[[1]]$vPatTrt), label = "ISA 1 - vTreatment List" )

} )



