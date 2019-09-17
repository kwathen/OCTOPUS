##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


context("Test SimulateSingleTrial.R")
source("TestHelperFunctions.R")
require( "OCTOPUS" )
require("nlme")

#Each test will created an lExp that gives the expected outcome of each test.

Sys.setenv( SGE_TASK_ID = 1)

cTrialDesign <- SetupTrialDesign2ISASimple( )
nQtyRep      <- 1
cSimulation  <- SetupSimulations( cTrialDesign, nQtyRep )

#---------------------------------------------------------------------------------------------- -
#
# Test  - The trial randomizer
#
#---------------------------------------------------------------------------------------------- -


test_that("RunSimulation - Outputfiles",
    {

        #Now perform a few random tests to make sure the randomization to the ISAs are equal
        CleanSimulationDirectories( )
        Sys.setenv( SGE_TASK_ID = 1)

        gDebug       <<- FALSE

        set.seed(123)
        lRet = RunSimulation( cSimulation )

        bFileExists = file.exists("enrollment/1enroll1.csv")
        expect_true( bFileExists, label="Enrollment/enroll-1.csv does not exist")

        bFileExists = file.exists("ISAOut1/1isaout1.csv")
        expect_true( bFileExists, label="/ISAOut1/isaout-1.csv does not exist")

        bFileExists = file.exists("ISAOut2/1isaout1.csv")
        expect_true( bFileExists, label="/ISAOut2/1isaout1.csv does not exist")

        bFileExists = file.exists("out/1out1.csv")
        expect_true( bFileExists, label="/out/1out1.csv does not exist")

        Sys.setenv( SGE_TASK_ID = 2)
        set.seed(124)
        lRet = RunSimulation( cSimulation )

        bFileExists = file.exists("enrollment/enroll2.csv")
        expect_true( bFileExists, label="Enrollment/enroll2.csv does not exist")

        bFileExists = file.exists("ISAOut1/isaout2.csv")
        expect_true( bFileExists, label="/ISAOut1/isaout2.csv does not exist")

        bFileExists = file.exists("ISAOut2/isaout2.csv")
        expect_true( bFileExists, label="/ISAOut2/isaout2.csv does not exist")

        bFileExists = file.exists("out/out2.csv")
        expect_true( bFileExists, label="/out/out2.csv does not exist")




    })

