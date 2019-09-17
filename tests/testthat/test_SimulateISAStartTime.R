##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for TrialMonitor.R
context("Test - SimulateISAStartTime.R")
library( "OCTOPUS")

#----------------------------------------------------------------------------------------------
#
# Test the function InitializePatientList( cTrialDesign )
#
# Note: I am using a variable name cTTrialDesign to make sure it does not appear accidentally anywhere
#       in the library and cause the code to work when it should not.
#----------------------------------------------------------------------------------------------



test_that("SimulateISAStartTime.SetTime",
{
    cParams     <- structure( list( dParam1 = 2.0), class= "SetTime" )
    dStartTime  <- SimulateISAStartTime( cParams )
    expect_equal( dStartTime, cParams$dParam1 )

    cParams     <- structure( list( dParam1 = 4.0), class= "SetTime" )
    dStartTime  <- SimulateISAStartTime( cParams )
    expect_equal( dStartTime, cParams$dParam1 )
})

test_that("SimulateISAStartTime.Uniform",
{
    cParams     <- structure( list( dParam1 = 2.0, dParam2 = 3.0), class= "Uniform" )
    for( i in 1:20 )
    {
        dStartTime  <- SimulateISAStartTime( cParams )
        bInRange    <- cParams$dParam1 < dStartTime & dStartTime < cParams$dParam2
        expect_true( bInRange )
    }

})
