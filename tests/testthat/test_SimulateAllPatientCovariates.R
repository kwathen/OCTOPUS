##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for SimulateAllPatientCovariates.R
context("Test - SimulateAllPatientCovaraites.R")

source("TestHelperFunctions.R")


test_that("SimulateAllPatientCovariates.default",
{
    cSimulateCovariates = SetupSimulateCovariateObject( )
    cTrialDesign        = structure(list( nMaxQtyPats = 100 ), class="default")
    lPatCovs            = SimulateAllPatientCovariates( cSimulateCovariates, cTrialDesign )
    expect_equal( length( lPatCovs ), 2 )
    expect_gte( length( lPatCovs[[ 1 ]] ), 100 )
    expect_gte( length( lPatCovs[[ 2 ]] ), 100 )
    expect_equal(length( lPatCovs[[ 1 ]] ), length( lPatCovs[[ 2 ]] ) )

    # Test Covariate 1 #####
    expect_true( all( lPatCovs[[ 1 ]] ==1 |lPatCovs[[ 1 ]] == 2 ) )

    # Test Covariate 2 ####
    expect_true( all( lPatCovs[[ 2 ]] ==1 |lPatCovs[[ 2 ]] == 2 |lPatCovs[[ 2 ]] == 3 ) )

} )
