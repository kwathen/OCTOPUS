##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

## Test script for SimulateAllPatientCovariates.R
context("Test - SimulatePatientCovaraites.R")

source("TestHelperFunctions.R")


test_that("SimulatePatientCovariates.default",
{
    set.seed( 123 )
    nMaxQtyPats         = 1000
    cSimulateCovariates = SetupSimulateCovariateObject( )

    cCov1   = cSimulateCovariates[[ 1 ]]  # Should be a covariate with 2 levels, 1 or 2
    cCov2   = cSimulateCovariates[[ 2 ]]  # Should be a covariate with 3 levels, 1, 2, 3

    # Test Covariate 1 #####
    # Expected results: 1.  the return vector should be of length =  nMaxQtyPats
    # 2.  The returned vector should only have values = 1, 2
    # 3.  Expected 40% to be 1s and 60% to be 2s
    # To test 3, get 10 samples and use a prop-test to see if the values are far off.  For example, if the probs vector
    # was not provided in the sample function this repeated test would catch something wrong.

    nQtyTest = 10
    nTest    = 1 # Counter for the number of tests
    dMinPVal = 0.001
    repeat
    {
        vPatCov1   = SimulatePatientCovariates( cCov1, nMaxQtyPats )
        expect_equal( length( vPatCov1 ), nMaxQtyPats,  label = "The number of simulated patients was incorrect for covariate 1 (Cov1)")
        expect_true( all( vPatCov1 ==1 |vPatCov1 == 2 ) )

        vTable = as.vector( table( vPatCov1 ))
        pt     = prop.test( vTable, rep( nMaxQtyPats, 2 ), p=c(0.4, 0.6) )

        expect_gt( pt$p.value, dMinPVal, label=paste( "SimulatePatientCOvariate (Cov1) - Repeated test gave p-value < ", dMinPVal, ".  Increasing the number of tests, 1 fail is okay.", sep="") )
        if( pt$p.value < dMinPVal && nQtyTest == 10 )
            nQtyTest = 20

        if( nTest == nQtyTest )
            break
        nTest = nTest + 1
    }


    # Test Covariate 2 #####
    # Expected results: 1.  the return vector should be of length =  nMaxQtyPats
    # 2.  The returned vector should only have values = 1, 2, 3
    # 3.  Expected 20% to be 1s and 30% to be 2s, 50% to be 3s
    # To test 3, get 10 samples and use a prop-test to see if the values are far off.  For example, if the probs vector
    # was not provided in the sample function this repeated test would catch something wrong.

    nQtyTest = 10
    nTest    = 1 # Counter for the number of tests
    dMinPVal = 0.001
    repeat
    {
        vPatCov2   = SimulatePatientCovariates( cCov2, nMaxQtyPats )
        expect_equal( length( vPatCov2 ), nMaxQtyPats,  label = "The number of simulated patients was incorrect for covariate 1 (Cov1)")
        expect_true( all( vPatCov2 ==1 |vPatCov2 == 2 |vPatCov2 == 3) )

        vTable = as.vector( table( vPatCov2 ))
        pt     = prop.test( vTable, rep( nMaxQtyPats, 3 ), p=c( 0.2, 0.3, 0.5 ) )

        expect_gt( pt$p.value, dMinPVal, label=paste( "SimulatePatientCOvariate (Cov2) - Repeated test gave p-value < ", dMinPVal, ".  Increasing the number of tests, 1 fail is okay.", sep="") )
        if( pt$p.value < dMinPVal && nQtyTest == 10 )
            nQtyTest = 20

        if( nTest == nQtyTest )
            break
        nTest = nTest + 1
    }

    # Test to make sure an error occures if the sum( vProbs ) != 1
    cCov2$vProbs = c( 0.2, 0.3, 0.2 )  # This vector does not sum = 1
    expect_error( SimulatePatientCovariates( cCov2, nMaxQtyPats)  )

} )
