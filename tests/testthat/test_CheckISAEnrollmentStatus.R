##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

context("Test - CheckISAEnrollmentStatus.R")
# load( "cRandomizerNoCov.RData")
# cRandomizerNoCov <- cRandomizer
# load( "cRandomizerWithCovs.RData")
# cRandomizerWithCov <- cRandomizer
# rm("cRandomizer")



############### Covariate tests #############################################################################################
#   Tests for trial with covaraites.  In the randomizer with covariates, cRandomzierWithCov ISA1 can enroll for all covariate groups.
#   ISA2 can only enroll when Cov1 = 1
#   Possible values for Cov1 = 1,2
#   Possible values for Cov2 = 1,2,3
#############################################################################################################################.
CreateDataFrameForPatient <- function( nCov1, nCov2 )
{
    return( data.frame( Cov1 = nCov1, Cov2 = nCov2 ))
}


test_that("CheckISAEnrollmentSatus - With covariates",
{
    load( "cRandomizerWithCovs.RData")
    cRandomizerWithCov <- cRandomizer

    # Cov2 = 1
    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 1, 1 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 1 )
    expect_equal( vRetured, vExpected, label = "Test 1: Cov2 = 1" )

    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 2, 1 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 0 )
    expect_equal( vRetured, vExpected, label = "Test 2: Cov2 = 1" )

    # Cov2 = 2
    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 1, 2 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 1 )
    expect_equal( vRetured, vExpected, label = "Test 3: Cov2 = 2" )

    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 2, 2 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 0 )
    expect_equal( vRetured, vExpected, label = "Test 4: Cov2 = 2" )

    # Cov2 = 3
    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 1, 3 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 1 )
    expect_equal( vRetured, vExpected, label = "Test 5: Cov2 = 2" )

    vISAStatus <- c(1,1)
    dfCov      <- CreateDataFrameForPatient( 2, 3 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov )
    vExpected  <- c( 1, 0 )
    expect_equal( vRetured, vExpected, label = "Test 6: Cov2 = 2" )



})



test_that("CheckISAEnrollmentSatus - no covariates",
{
    load( "cRandomizerNoCov.RData")
    cRandomizerNoCov <- cRandomizer

    dfCov      <- NULL

    vISAStatus <- c( 1, 1 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerNoCov, vISAStatus, dfCov )
    expect_equal( vRetured, vISAStatus, label = "Test 1" )

    vISAStatus <- c( 1, 0 )
    vRetured   <- CheckISAEnrollmentStatus( cRandomizerNoCov, vISAStatus, dfCov )
    expect_equal( vRetured, vISAStatus, label = "Test 2" )


})



test_that("CheckISAEnrollmentSatus - Invalid Input",
{

    #Error number 1 - randomizer has covaraites, no covariates provided
    load( "cRandomizerWithCovs.RData")
    cRandomizerWithCov <- cRandomizer

    dfCov      <- NULL

    vISAStatus <- c( 1, 1 )
    expect_error( CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov ) ,
                  "If a randomizer provides dfSubGroupEnrollmentStatus then you must supply dfCov, however, dfCov= NULL ",
                  label="Test when dfCov==NULL" )

    #Error number 2 - randomizer does not have covaraites, covariates provided
    load( "cRandomizerNoCov.RData")
    cRandomizerWithCov <- cRandomizer

    dfCov      <- CreateDataFrameForPatient( 1,1 )

    vISAStatus <- c( 1, 1 )
    expect_error( CheckISAEnrollmentStatus( cRandomizerWithCov, vISAStatus, dfCov ) ,
                  "A patient covariate was supplied, however, one of he randomizers does not provide dfSubGroupEnrollmentStatus.",
                  label="Test when dfCov==NULL" )

})
