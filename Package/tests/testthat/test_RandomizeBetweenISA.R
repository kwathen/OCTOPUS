##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


context("Test RandomizeBetweenISA.R")
source("TestHelperFunctions.R")


#Each test will created an lExp that gives the expected outcome of each test.


cTrialDesign <- SetupTrialDesign2ISA( )
cTrialRand   <- InitializeTrialRandomizer( cTrialDesign )

#----------------------------------------------------------------------------------------------
#
# Test  - The trial randomizer
#
#----------------------------------------------------------------------------------------------

test_that("Equal Randomizer - Class Names",
          {
              strExp <- "EqualRandomizer"
              expect_equal( class(cTrialDesign), strExp )
              expect_equal( class( cTrialRand), strExp )
          })

test_that("Equal Randomizer - Random tests",
          {

              #Now perform a few random tests to make sure the randomization to the ISAs are equal
              vISAStatus <- c( 1, 1 )
              nQtyISAs   <- length( cTrialRand )
              expect_equal( cTrialDesign$nQtyISAs, nQtyISAs)

              #When you call Randomize the function should return a list with an the following elements
              lRandRet <- Randomize(cTrialRand, vISAStatus  )
              bExp     <- FALSE
              bRet     <- is.null(lRandRet$nTrt )
              expect_equal( bRet, bExp )

              bRet     <- is.null(lRandRet$nISA )
              expect_equal( bRet, bExp )

              bRet     <- is.null(lRandRet$cRandomizer )
              expect_equal( bRet, bExp )

              bExp      <- TRUE
              bPassTest <- TRUE
              vQtyISA   <- c( 0, 0 )
              strErr    <- ""

              for( i in 1:200 )
              {
                  lRandRet   <- Randomize( cTrialRand, vISAStatus )
                  vQtyISA[ lRandRet$nISA ] <- vQtyISA[ lRandRet$nISA ] + 1

                  expect_lte( lRandRet$nISA, nQtyISAs )

              }
              #The 99% of a binomial distribution with prob = 0.5 with 200 observations is 116, so if if either count is greater than 116 there could be a problem
              bCountTest <- all( vQtyISA <= 116 )
              if( bCountTest == FALSE )   #Potential problem rerun the test
              {
                  cTrialDesign <- SetupTrialDesign2ISA( )
                  cTrialRand   <- InitializeTrialRandomizer( cTrialDesign )
                  vQtyISA   <- c( 0, 0 )

                  for( i in 1:200 )
                  {
                      lRandRet   <- Randomize( cTrialRand, vISAStatus )
                      vQtyISA[ lRandRet$nISA ] <- vQtyISA[ lRandRet$nISA ] + 1

                  }
                  # The 99% of a binomial distribution with prob = 0.5 with 200 observations is 116, so if if either count is greater than 116 there could be a problem
                  bCountTest <- all( vQtyISA <= 116 )

              }

              expect_true( bCountTest )

          })

