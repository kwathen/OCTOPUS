##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

context("Test GetBayesianCutoffs.R")


#############################################################################################################################.
#
# GetBayesianCutoffs.MAVOnly                                                                                            #####
#
#############################################################################################################################.

cAnalysis <- structure( list( vPUpper = c( 0.9,  0.85),
                              vPLower = c( 0.05, 0.1 ),
                              dFinalPUpper = 0.8,
                              dFinalPLower = 0.15), class = "MAVOnly")


test_that("GetBayesianCutoffs.MAVOnly",
          {
              lExp <- list( dPUpperCutoff = 0.9, dPLowerCutoff = 0.05 )
              lRet <- GetBayesianCutoffs( cAnalysis, 1, FALSE )
              expect_equal( lRet, lExp, label = "1st IA" )

              lExp <- list( dPUpperCutoff = 0.85, dPLowerCutoff = 0.1 )
              lRet <- GetBayesianCutoffs( cAnalysis, 2, FALSE )
              expect_equal( lRet, lExp, label = "2nd IA" )

              lExp <- list( dPUpperCutoff = 0.85, dPLowerCutoff = 0.1 )
              lRet <- GetBayesianCutoffs( cAnalysis, 3, FALSE )
              expect_equal( lRet, lExp, label = "Beyond 2nd IA" )

              lExp <- list( dPUpperCutoff = 0.8, dPLowerCutoff = 0.15 )
              lRet <- GetBayesianCutoffs( cAnalysis, 3, TRUE)
              expect_equal( lRet, lExp, label = "FA" )

          })
