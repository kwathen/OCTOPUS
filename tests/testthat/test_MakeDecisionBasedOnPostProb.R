##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


context("Test MakeDecisionBasedOnPostProb.R")


###############################################################################
#
# MakeDecisionBasedOnPostProb.MAVOnly
#
###############################################################################

cAnalysis <- structure( list(  ), class = "MAVOnly")
cCalcs <- list(     dPrGrtMAV = 0.01,
                    dPUpperCutoff = 0.9,
                    dPLowerCutoff = 0.1 )


test_that("MakeDecisionBasedOnPostProb.MAVOnly",
          {
              lExp <- list( nGo = 0, nNoGo = 1, nPause = 0, dPrGrtMAV = 0.01 )
              lRet <- MakeDecisionBasedOnPostProb( cAnalysis, cCalcs )
              expect_equal( lRet, lExp, label = "Expected No Go" )

              cCalcs$dPrGrtMAV <- 0.1
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.1 )
              lRet <- MakeDecisionBasedOnPostProb( cAnalysis, cCalcs )
              expect_equal( lRet, lExp, label ="Expected Pause - dPrGrtMAV = dPLowerCutoff" )

              cCalcs$dPrGrtMAV <- 0.9
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.9 )
              lRet <- MakeDecisionBasedOnPostProb( cAnalysis, cCalcs )
              expect_equal( lRet, lExp, label = "Expected Pause - dPrGrtMAV = dPUpperCutoff" )

              cCalcs$dPrGrtMAV <- 0.7
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.7 )
              lRet <- MakeDecisionBasedOnPostProb( cAnalysis, cCalcs )
              expect_equal( lRet, lExp, label = "Expected Pause - dPLowerCutof < dPrGrtMAV < dPUpperCutoff" )

              cCalcs$dPrGrtMAV <- 0.95
              lExp <- list( nGo = 1, nNoGo = 0, nPause = 0, dPrGrtMAV = 0.95 )
              lRet <- MakeDecisionBasedOnPostProb( cAnalysis, cCalcs )
              expect_equal( lRet, lExp, label = "Expected Go" )
          })
