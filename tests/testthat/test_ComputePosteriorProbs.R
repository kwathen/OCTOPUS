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
# ComputePosteriorProbs.MAVOnly                                                                                         #####
#
#############################################################################################################################.

dMAV <- 1.0
cAnalysis <- structure( list(      vPUpper = c( 0.9,  0.85),
                                   vPLower = c( 0.05, 0.1 ),
                              dFinalPUpper = 0.8,
                              dFinalPLower = 0.15,
                                      dMAV = dMAV,
                             bPlacMinusTrt = FALSE), class = "MAVOnly")

SetTrtVect <- function( vSampPlac, dTargetRate )
{
    vSampTrt <- vSampPlac
    nReq <- dTargetRate * length( vSampPlac )
    if( nReq >= 1 )
        vSampTrt[ 1:nReq ] <- vSampTrt[ 1:nReq ] + 2
    else
        vSampTrt <- vSampPlac
    return( vSampTrt )
}

#This should make the Post Prob = 0.95
vPostSampPlac <- rep( 1, 20 )
vPostSampTrt  <- SetTrtVect( vPostSampPlac, 0.95)

lSamples <- list( vPostSampPlac = vPostSampPlac,
                  vPostSampTrt =  vPostSampTrt  )



test_that("ComputePosteriorProbs.MAVOnly",
          {
              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.95)  #Should result in a Go
              lExp <- list( nGo = 1, nNoGo = 0, nPause = 0, dPrGrtMAV = 0.95 )
              lRet <- ComputePosteriorProbs( cAnalysis, 1, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "1st IA - Go" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.9)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.9 )
              lRet <- ComputePosteriorProbs( cAnalysis, 1, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "1st IA - Pause at PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.85)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.85 )
              lRet <- ComputePosteriorProbs( cAnalysis, 1, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "1st IA - Below PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.05)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.05 )
              lRet <- ComputePosteriorProbs( cAnalysis, 1, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "1st IA - Pause at PL" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.0)  #Should result in a Stop
              lExp <- list( nGo = 0, nNoGo = 1, nPause = 0, dPrGrtMAV = 0.0 )
              lRet <- ComputePosteriorProbs( cAnalysis, 1, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "1st IA - No Go" )

              #############################################################################################
              ##### Moving to the 2nd Analysis
              #############################################################################################


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.9)  #Should result in a Go
              lExp <- list( nGo = 1, nNoGo = 0, nPause = 0, dPrGrtMAV = 0.9 )
              lRet <- ComputePosteriorProbs( cAnalysis, 2, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "2nd IA - Go" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.85)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.85 )
              lRet <- ComputePosteriorProbs( cAnalysis, 2, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "2nd IA - Pause at PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.8)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.8 )
              lRet <- ComputePosteriorProbs( cAnalysis, 2, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "2nd IA - Below PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.1)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.1 )
              lRet <- ComputePosteriorProbs( cAnalysis, 2, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "2nd IA - Pause at PL" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.05)  #Should result in a Stop
              lExp <- list( nGo = 0, nNoGo = 1, nPause = 0, dPrGrtMAV = 0.05 )
              lRet <- ComputePosteriorProbs( cAnalysis, 2, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "2nd IA - No Go" )


              #############################################################################################
              ##### Moving to the 3rd Analysis
              #############################################################################################


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.9)  #Should result in a Go
              lExp <- list( nGo = 1, nNoGo = 0, nPause = 0, dPrGrtMAV = 0.9 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "3rd IA - Go" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.85)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.85 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "3rd IA - Pause at PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.8)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.8 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "3rd IA - Below PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.1)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.1 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "3rd IA - Pause at PL" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.05)  #Should result in a Stop
              lExp <- list( nGo = 0, nNoGo = 1, nPause = 0, dPrGrtMAV = 0.05 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, FALSE, lSamples )
              expect_equal( lRet, lExp, label = "3rd IA - No Go" )


              #############################################################################################
              ##### Moving to the FA Analysis
              #############################################################################################


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.85)  #Should result in a Go
              lExp <- list( nGo = 1, nNoGo = 0, nPause = 0, dPrGrtMAV = 0.85 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, TRUE, lSamples )
              expect_equal( lRet, lExp, label = "FA IA - Go" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.8)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.8 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, TRUE, lSamples )
              expect_equal( lRet, lExp, label = "FA IA - Pause at PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.5)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.5 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, TRUE, lSamples )
              expect_equal( lRet, lExp, label = "FA IA - Below PU" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.15)  #Should result in a pause
              lExp <- list( nGo = 0, nNoGo = 0, nPause = 1, dPrGrtMAV = 0.15 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, TRUE, lSamples )
              expect_equal( lRet, lExp, label = "FA IA - Pause at PL" )


              lSamples$vPostSampTrt  <- SetTrtVect( lSamples$vPostSampPlac, 0.10)  #Should result in a Stop
              lExp <- list( nGo = 0, nNoGo = 1, nPause = 0, dPrGrtMAV = 0.1 )
              lRet <- ComputePosteriorProbs( cAnalysis, 3, TRUE, lSamples )
              expect_equal( lRet, lExp, label = "FA IA - No Go" )
          }
)

