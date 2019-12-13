##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name SimulateScenario
#' @title SimulateScenario
#' @description { SimulateScenario This function is the main funciton in simulating a scenario.
#' The scenario will be simulated cScen$nQtyReps times.  }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateScenario.R}{View Code on GitHub} }
#' @export
SimulateScenario <- function( cScen, cTrialDesign  )
{
    UseMethod( "SimulateScenario", cScen )

}

#' @title SimulateScenario.default
#' @describeIn SimulateScenario  Default method that is suitable for most cases.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimulateScenario.R}{View Code on GitHub} }
#' @export
SimulateScenario.default <- function( cScen, cTrialDesign  )
{

    vRes            <- vector() #vector( length=(20 + length(vPatsPerMonthPerSite1 )))

    i               <- 1
    nQtyReps        <- cScen$nQtyReps

    cScen$nTrialID  <- cScen$nTrialIDStart
    repeat
    {
        #print( paste( "Rep ", i))
        rRes <- SimulateSingleTrial( cScen, cTrialDesign  )

        vRes <- rbind( vRes,c( unlist( rRes ) ))
        if( i == nQtyReps)
            break
        i <- i + 1
        cScen$nTrialID <- cScen$nTrialID + 1
    }
    return( vRes )

}
