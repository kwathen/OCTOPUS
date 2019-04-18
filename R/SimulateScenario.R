##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @export
SimulateScenario <- function( cScen, cTrialDesign  )
{
    UseMethod( "SimulateScenario", cScen )

}

#' @export
SimulateScenario.default <- function( cScen, cTrialDesign  )
{

    vRes            <- vector() #vector( length=(20 + length(vPatsPerMonthPerSite1 )))

    i               <- 1
    nQtyReps        <- cScen$nQtyReps

    cScen$nTrialID  <- cScen$nTrialIDStart
    repeat
    {
        print( paste( "Rep ", i))
        rRes <- SimulateSingleTrial( cScen, cTrialDesign  )

        vRes <- rbind( vRes,c( unlist( rRes ) ))
        if( i == nQtyReps)
            break
        i <- i + 1
        cScen$nTrialID <- cScen$nTrialID + 1
    }
    return( vRes )

}