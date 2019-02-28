##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name InitializeTrialRandomizer.default
#' @title InitializeTrialRandomizer.default
#' @description {This function is used to initialize the trial randomizer. The default version each
#' ISA and calls the InitializeISARandomizer.  Since each ISA can have a specific randomizer  the default
#' for the TrialRandomizer is just to initialize each ISA randomizer.
#' }
#' @export
InitializeTrialRandomizer  <- function( cTrialDesign )
{
    UseMethod( "InitializeTrialRandomizer", cTrialDesign )
}

#' @export
InitializeTrialRandomizer.default  <- function( cTrialDesign )
{
    nQtyISA   <- cTrialDesign$nQtyISAs

    lISARands <- list()
    for( iISA in 1:nQtyISA )
    {
        strISAName <- paste( "cISA", iISA, sep="" )

        lISARands[[ paste( "lISA", iISA, sep="")]] <- InitializeISARandomizer( cTrialDesign$cISADesigns[[ strISAName ]]  )

    }
    cISARand <- structure( lISARands, class= class( cTrialDesign) )

    return( cISARand )

}
