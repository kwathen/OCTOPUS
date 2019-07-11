##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#  For a platform trial the randomization is a two step process.  The two steps are as follows:
#       1.  First  randomize between the ISAs.
#               To do this RandomizeBetweenISA is called and the class( cRandomizer ) will determine which version of this function
#               is called.  For example, if class( cRandomzier ) = "Equal" see RandomizeBetweenISA.Equal() for that definition.
#               Each ISA RandomzieBetweenISA function will return an integer nISA to indicate which ISA the patient was randomized to.
#       2.  Randomize within the ISA.
#               To do this RandomizeWithinISA will be called and the class( cRandomzier[[ nISA ]] ) which specific randomizer is called.
#

#' @name Randomize
#' @title Randomize
#' @description { Randomize should return a list with at least the following items
#'  nTrt - The treatment within the ISA that the patient should receive
#'  nISA - The index of the ISA
#'  cISARand - the same is cISARand but if it is updates this allows the caller to get the update.
#'  For a platform trial the randomization is a two step process.  The two steps are as follows:
#'       1.  First  randomize between the ISAs.
#'               To do this RandomizeBetweenISA is called and the class( cRandomizer ) will determine which version of this function
#'               is called.  For example, if class( cRandomzier ) = "Equal" see RandomizeBetweenISA.Equal() for that definition.
#'               Each ISA RandomzieBetweenISA function will return an integer nISA to indicate which ISA the patient was randomized to.
#'       2.  Randomize within the ISA.
#'               To do this RandomizeWithinISA will be called and the class( cRandomzier[[ nISA ]] ) which specific randomizer is called.
#'   }
#' @export
Randomize <- function( cRandomizer, vISAStatus, dCurrentTime, dfCov, nPrintDetail )
{
    UseMethod( "Randomize", cRandomizer )

}


# vISAStatus =  0 is not open yet, 1 is open, 2 is close.  Status = 0,2 --> ISA cannot be randomized to.
#  Any Randomize function should return a list with the following items
#  $cRandomzier - the randomizer with any necessary updates to inform the caller about
#  $nISA - the index of the ISA the patient was assigned to
#  #nTrt - the treatment the patient was assigned to
#' @export
Randomize.default   <- function( cRandomizer, vISAStatus, dCurrentTime, dfCov = NULL, nPrintDetail = 0 )
{
    if( nPrintDetail >= 100 )
        print( "Randomize.default")
    nQtyISA   <- length( cRandomizer )  #This is the number of ISAs

    #The nQtyISA must == length( vISAStatus ) or something is wrong
    if( nQtyISA != length( vISAStatus ) )
        stop( paste( "Critical Error in RandomizeBetweenISA.Equal: The number of ISAs in cRandomizer does not equal the length( vISAStatus) " ))

    # vISAStatus  <- CheckISAEnrollmentStatus( vISAStatus, cRandomizer, dfCov )
    #
    # if( all( vISAStatus != 1 ) )
    # {
    #     # if no element in vISAStatus == 1 --> No ISA is open for the patient with dfCov covariates, return a list with nISA = NA, nTrt = NA
    #     return( list( cRandomizer = cRandomizer, nISA = NA, nTrt = NA ) )
    #
    # }
    nISA        <- RandomizeBetweenISA( cRandomizer, vISAStatus,  dCurrentTime )
    #print( paste("After Randomize between ISA - ISA = ", nISA, " class(cRandomizer) ", class(cRandomizer)))

    cISARand    <- cRandomizer[[ nISA ]]
    lRet        <- RandomizeWithinISA( cISARand,  dCurrentTime )

    #Need to update the cRandoizer$ISARandomizer
    cRandomizer[[ nISA ]] <- lRet$cISARand
    lRet <- list( cRandomizer = cRandomizer, nISA = nISA, nTrt = lRet$nTrt )

    return( lRet )

}


