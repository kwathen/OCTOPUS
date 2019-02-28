##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


# RandomizeWithInISA should return a list with at least the following items
#  nTrt - The treatment within the ISA that the patient should receive
#  cISARand - the same is cISARand but if it is updates this allows the caller to get the update.
#' @export
RandomizeWithinISA <- function( cISARand )
{
    UseMethod( "RandomizeWithinISA", cISARand )

}

#Do not define a default RandomizeWithinISA as we want it to be defined in the trial setup and get to .default by accident.
#' @export
RandomizeWithinISA.default <- function( cISARand )
{
    stop(  "RandomizeWithinISA.default IS NOT IMPLEMENTED.  class( cISARand ) =", class(cISARand ))


}

#   This function contains code that is common for randomizer that create a list with InitializeISARandomizer
#' @export
ReturnNextTreatment <- function( cISARand )
{

    nTrt            <- cISARand[[1]][ 1 ]
    cISARand[[ 1 ]] <- cISARand[[ 1 ]][-1]

    lRet <- list( nTrt = nTrt, cISARand = cISARand )
    return( lRet )
}

#' @export
RandomizeWithinISA.EqualRandomizer <- function( cISARand )
{
    return( ReturnNextTreatment( cISARand ) )


}

#' @export
RandomizeWithinISA.POCRandomizer <- function( cISARand )
{
    return( ReturnNextTreatment( cISARand ) )


}
