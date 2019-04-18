##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

# See also InitilalizeISARandomizer.R

# RandomizeWithInISA should return a list with at least the following items
#  nTrt - The treatment within the ISA that the patient should receive
#  cISARand - the same is cISARand but if it is updates this allows the caller to get the update.
#' @export
RandomizeWithinISA <- function( cISARand, dCurrentTime )
{
    UseMethod( "RandomizeWithinISA", cISARand )

}

#Do not define a default RandomizeWithinISA as we want it to be defined in the trial setup and get to .default by accident.
#' @export
RandomizeWithinISA.default <- function( cISARand, dCurrentTime )
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
RandomizeWithinISA.EqualRandomizer <- function( cISARand, dCurrentTime )
{
    return( ReturnNextTreatment( cISARand ) )


}

#' @export
RandomizeWithinISA.POCRandomizer <- function( cISARand, dCurrentTime )
{
    return( ReturnNextTreatment( cISARand ) )


}

#' @export
RandomizeWithinISA.DelayedStartRandomizer <- function( cISARand, dCurrentTime )
{

    nTrt          <- cISARand[[1]][ 1 ]            # The next treatment

    #Need to make sure the treatment is open based on start time.
    vTrtLab       <- cISARand$vTrtLab
    vStartTime    <- cISARand$vTreatmentStart
    dTrtStartTime <- vStartTime[ vTrtLab == nTrt ]
    #browser()
    if( dCurrentTime >= dTrtStartTime )  # The treatment has been started, remove it from the list
    {
        cISARand[[ 1 ]] <- cISARand[[ 1 ]][-1]
        lRet <- list( nTrt = nTrt, cISARand = cISARand )
    }
    else
    {

        # Sample the index of the remaining assignments to move cISARand[[1]][1] to then move it
        vRand  <- cISARand[[ 1 ]]

        vUnopenTrts <- vTrtLab[ vStartTime > dCurrentTime ]
        if( all( vRand %in% vUnopenTrts ) )  #Should this occure then the treatment is not open and all remaining randomizations are equal to an unoppen treatment
        {

            stop( "Error in RandomizeWithinISA.DelayedStartRandomizer - All remainng randomizations are to an unopen treatment.")
        }
        while( dCurrentTime < dTrtStartTime )  #Treatment has not been opened
        {
            #print( paste( "Patient was randomzied to unopen arm, moving randomization beteen 2 and ",length( cISARand[[ 1 ]]),
             #               " Currrent time ", dCurrentTime, " Trt Start Time ", dTrtStartTime) )

            nIndex <- sample( 2:length( vRand ), 1 )

            vRand  <- append( vRand, vRand[ 1 ], nIndex)
            vRand  <- vRand[ -1 ]
            nTrt   <- vRand[ 1 ]
            dTrtStartTime <- vStartTime[ vTrtLab == nTrt ]

        }
        #nTrt will be assinged and we need to drop the first element of vRand before assigning it to update the randomzier
        cISARand[[ 1 ]] <- vRand[ -1 ]
        lRet <- list( nTrt = nTrt, cISARand = cISARand )

    }
    return( lRet )


}
