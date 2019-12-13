##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

# See also InitilalizeISARandomizer.R


#' @title  RandomizeWithinISA
#' @description { This function is used in close connection with InitializeISARandomizer and
#' if a new version of this fuction is developed a new version InitializeISARandomizer with the same
#' class is required.  RandomizeWithInISA should return a list with at least the following items
#'  nTrt - The treatment within the ISA that the patient should receive
#'  cISARand - the same is cISARand but if it is updates this allows the caller to get the update. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
#' @export
RandomizeWithinISA <- function( cISARand, dCurrentTime )
{
    UseMethod( "RandomizeWithinISA", cISARand )

}

#' @title ReturnNextTreatment
#' @description { This function uses the first element of cISARand and the first item in the vector
#' and as the assigned treatment.  The cISARand in the return will have this element removed. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
#' @export
ReturnNextTreatment <- function( cISARand )
{

    nTrt            <- cISARand[[1]][ 1 ]
    cISARand[[ 1 ]] <- cISARand[[ 1 ]][-1]

    lRet <- list( nTrt = nTrt, cISARand = cISARand )
    return( lRet )
}



#' @title  RandomizeWithinISA.defaut
#' @describeIn RandomizeWithinISA { Because several options are provided and there in no well defined default
#' an stop error occurs if you call the default method. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
#' @export
RandomizeWithinISA.default <- function( cISARand, dCurrentTime )
{
    stop(  "RandomizeWithinISA.default IS NOT IMPLEMENTED.  class( cISARand ) =", class(cISARand ))
}

#' @title RandomizeWithinISA.EqualRandomizer
#' @describeIn RandomizeWithinISA { Patients are randomized equally/fairly amount the treatments in
#' the ISA. }
#' @seealso InitializeISARandomizer.EqualRandomizer
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
#' @export
RandomizeWithinISA.EqualRandomizer <- function( cISARand, dCurrentTime )
{
    return( ReturnNextTreatment( cISARand ) )


}

#' @title RandomizeWithinISA.POCRandomizer
#' @describeIn RandomizeWithinISA { This randomzier will randomize the first patients to a select
#' set of arms or doses in the ISA (eg a POC phase).  The remaining arms or doses are opened based on the number of intitial patients. }
#' @seealso InitializeISARandomizer.POCRandomizer
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
#' @export
RandomizeWithinISA.POCRandomizer <- function( cISARand, dCurrentTime )
{
    return( ReturnNextTreatment( cISARand ) )


}

#' @title RandomizeWithinISA.DelayedStartRandomizer
#' @describeIn RandomizeWithinISA { This version will randomize the first patients to a select set of arms or doses in the ISA (eg a POC phase)
#'  The remaining arms or doses are opened based on the provided times to open. }
#' @seealso InitializeISARandomizer.DelayedStartRandomizer
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RandomizeWithinISA.R}{View Code on GitHub} }
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
