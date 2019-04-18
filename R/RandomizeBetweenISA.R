##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


# RandomizeBetweenISA should just return the index of the ISA the patient is assigned to
#' @export
RandomizeBetweenISA <- function( cRandomizer, vISAStatus, dCurrentTime )
{
    UseMethod( "RandomizeBetweenISA", cRandomizer )

}
#' @export
RandomizeBetweenISA.default <- function( cRandomizer, vISAStatus, dCurrentTime )
{
    stop(  "RandomizeBetweenISA.default IS NOT IMPLEMENTED.  class( cRandomizer) =", class(cRandomizer))


}

#' @export
RandomizeBetweenISA.EqualRandomizer <- function( cRandomizer, vISAStatus, dCurrentTime )
{
    nQtyISA   <- length( cRandomizer )      # This is the number of ISAs
    vIndex    <- 1:nQtyISA
    vIndex    <- vIndex[ vISAStatus == 1 ]  # We can only randomize to the open ISAs

    #Note - The following if-else structure is required because if only 1 ISA is active and it is not the first ISA
    #vIndex will have length 1 with a value > 1.  This will cause sample( vIndex, 1 ) to sample 1 item from elements 1:vIndex[1]
    #which is not what is desired.
    if( length( vIndex ) > 1 )
        nISA      <- sample( vIndex, 1  )
    else if( length( vIndex ) == 1 )
        nISA      <- vIndex[ 1 ]
    else
        nISA <- NA
    return( nISA )

}
