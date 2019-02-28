##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.



#' @export
InitializeISARandomizer <- function( cISARandomizer )
{
    UseMethod( "InitializeISARandomizer",  cISARandomizer )

}

#' @export
InitializeISARandomizer.default <- function( cISARandomizer )
{
    stop( paste( "InitializeISARandomizer.default is not defined, stopping execution..."))

}


#' @export
InitializeISARandomizer.EqualRandomizer <- function( cISARandomizer )
{
    vTrtLab  <- cISARandomizer$vTrtLab
    vQtyPats <- cISARandomizer$vQtyPats
    vTmpTrt  <- rep( vTrtLab, vQtyPats )
    vISARand <- sample( vTmpTrt, length( vTmpTrt ) )

    cISARand <- structure( list(vISARand), class= class(cISARandomizer))
    return( cISARand )


}


# This version will randomize the first patients to a select set of arms or doses in the ISA (eg a POC phase)
# The cISARandomizer must define the following
# The cISARandomizer$vQtyPatsInit identifies the initial patients to randomize first, the vQtyPatsInit is part of the vQtyPats for that ISA
#   and is not required by other randomizer so this function will stop if it is not defined
#' @export
InitializeISARandomizer.POCRandomizer <- function(  cISARandomizer )
{
    #print( paste( "POC Randomizer"))
    vQtyPatsInit <- cISARandomizer$vQtyPatsInit
    if( is.null( vQtyPatsInit ) )
        stop( paste( "In the function InitializeISARandomizer.POCRandomizer(  cISA ) the cISA object must define cISA$vQtyPatsInit...Stopping execution "))

    vQtyPats     <- cISARandomizer$vQtyPats
    vTrtLab      <- cISARandomizer$vTrtLab

    vQtyPats2    <- vQtyPats - vQtyPatsInit

    vTmpTrt      <- rep( vTrtLab, vQtyPats2  )
    vTmpTrtInit  <- rep( vTrtLab, vQtyPatsInit )

    vISARand     <- sample( vTmpTrtInit, length( vTmpTrtInit ) )   #Randomize the initial set of patients
    vISARand     <- c( vISARand, sample( vTmpTrt, length( vTmpTrt ) ) ) # Randomize the remainder of the patients


    cISARand <- structure( list(vISARand), class= class(cISARandomizer))

    return( cISARand )


}
