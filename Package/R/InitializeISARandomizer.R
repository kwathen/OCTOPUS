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


# This version will randomize the first patients to a select set of arms or doses in the ISA (eg a POC phase)
# The cISARandomizer must define the following
# The cISARandomizer$mStartTime identifies how to simulate/assign the start time of each of the treatments (doses) in the ISA.
#   Since cISARandomizer$mStartTime is not required by other randomizer so this function will stop if it is not defined.
#   The nrow( cISARandomizer$mStartTime  ) = number of treatments in the ISA.
#   cISARandomizer$mStartTime should have 2 columns, 1 to define the lower limit of the start time and column 2 the potential max
#   such that a time is simulated from a Uniform( cISARandomizer$mStartTime[i, 1], cISARandomizer$mStartTimet[,2])
#   for each treatment.
#  cISARandomizer$mStartTime  is to specify the time >= 0 that a treatment in the ISA is opened.  A value of 0 in column 1 and 2 indicates
#  the treatment is open once the ISA is added to the trial
#' @export
InitializeISARandomizer.DelayedStartRandomizer <- function(  cISARandomizer )
{
    #print( paste( "mStartTime Randomizer"))
    mStartTime <- cISARandomizer$mStartTime
    if( is.null( mStartTime ) )
        stop( paste( "In the function InitializeISARandomizer.DelayedStartRandomizer(  cISA ) the cISA object must define cISA$mStartTime...Stopping execution "))

    vQtyPats     <- cISARandomizer$vQtyPats
    vTrtLab      <- cISARandomizer$vTrtLab

    if( ncol( mStartTime ) != 2 )
        stop( paste( "In the function InitializeISARandomizer.DelayedStartRandomizer(  cISA ) the cISA object must define cISA$mStartTime with 2 columns...Stopping execution "))

    nQtyTreatments <- length(vTrtLab)
    if( nrow( mStartTime ) !=  nQtyTreatments )
        stop( paste( "In the function InitializeISARandomizer.DelayedStartRandomizer(  cISA ) the cISA object must define cISA$mStartTime with a row for each treatment in the ISA...Stopping execution "))


    #Randomzie patients equally - When the RandomizeWithinISA.DelayedStartRandomizer
    # the current time is sent and will compare time vs the start time of each treatment in ISA
    vTrtLab  <- cISARandomizer$vTrtLab
    vQtyPats <- cISARandomizer$vQtyPats
    vTmpTrt  <- rep( vTrtLab, vQtyPats )
    vISARand <- sample( vTmpTrt, length( vTmpTrt ) )

    vTreatmentStart <- runif( rep(1, nQtyTreatments), mStartTime[,1], mStartTime[,2])


    cISARand <- structure( list(vISARand=vISARand, vTreatmentStart = vTreatmentStart, vTrtLab = vTrtLab ), class= class(cISARandomizer))

    return( cISARand )


}
