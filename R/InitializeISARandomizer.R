##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name InitializeISARandomizer
#' @title InitializeISARandomizer
#' @description {This function is used to initialize the randomzier for an ISA. }
#' @export
InitializeISARandomizer <- function( cISARandomizer, dISAStartTime  )
{
    UseMethod( "InitializeISARandomizer",  cISARandomizer )

}

#' @name InitializeISARandomizer.default
#' @title InitializeISARandomizer.default
#' @description {This function is used to initialize the randomzier for an ISA. No default option
#' is specified to make sure the user selects a valid randomizer. }
#' @export
InitializeISARandomizer.default <- function( cISARandomizer, dISAStartTime )
{
    stop( paste( "InitializeISARandomizer.default is not defined, stopping execution..."))

}

#' @name InitializeISARandomizer.EqualRandomizer
#' @title InitializeISARandomizer.EqualRandomizer
#' @description {This function is used to initialize the EqualRandomzier for an ISA. It uses the vTrtLab and vQtyPats
#' to sample the desired number of each treatment in the ISA. }
#' @param  cISARandomizer Must contain elements cISARandomizer$vTrtLab and cISARandomizer$vQtyPats and the lengths must
#' be equal.
#' @param dISAStartTime Parameter is not utilized for EqualRandomizer
#' @export
InitializeISARandomizer.EqualRandomizer <- function( cISARandomizer, dISAStartTime )
{
    vTrtLab  <- cISARandomizer$vTrtLab
    vQtyPats <- cISARandomizer$vQtyPats
    vTmpTrt  <- rep( vTrtLab, vQtyPats )
    vISARand <- sample( vTmpTrt, length( vTmpTrt ) )

    cISARand <- structure( list( vISARand = vISARand,
                                 dfSubGroupEnrollmentStatus = cISARandomizer$dfSubgroupEnrollmentStatus), class= class(cISARandomizer))
    return( cISARand )


}



#' @name InitializeISARandomizer.POCRandomizer
#' @title InitializeISARandomizer.POCRandomizer
#' @description { InitializeISARandomizer.POCRandomizer will randomize the first patients to a select set of
#' arms or doses in the ISA (eg a POC phase).  The remaining arms or doses are opened based on the number of intitial patients.}
#' @param  cISARandomizer The cISARandomizer$vQtyPatsInit identifies the initial patients to randomize first, the vQtyPatsInit is part of the vQtyPats for that ISA
#'   and is not required by other randomizer so this function will stop if it is not defined
#' @param dISAStartTime Parameter is not utilized for POCRandomizer
#' @export
InitializeISARandomizer.POCRandomizer <- function(  cISARandomizer, dISAStartTime )
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


    cISARand <- structure( list(vISARand = vISARand,
                                dfSubGroupEnrollmentStatus = cISARandomizer$dfSubgroupEnrollmentStatus), class= class(cISARandomizer))

    return( cISARand )


}


#' @name InitializeISARandomizer.DelayedStartRandomizer
#' @title InitializeISARandomizer.DelayedStartRandomizer
#' @description {This version will randomize the first patients to a select set of arms or doses in the ISA (eg a POC phase)
#'  The remaining arms or doses are opened based on the provided times to open.}
#' @param  cISARandomizer cISARandomizer$mTreatmentStartTimes identifies how to simulate/assign the start time of each of the treatments (doses) in the ISA.
#'   Since cISARandomizer$mTreatmentStartTimes is not required by other randomizer so this function will stop if it is not defined.
#'   The nrow( cISARandomizer$mTreatmentStartTimes  ) = number of treatments in the ISA.
#'   cISARandomizer$mTreatmentStartTime should have 2 columns, 1 to define the lower limit of the start time and column 2 the potential max
#'   such that a time is simulated from a Uniform( cISARandomizer$mTreatmentStartTimes[i, 1], cISARandomizer$mTreatmentStartTimes[,2])
#'   for each treatment.
#'  cISARandomizer$mTreatmentStartTimes  is to specify the time >= 0 that a treatment in the ISA is opened.  A value of 0 in column 1 and 2 indicates
#'  the treatment is open once the ISA is added to the trial.
#' @param dISAStartTime The time the ISA is added to the platform.  All arms are opened at time >= dISAStartTime
#' @export
InitializeISARandomizer.DelayedStartRandomizer <- function(  cISARandomizer, dISAStartTime )
{
    #print( paste( "mStartTime Randomizer"))
    mStartTime <- cISARandomizer$mTreatmentStartTimes
    if( is.na( dISAStartTime ) | is.null( dISAStartTime ) )
        stop( paste( "In call to InitializeISARandomizer.DelayedStartRandomizer you must provide cISARandomizer and dISAStartTime."))

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
    #These start times are relative to the ISA entering the study so need to add the ISA start time
    vTreatmentStart <- vTreatmentStart + dISAStartTime

    cISARand <- structure( list(vISARand=vISARand, vTreatmentStart = vTreatmentStart, vTrtLab = vTrtLab ), class= class(cISARandomizer))

    return( cISARand )


}
