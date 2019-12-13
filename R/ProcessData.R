##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name ProcessData
#' @title ProcessData
#' @description{ This function is used to process the data before the RunAnalysis function is called.  This can
#' be useful for cases where the data is simulated/collected in one form, but another form is needed for analysis.  For
#' example, the patient data over time is simulated but the data is analysed as change from baseline. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/ProcessData.R}{View Code on GitHub} }
#' @export
ProcessData <- function( cDS, dCurrentTime)
{
    UseMethod( "ProcessData", cDS )
}

#' @name ProcessData.default
#' @title ProcessData.default.
#' @description{ No default version is defined to verify that the user correctly specifies the desired function.
#' This function is used to process the data before the RunAnalysis function is called.  This can
#' be useful for cases where the data is simulated/collected in one for but another form is needed for analysis.  For
#' example, the patient data over time is simulated but the data is analysed as change from baseline. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/ProcessData.R}{View Code on GitHub} }
#' @export
ProcessData.default <- function( cDS, dCurrentTime )
{

    stop("ERROR: The default ProcessData.default is not defined, class(cDS) = ", class(cDS))

    stop("ProcessData.default not defined")
    #print( "ProcessData.default")
}



#' @name ProcessData.ProcessReptMeasChngBaseline
#' @title ProcessData.ProcessReptMeasChngBaseline
#' @description{ This function is used to process the data before the RunAnalysis function is called.  This can
#' be useful for cases where the data is simulated/collected in one form but another form is needed for analysis.  For
#' example, the patient data over time is simulated but the data is analysed as change from baseline.
#' This function will use the vObsTime and vStartTime to remove patients that have not enrolled
#' and set outcomes to NA that have not been observed
#' Will return the change from baseline
#' dCurrentTime, vTrt, mPatOut, lSimOut, vStartTimes }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/ProcessData.R}{View Code on GitHub} }
#' @export
ProcessData.ProcessReptMeasChngBaseline <- function(  cDS, dCurrentTime  )
{
    #print( "ProcessData.ProcessReptMeasChngBaseline")
    #Start with all the simulated data, then make those at times > dCurrentTime = NA

    #print( cDS)
    if( is.null( cDS) | is.null(  cDS$lOut ) | is.null(nrow(cDS$lOut)))
    {
        print( paste( "cDS$lOut was null"))
        return(  structure( list( vOut = c(), vTime = c(), vTrt = c(),  vIND = c(), nQtyPats = 0,
                                  vObsTime = c(), vBaseline= c()), class=class(cDS) ) )
    }

    vObsTime    <- cDS$vObsTime
    mSimDataAna <- matrix( unlist(cDS$lOut), nrow=nrow( cDS$lOut))
    mStartTimes <- matrix( cDS$vStartTimes, ncol = length( vObsTime ), nrow= length(cDS$vStartTimes))
    mObsTime    <- matrix(vObsTime, byrow = TRUE, ncol = length( vObsTime ), nrow= length(cDS$vStartTimes))
    mObsTime    <- mStartTimes + mObsTime

    #If data has not been observed make it NA
    mSimDataAna[ mObsTime > dCurrentTime ] <- NA

    #If the second column is NA we cannot calculate any change from baseline for this patient so drop them off
    vKeep       <- !is.na(mSimDataAna[,2])

    mSimDataAna <- matrix(mSimDataAna[ vKeep, ], ncol=ncol(mSimDataAna))      #If the 2nd column (first obs post baseline) is NA cant use
    if(  nrow(mSimDataAna) > 0 )
    {
        #print( paste( "nrow( mSimDataAna ) ", nrow( mSimDataAna ), " len ", length(cDS$vTrt)))
        vTrt        <- cDS$vTrt[ 1:nrow( mSimDataAna ) ] #vTrt[1:nQtyPats ]
    }
    else
        vTrt <- c()
    #vISA        <- cDS$vISA[ 1:nrow( mSimDataAna ) ] #vTrt[1:nQtyPats ]

    mBaseline   <- matrix( mSimDataAna[,1], nrow= nrow( mSimDataAna), ncol = (length( vObsTime)) )

    #Now we want to subtract the baseline and leave that are NA as NA
    #mSimDataNA <- is.na(  mSimDataAna)
    #mSimDataAna[ is.na(mSimDataAna)] <- 0

    mSimDataAna <- mSimDataAna - mBaseline
    #mSimDataAna[ mSimDataNA ] <- NA

    nQtyPats    <- nrow(mSimDataAna )

    vTmpOut     <- as.vector(mSimDataAna[ ,-1] )
    vBaseline   <- as.vector( mBaseline[,-1])  #We need baseline value as a covariate

    vTime       <- vObsTime[c(-1)]  # - 1 because we don't need the baseline since we are doing change,
    vTime       <- rep( vTime, rep( nQtyPats, rep( length( vTime ) ) ) )

    nRept       <- length( vObsTime ) - 1
    vIND        <- 1:nQtyPats
    vIND        <- rep( vIND, nRept )
    vTrt        <- rep( vTrt, nRept )
    #vISA        <- rep( vISA, nRept )

    #The vKeep in included because it may drop some of the patients, in which case other patient data, like covariates,
    #would need this information to be copied correctly
    return(  structure( list( vOut = vTmpOut, vTime = vTime, vTrt = vTrt,  vIND = vIND, nQtyPats = nQtyPats,
                              vObsTime = vObsTime, vBaseline= vBaseline, vKeep = vKeep), class=class(cDS) ) )

}


#' @name ProcessData.ProcessReptMeas
#' @title ProcessData.ProcessReptMeas
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/ProcessData.R}{View Code on GitHub} }
#' @export
ProcessData.ProcessReptMeas <- function(  cDS, dCurrentTime  )
{

    #print( cDS)
    if( is.null( cDS) | is.null(  cDS$lOut ) | is.null(nrow(cDS$lOut)))
    {
        print( paste( "cDS$lOut was null"))
        return(  structure( list( vOut = c(), vTime = c(), vTrt = c(),  vIND = c(), nQtyPats = 0,
                                  vObsTime = c(), vBaseline= c()), class=class(cDS) ) )
    }

    vObsTime    <- cDS$vObsTime
    mSimDataAna <- matrix( unlist(cDS$lOut), nrow=nrow( cDS$lOut))
    mStartTimes <- matrix( cDS$vStartTimes, ncol = length( vObsTime ), nrow= length(cDS$vStartTimes))
    mObsTime    <- matrix(vObsTime, byrow = TRUE, ncol = length( vObsTime ), nrow= length(cDS$vStartTimes))
    mObsTime    <- mStartTimes + mObsTime

    #If data has not been observed make it NA
    mSimDataAna[ mObsTime > dCurrentTime ] <- NA

    #If the first column is NA we cannot use this patient so drop them off
    vKeep       <- !is.na(mSimDataAna[,1])

    mSimDataAna <- matrix(mSimDataAna[ vKeep, ], ncol=ncol(mSimDataAna))      #If the 2nd column (first obs post baseline) is NA cant use
    if(  nrow(mSimDataAna) > 0 )
    {
        #print( paste( "nrow( mSimDataAna ) ", nrow( mSimDataAna ), " len ", length(cDS$vTrt)))
        vTrt        <- cDS$vTrt[ 1:nrow( mSimDataAna ) ] #vTrt[1:nQtyPats ]
    }
    else
        vTrt <- c()
    #vISA        <- cDS$vISA[ 1:nrow( mSimDataAna ) ] #vTrt[1:nQtyPats ]

    #mSimDataAna[ mSimDataNA ] <- NA

    nQtyPats    <- nrow(mSimDataAna )

    vTmpOut     <- as.vector(mSimDataAna )

    vTime       <- vObsTime
    vTime       <- rep( vTime, rep( nQtyPats, rep( length( vTime ) ) ) )

    nRept       <- length( vObsTime )
    vIND        <- 1:nQtyPats
    vIND        <- rep( vIND, nRept )
    vTrt        <- rep( vTrt, nRept )
    #vISA        <- rep( vISA, nRept )

    #The vKeep in included because it may drop some of the patients, in which case other patient data, like covaraites,
    #would need this information to be copied correctly
    return(  structure( list( vOut = vTmpOut, vTime = vTime, vTrt = vTrt,  vIND = vIND, nQtyPats = nQtyPats,
                              vObsTime = vObsTime, vKeep = vKeep), class=class(cDS) ) )

}



#' @name ProcessData.ProcessSingleTimeOutcome
#' @title ProcessData.ProcessSingleTimeOutcome
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/ProcessData.R}{View Code on GitHub} }
#' @export
ProcessData.ProcessSingleTimeOutcome <- function(  cDS, dCurrentTime  )
{
    #print( "ProcessData.ProcessSingleTimeOutcome")

    #Start with all the simulated data, then make those at times > dCurrentTime = NA
    vObsTime    <- cDS$vObsTime
    #vObsTime    <- lSimOut$vObsTime
    #TODO: Error checking to make sure there is only one time point
    #if( length( vObsTime ) != 1 )
    #    stop()
    vSimDataAna <-unlist(cDS$lOut)
    vStartTimes <-  cDS$vStartTimes

    vObsTime    <- vStartTimes + vObsTime

    #If data has not been observed make it NA
    vSimDataAna[ vObsTime > dCurrentTime ] <- NA

    #vOutObsTime <- cDS$vStartTimes + vObsTime


    #vSimDataAna <-  ifelse( vOutObsTime > dCurrentTime, NA, vSimDataAna)

    vKeep       <- !is.na( vSimDataAna)  #If the 1ar column is NA cant use
    vSimDataAna <- vSimDataAna[ vKeep]
    nQtyPats    <- length(vSimDataAna )
    #vTrt        <- vTrt[ 1:nQtyPats ] #vTrt[1:nQtyPats ]

    vTrt        <- cDS$vTrt[ 1:nQtyPats] #vTrt[1:nQtyPats ]
    #vISA        <- cDS$vISA[ 1:nQtyPats ] #vTrt[1:nQtyPats ]
    vIND        <- 1:nQtyPats

    #The vKeep in included because it may drop some of the patients, in which case other patient data, like covariates,
    #would need this information to be copied correctly
    return(   structure(list( vOut = vSimDataAna ,  vTrt = vTrt,   vIND = vIND, nQtyPats = nQtyPats,
                              vObsTime = vObsTime, vKeep = vKeep), class= class(cDS)) )

}


