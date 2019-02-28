##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

##### TODO(Kyle) - Create a version with S3 classes rather than S4.

#############################################################################################################################.
#   Description                                                                                                         #####
#   This class can be used to simulate the arrival times of patients in a simulated trial.  The class assumes
#   a Poisson process and allows the user to several options, such as ramp up the accrual rate which typically
#   occurs in clinical trials.
#
#   This is an S4 class with function to allow users to call the code without knowledge of S4.  S4 classes
#   should be avoided if possible.
#
#   Keywords: Patient arrival time, patient accrual
#
#    Example:
#
#    vPatsPerMonth  <- c(5, 10, 15, 20, 30, 40, 50) # The ramp up in expected Pat/month
#    nMaxQtyPats    <- 100  # Maximum of 100 patients
#    ap             <- NewAccrualProcess( vQtyPatsPerMonth = vPatsPerMonth, nMaxQtyPatients = nMaxQtyPats )
#    vAccTimes      <- SimulateArrivalTimes( ap )
#####   Developer(s): J. Kyle Wathen, PhD                                                                               #####
#############################################################################################################################.



if( isClass(Class="AccrualMethods") )
    removeClass("AccrualMethods")
setGeneric( name = "Validate", def=function(obj){standardGeneric("Validate")})


setClass(
    Class = "AccrualMethods",
    representation = representation(
        m.vQtyPatsPerMonth      = "numeric",        #Required - enrollment rate per month.
        #If length=1 constant rate else monthly rate and will only enroll for length( m.vQtyPatPerMonth)-->make sure
        #to provide enough monthly rates to meet max # patients and/or length
        m.nMaxQtyPatients       = "numeric",        #The maximum number of patients to enroll
        m.nMaxMonthsOfAccrual   = "numeric",        #Maximum time to accrue patients, the number of patients is random
        m.strDescription        = "character",      #Used to keep track of options, helps ease printing
        m.bValid                = "logical",        #Is the class valid?
        m.strError              = "character"       #If the class is not valid this is a string with an error code/description
    )

)#End - AccrualMethods


setGeneric( name = "SimulateArrivalTimes",          def=function(cAP){standardGeneric("SimulateArrivalTimes")})

#GetSlot Generics
setGeneric( name = "GetMaxQtyPats",                 def=function(cAP){standardGeneric("GetMaxQtyPats")})
setGeneric( name = "GetMaxQtyMonths",               def=function(cAP){standardGeneric("GetMaxQtyMonths")})
setGeneric( name = "GetDescription",                def=function(cAP){standardGeneric("GetDescription")})

#Set the replace methods for the slots
setGeneric("SetQtyPatsPerMonth<-",function(object,value){standardGeneric("SetQtyPatsPerMonth<-")})

#Validate checks things about the class to make sure it is "valid" and ready to be used
setGeneric( name = "Validate", def=function(obj){standardGeneric("Validate")})


###########################################################################################################.
#   vQtyPatsPerMonth is a vector (possibly of length 1 for constant rate) for the number of patient accrued each month
#
#   For this function you must supply nMaxQtyPatients and/or nMaxMonthsOfAccrual
#   Accrual will continue until either the max qty of patient or max months of accrual is reached,
#   which ever comes first
###########################################################################################################.

#' @name NewAccrualProcess
#' @title NewAccrualProcess
#' @description {This class can be used to simulate the arrival times of patients in a simulated trial.  The function
#' NewAccrualProcess returns an object used to simulate arrival times for patients by calling
#' The class assumes
#'   a Poisson process and allows the user to several options, such as ramp up the accrual rate which typically
#'   occurs in clinical trials.
#'   #'
#'   For this function you must supply nMaxQtyPatients and/or nMaxMonthsOfAccrual
#'   Accrual will continue until either the max qty of patient or max months of accrual is reached,
#'   which ever comes first
#' }
#' @param vQtyPatsPerMonth A vector (possibly of length 1 for constant rate) for the number of patient accrued each month
#' @param nMaxQtyPatients The maximum of of patient recruitment times to simulate.
#' @param nMaxMonthsOfAccrual The maximum number of months that you want to simulate accrual for.
#' @details{ If both nMaxQtyPatients and nMaxMonthsOfAccrual are supplied then the returned vector will be at most
#' nMaxQtyPatients in length.  However, when nMaxMonthsOfAccrual all values will be less than nMaxMonthsOfAccrual so the
#' length of the returned vector could be less than nMaxQtyPatient.  }
#' @examples
#'   vPatsPerMonth  <- c(5, 10, 15, 20, 30, 40, 50) # The ramp up in expected Pat/month
#'   nMaxQtyPats    <- 100  # Maximum of 100 patients
#'   ap             <- NewAccrualProcess( vQtyPatsPerMonth = vPatsPerMonth, nMaxQtyPatients = nMaxQtyPats )
#'   vAccTimes      <- SimulateArrivalTimes( ap )
#' @seealso \code{\link{SimulateArrivalTimes}} for simulating the arrival times once you have created the object..
#' @export
NewAccrualProcess <- function(  vQtyPatsPerMonth, nMaxQtyPatients, nMaxMonthsOfAccrual )
{
    if( missing( nMaxQtyPatients ) & missing( nMaxMonthsOfAccrual ))
    {
        stop( "Error in NewAccrualProcess: You must supply nMaxQtyPatients and/or nMaxMonthsOfAccrual.")
    }
    if( missing( nMaxQtyPatients ) )
    {
        nMaxQtyPatients <- -1
    }
    if( missing( nMaxMonthsOfAccrual ) )
    {
        nMaxMonthsOfAccrual <- -1
    }


    apRet <- new(   Class = "AccrualMethods",
                    vQtyPatsPerMonth = vQtyPatsPerMonth,
                    nMaxQtyPatients  = nMaxQtyPatients,
                    nMaxMonthsOfAccrual = nMaxMonthsOfAccrual )


    return( apRet )

}

setMethod(  f           = "initialize",
            signature( .Object="AccrualMethods" ),
            definition=function( .Object, vQtyPatsPerMonth, nMaxQtyPatients, nMaxMonthsOfAccrual  )
            {
                # First assign the slots
                .Object@m.vQtyPatsPerMonth      <- vQtyPatsPerMonth
                .Object@m.nMaxQtyPatients       <- nMaxQtyPatients
                .Object@m.nMaxMonthsOfAccrual   <- nMaxMonthsOfAccrual

                #Now we need to Validate the object or the validity function does not run!
                Validate(.Object)

                #The following if block is set-up to make sure that if the user sends in a vector for vQtypatsPerMonth
                # that is WAY to long to make sure that we are only generating slightly more than needed.  This was done
                # because if the vector is way too long it could take considerably more time, especially in the case of a ramp-up
                if( nMaxQtyPatients > 0 && length( vQtyPatsPerMonth ) > 1)
                {
                    vTmpCumSum <- cumsum( vQtyPatsPerMonth )
                    vTmpCumSum <- vTmpCumSum[ vTmpCumSum < 2*nMaxQtyPatients ]
                    .Object@m.vQtyPatsPerMonth <- .Object@m.vQtyPatsPerMonth[ 1:length( vTmpCumSum) ]
                }


                if( .Object@m.nMaxQtyPatients > 0 & .Object@m.nMaxMonthsOfAccrual > 0 )
                {
                    strDesc <- "Accrual will continue until either the max qty of patients or max months"
                    strDesc <- paste( strDesc, " of accrual is reached, whichever comes first.")
                    .Object@m.strDescription <- strDesc
                }
                else if( .Object@m.nMaxQtyPatients > 0)
                {
                    strDesc <- "Accrual will continue until the max qty of patients is reached."
                    .Object@m.strDescription <- strDesc
                }
                else if( .Object@m.nMaxMonthsOfAccrual > 0 )
                {
                    strDesc <- "Accrual will continue until  max months of accrual is reached."
                    .Object@m.strDescription <- strDesc
                }

                if( length( vQtyPatsPerMonth ) > 1 )
                {
                    .Object@m.strDescription <- paste( .Object@m.strDescription, " Using a variable accrual rate")
                }
                else
                {
                    .Object@m.strDescription <- paste( .Object@m.strDescription, " Using a constant accrual rate")
                }

                return( .Object )

            }
)


setMethod(  f="Validate",
            signature="AccrualMethods",
            def=function( obj )
            {
                nameObject<-deparse(substitute(obj))

                strErr <- ""
                bRetValid <- FALSE
                if( obj@m.nMaxQtyPatients == -1 & obj@m.nMaxMonthsOfAccrual == -1)
                {
                    strErr      <- "Validation Error: AccrualMethods object is invalid, you must supply nMaxQtyPatients and/or nMaxMonthsOfAccrual.\n"
                    bRetValid   <- FALSE
                }
                else if( any( obj@m.vQtyPatsPerMonth < 0) )
                {
                    strErr      <- paste( strErr, "Error Code 1: An element of vQtyPatsPerMonth is < 0.\n")
                    bRetValid   <- FALSE
                }
                else if( length( obj@m.vQtyPatsPerMonth ) > 1 & obj@m.nMaxMonthsOfAccrual > length( obj@m.vQtyPatsPerMonth ) )
                {
                    strErr      <- paste(strErr, "Error Code 2: Not enough monthly accrual rates were provided to reach the desired max number of months.")
                    bRetValid   <- FALSE
                }
                else
                    bRetValid <- TRUE

                obj@m.strError    <- strErr
                obj@m.bValid      <- bRetValid


                assign(nameObject,obj,envir=parent.frame())
                return(invisible())
            }
)

#' @name SimulateArrivalTimes
#' @title SimulateArrivalTimes
#' @description {This class can be used to simulate the arrival times of patients in a simulated clinical trial.  The function
#' NewAccrualProcess returns an object used to simulate arrival times for patients by calling SimulateArrivalTime.
#' The class assumes
#   a Poisson process and allows the user to several options, such as ramp up the accrual rate which typically
#   occurs in clinical trials.
#'  After calling NewAccrualProces you can call SimulateArrivalTimes to generate a vector of accrual times.
#' }
#' @seealso \code{\link{NewAccrualProcess}} for creating the object.
#' @param vQtyPatsPerMonth A vector (possibly of length 1 for constant rate) for the number of patient accrued each month
#' @param nMaxQtyPatients The maximum of of patient recruitment times to simulate.
#' @param nMaxMonthsOfAccrual The maximum number of months that you want to simulate accrual for.
#' @details{ If both nMaxQtyPatients and nMaxMonthsOfAccrual are supplied then the returned vector will be at most
#' nMaxQtyPatients in length.  However, when nMaxMonthsOfAccrual all values will be less than nMaxMonthsOfAccrual so the
#' length of the returned vector could be less than nMaxQtyPatient.  }
#' @examples
#'   vPatsPerMonth  <- c(5, 10, 15, 20, 30, 40, 50) # The ramp up in expected Pat/month
#'   nMaxQtyPats    <- 100  # Maximum of 100 patients
#'   ap             <- NewAccrualProcess( vQtyPatsPerMonth = vPatsPerMonth, nMaxQtyPatients = nMaxQtyPats )
#'   vAccTimes      <- SimulateArrivalTimes( ap )
#'
#' @export
setMethod(  f = "SimulateArrivalTimes",
            signature( cAP ="AccrualMethods" ),
            definition=function( cAP  )
            {
                if( !cAP@m.bValid )
                {
                    stop( "Error: The AccrualMethods object must be valid before calling SimulateArrivalTimes.")
                }

                #This is a local function because it is only used here
                SimulateAMonthlOfAccrualTimes <- function( dPatsPerMonth , dStartMonth )
                {
                    vTimes <- cumsum(rexp( qpois(0.9999,dPatsPerMonth)+10,dPatsPerMonth))
                    vTimes <- vTimes[ vTimes < 1 ]
                    vTimes <- vTimes + dStartMonth
                    return( vTimes )

                }
                vRetAccTimes    <- vector()
                vPatsPerMonth   <- cAP@m.vQtyPatsPerMonth
                nMaxQtyPats     <- cAP@m.nMaxQtyPatients
                nMaxMonths      <- cAP@m.nMaxMonthsOfAccrual

                if( length(vPatsPerMonth) == 1 )  #Using a constant hazard
                {
                    #Now either we are going to enrol for a maximum number of months, patients or whichever comes first

                    if( nMaxQtyPats > 0 )
                    {
                        #Simulate the arrival times for the max number of patients
                        vRetAccTimes <- cumsum( rexp( nMaxQtyPats, vPatsPerMonth[ 1 ] ) )
                    }
                    else
                    {
                        vRetAccTimes <- cumsum(rexp( qpois(0.9999,vPatsPerMonth[ 1 ])+10,vPatsPerMonth[ 1 ]))
                        vRetAccTimes <- vRetAccTimes[ vRetAccTimes < nMaxMonths  ]

                    }
                }
                else #We have a monthly accrual rate
                {

                    vRetAccTimes <- unlist( mapply( SimulateAMonthlOfAccrualTimes, dPatsPerMonth = vPatsPerMonth, dStartMonth = 0:(length( vPatsPerMonth)-1)) )
                    if( nMaxQtyPats > 0 )
                    {
                        #In previous versions this created a stop, however, this suggested to just send a very long vector
                        # of month recruitments just to make sure this does not happen.  However, this could be VERY slow due to
                        # an excessive length of the vector.  So this was changed to just use the last moth rate for the remainder of the time
                        nQtyTimesSimulated <- length( vRetAccTimes )
                        if( nQtyTimesSimulated < nMaxQtyPats )# & nMaxMonths <= 0 )  #Not enough monthly rates were provided, thus stop because this could indicate an error
                        {
                            nStartMonth <- length( vPatsPerMonth)
                            dLastMonthRate  <- vPatsPerMonth[ length( vPatsPerMonth ) ]
                            while( length( vRetAccTimes ) < nMaxQtyPats )
                            {

                                vRetAccTimes <- c( vRetAccTimes, SimulateAMonthlOfAccrualTimes( dLastMonthRate, nStartMonth ))
                                nStartMonth  <- nStartMonth + 1
                            }
                            vRetAccTimes <- vRetAccTimes[ 1:nMaxQtyPats ]
                        }
                        #else
                        #    vRetAccTimes <- vRetAccTimes[ 1:nMaxQtyPats ]
                    }
                }

                #Make sure to cap the last accrual time if it was provided
                if( nMaxMonths > 0 )
                    vRetAccTimes <- vRetAccTimes[ vRetAccTimes < nMaxMonths  ]
                if( nMaxQtyPats > 0 & length( vRetAccTimes ) > nMaxQtyPats )
                    vRetAccTimes <- vRetAccTimes[ 1:nMaxQtyPats ]

                return( vRetAccTimes )
            }
)

#########################################################################################################.
#    Get the slot variables                                                                         #####
#########################################################################################################.
setMethod(  f           = "GetMaxQtyPats",
            signature( cAP ="AccrualMethods" ),
            definition=function( cAP  )
            {
                return( cAP@m.nMaxQtyPatients)
            }
)

setMethod(  f           = "GetMaxQtyMonths",
            signature( cAP ="AccrualMethods" ),
            definition=function( cAP  )
            {
                return( cAP@m.nMaxMonthsOfAccrual)
            }
)

setMethod(  f           = "GetDescription",
            signature( cAP ="AccrualMethods" ),
            definition=function( cAP  )
            {
                return( cAP@m.strDescription)
            }
)
#########################################################################################################.
# Set the slot variables                                                                            #####
#########################################################################################################.
setReplaceMethod(   f="SetQtyPatsPerMonth",
                    signature="AccrualMethods",
                    definition=function(object,value)
                    {
                        object@m.vQtyPatsPerMonth <- value
                        Validate(object)
                        return (object)
                    }
)



