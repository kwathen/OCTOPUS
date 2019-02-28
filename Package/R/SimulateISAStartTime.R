##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#This file contains the generic methods that could be used to set the start time of any ISA, by default the time is specified.

#' @name  SimulateAllISAStartTimes
#' @title  Simulate ISA Start Times for all ISAs
#' @export
SimulateAllISAStartTimes <- function( cISASimDesigns )
{
    UseMethod( "SimulateAllISAStartTimes", cISASimDesigns )

}

#' @name  SimulateAllISAStartTimes
#' @title  Simulate ISA Start Times for all ISAs
#' @export
SimulateAllISAStartTimes.default <- function( cISASimDesigns)
{
    nQtyISA       <- length( cISASimDesigns )
    vISAStartTime <- rep( 0, nQtyISA )

    for( i in 1:nQtyISA )
    {
        vISAStartTime[ i ] <- SimulateISAStartTime( cISASimDesigns[[ i ]]$cSimISAStart  )
    }
    return( vISAStartTime )
}


#' @name  SimulateISAStartTime
#' @title  Simulate ISA Start Time
#' @export
SimulateISAStartTime <- function( cParams )
{
    UseMethod( "SimulateISAStartTime", cParams )

}

#' @name  SimulateISAStartTime.default
#' @title  Simulate ISA Start Time
#' @description This version should not be implemented
#' @export
SimulateISAStartTime.default <- function( cParams )
{
    stop( "SimulateISAStartTime.default is not defined. ")
}


#' @name  SimulateISAStartTime.SetTime
#' @title  SimulateISAStartTime SetTime
#' @description This version assumes the cParams has a dParam1 which specifies the start time for the ISA, eg it is fixed when the ISA starts
#' @export
SimulateISAStartTime.SetTime <- function( cParams )
{
    #print( "SimulateISAStartTime.SetTime")

    return( cParams$dParam1 )

}


#' @name  SimulateISAStartTime.Uniform
#' @title  Simulate ISA Start Time from a uniform distribution
#' @description This version assumes the cParams has a dParam1 and dParam2 and simulates a unif( dParam1, dParam2 ) as the ISA start time
#' @export
SimulateISAStartTime.Uniform <- function( cParams )
{
    #print( " SimulateISAStartTime.Uniform")
    return( runif( 1, cParams$dParam1, cParams$dParam2 ))

}
