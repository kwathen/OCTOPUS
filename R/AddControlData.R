##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name AddControlData
#' @title  AddControlData
#' @description {Used to include control data.}
#' @param  cISAData The ISA object, the class( cISAData ) will determine which version of AddControlData is utilized.
#' @param  cSourceData The object with the patient data to be used as the patient database for pulling control data from.  THis object must have
#' the following elements:
#' 1) cSourceData$vIND
#' 2) cSourceData$vOut
#' 3) cSourceData$vTime
#' 4) cSourceData$vTrt
#' @param  nISA An integer for the ISA number.  As the data is added to the control in may be necessary to know what ISA the data came from.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/AddControlData.R}{View Code on GitHub} }
#' @export
AddControlData <- function( cISAData, cSourceData, nISA )
{
    UseMethod( "AddControlData", cISAData )
}











