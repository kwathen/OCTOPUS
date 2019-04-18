##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name GetFinalISAAnalysisTime
#' @title GetFinalISAAnalysisTime
#' @description{ Compute the final analysis time for a given ISA based on the current data}
#' @export
GetFinalISAAnalysisTime <- function( cISADesigns, nISA, lEnrolledPats )
{
    UseMethod( "GetFinalISAAnalysisTime", cISADesigns )
}



#' #' export
#' GetFinalISAAnalysisTime.default <- function( cISADesigns, nISA, lEnrolledPats )
#' {
#'     stop( paste("ERROR: The GetFinalISAAnalysisTime.default is not defined class( GetFinalISAAnalysisTime )=", class(cISADesigns) ) )
#' }

#' @export
GetFinalISAAnalysisTime.default <- function(cISADesigns, nISA, lEnrolledPats)
{
    nLen                <- length( cISADesigns[[ nISA ]]$vMinQtyPats )
    dMinFU              <- cISADesigns[[ nISA ]]$vMinFUTime[ nLen ]
    nPat                <- cISADesigns[[ nISA ]]$vMinQtyPats[ nLen ]

    vISAPatStartTimes   <- lEnrolledPats$lPatOut[[nISA]]$vStartTimes
    dPatStartTime       <- vISAPatStartTimes[ nPat ]
    dAnayTime           <- dPatStartTime + dMinFU


    return( dAnayTime )
}


#' #' export
#' GetFinalISAAnalysisTime.IncludeAllControl <- function(cISADesigns, nISA, lEnrolledPats)
#' {
#'     nLen                <- length( cISADesigns[[ nISA ]]$vMinQtyPats )
#'     dMinFU              <- cISADesigns[[ nISA ]]$vMinFUTime[ nLen ]
#'     nPat                <- cISADesigns[[ nISA ]]$vMinQtyPats[ nLen ]
#'
#'     vISAPatStartTimes   <- lEnrolledPats$vStartTimes[ lEnrolledPats$vISA == nISA ]
#'     dPatStartTime       <- vISAPatStartTimes[ nPat ]
#'     dAnayTime           <- dPatStartTime + dMinFU
#'
#'
#'     return( dAnayTime )
#' }
