##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @export
MakeDecisionDoses<- function( lDoseDec )
{
    UseMethod( "MakeDecisionDoses", lDoseDec )
}

#' @export
MakeDecision.default<- function( lDecision, lResAnalysis, bFinalAnalysis )
{
    if( length( lResAnalysis ) == 2 )  #There is just one outcome so return it.  Checking if 2 beause the length( lResAnalysis) = # outcomes + 1 (for binary if analysis is run)
        return( list( nGo = lResAnalysis[[1]]$nGo, nNoGo = lResAnalysis[[1]]$nNoGo, nPause = lResAnalysis[[1]]$nPause)  )
    else
    {
        stop(paste( "ERROR: The default MakeDecision is not defined, class(lDecision) = ", class( lDecision)))
    }

}


#' @export
MakeDecisionDoses.default <- function( lDoseDec )
{

    stop("ERROR: The default MakeDecisionDoses.default is not defined, class(lDecision) = ", class(lDoseDec))

}

#' @name MakeDecisionDoses.HighDoseOnly
#' @title MakeDecisionDoses.HighDoseOnly
#' @description {"This option allows for multiple doses but only uses the high dose for decision making "  }
#' @export
MakeDecisionDoses.HighDoseOnly <- function( lDoseDec )
{

    nQtyDose <- length( lDoseDec )
    lRet     <- lDoseDec[[nQtyDose]]

    return( lRet )
}


#' @name MakeDecisionDoses.AtLeastOne
#' @title MakeDecisionDoses.AtLeastOne
#' @description {This option allows for mulitiple doses and if all doses reach a No Go decision
#' then the combined decision is a No Go.  If ANY dose is a Go then a Go decision is made.     }
#' @export
MakeDecisionDoses.AtLeastOne <- function( lDoseDec )
{

    #print( "MakeDecisionDoses.AtLeastOne" )
    lRet     <- list( nGo= 0, nNoGo = 0, nPause = 1)
    nQtyDose <- length( lDoseDec )
    nQtyNoGo <- 0
    nQtyGo   <- 0
    for( i in 1:nQtyDose )
    {
        if( is.na(  lDoseDec[[i]]$nGo ) |  is.na( lDoseDec[[i]]$nNoGo)  )
        {
            strErr <- paste( "Error in MakeDecisionDoses.AtLeastOne ( MakeDecisionsDoses.R ): For dose, ", i, " an NA was suppled for $nGo or $nNoGo. ")
            strErr <- paste( strErr, "  All doses you must define $nGo and $nNoGo as 0 or 1")
            stop( strErr )
        }
        else
        {
            nQtyGo   <- nQtyGo   + lDoseDec[[i]]$nGo
            nQtyNoGo <- nQtyNoGo + lDoseDec[[i]]$nNoGo
        }

    }
    if( nQtyNoGo == nQtyDose )
        lRet <- list( nGo= 0, nNoGo = 1, nPause = 0)
    else if( nQtyGo >= 1 )
        lRet <- list( nGo= 1, nNoGo = 0, nPause = 0)
    return( lRet )
}




#' @export
MakeDecisionDoses.MakeDecisionMCPMod <- function( lDoseDec )
{
    lRet     <- list( nGo= 0, nNoGo = 0, nPause = 1)
    nQtyDose <- length( lDoseDec )
    nQtyNoGo <- 0
    nQtyGo   <- 0
    for( i in 1:nQtyDose )
    {
        nQtyGo   <- nQtyGo   + lDoseDec[[i]]$nGo
        nQtyNoGo <- nQtyNoGo + lDoseDec[[i]]$nNoGo


    }
    if( nQtyNoGo == nQtyDose )
        lRet <- list( nGo= 0, nNoGo = 1, nPause = 0)
    else if( nQtyGo >= 2 )
        lRet <- list( nGo= 1, nNoGo = 0, nPause = 0)
    return( lRet )
}
