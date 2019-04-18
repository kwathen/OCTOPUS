##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @export
CreateDataSetForAnalysis <- function( cPats,  dCurrentTime, vISAStatus  )
{
    UseMethod( "CreateDataSetForAnalysis", cPats )

}

#cPats <- cEnrolledPats, dCurrentTime

#' @export
CreateDataSetForAnalysis.default   <- function( cPats,  dCurrentTime, vISAStatus   )  #IndependentISA <- function( cPats,  dCurrentTime   )
{
    #print( "CreateDataSetForAnalysis.IndependentISA ")
    lRetData        <- structure( list(), class="TrialData")


    nQtyISA <- length( cPats$lPatOut )
    iISA    <- 1
    repeat
    {

        nQtyOutcomes    <- cPats$lPatOut[[iISA]]$nQtyOut
        iOut            <- 1
        lISA            <- structure( list(), class="TrialData")


        if( vISAStatus[ iISA ] != 0 )  #Only created a data set for the ISa that have been opened
        {
            repeat
            {


                strOut       <- paste( "mOut", iOut,sep="")
                cDS <- structure( list( lOut=NULL,
                                        vTrt        = cPats$lPatOut[[iISA]]$vTrt,
                                        vStartTimes = cPats$lPatOut[[iISA]]$vStartTimes),
                                  class= class(cPats$lPatOut[[iISA]][[strOut]] ))

                cDS$lOut     <- cPats$lPatOut[[iISA]][[strOut]]

                strObsTime   <- paste("vObsTime", iOut, sep="")
                cDS$vObsTime <- cPats$lPatOut[[iISA]][[ strObsTime ]]


                lData        <- ProcessData( cDS, dCurrentTime )
                lData        <-CopyCovariates( lData, cPats$lPatOut[[iISA]] )
                lISA[[paste("lDataOut", iOut, sep="")]] <- lData
                if( iOut == nQtyOutcomes )
                    break
                iOut <- iOut+1

            }
        }

        lRetData[[ iISA ]] <- lISA
        if( iISA == nQtyISA )
            break
        iISA <- iISA + 1
    }

    return( lRetData )
}

