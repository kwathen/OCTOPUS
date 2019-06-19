##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#AddPatient( lPatOut, dCurrentTime,   nISA <-cRandUpdate$nISA, nTrt <-cRandUpdate$nTrt, cEnrolledPats <- cEnrolledPats )

#' @name AddPatint
#' @title AddPatient
#' @description {This function uses the lPatOut as a database of patients, it pulls 1 patient out and adds it to lEnrolledPats}
#' @export
AddPatient <- function( lPatOut, dCurrentTime, nISA, nTrt,  cEnrolledPats, nPrintDetail = 0 )
{
    if( nPrintDetail >= 15)
        print(paste( "....AddPatient Time ", round( dCurrentTime,3), " Patient Randomized to ISA ", nISA, " Treatment ", nTrt, " # In ISA ", cEnrolledPats$vCurrentQtyPatsISA[ nISA ]))

    cEnrolledPats$vCurrentQtyPatsISA[ nISA ] <- cEnrolledPats$vCurrentQtyPatsISA[ nISA ] + 1
    #lEnrolledPats$vISA <- c( lEnrolledPats$vISA, nISA )
    #lEnrolledPats$vTrt <- c( lEnrolledPats$vTrt, nTrt )


    cEnrolledPats$vQtyPatsArmISA[ cEnrolledPats$vTrtLab == nTrt  & cEnrolledPats$vISALab == nISA ] <-
        cEnrolledPats$vQtyPatsArmISA[ cEnrolledPats$vTrtLab == nTrt  & cEnrolledPats$vISALab == nISA ] + 1

    #Copy the treatment and start times to the correct ISA
    cEnrolledPats$lPatOut[[nISA]]$vStartTimes <- c(cEnrolledPats$lPatOut[[nISA]]$vStartTimes, dCurrentTime )
    cEnrolledPats$lPatOut[[nISA]]$vTrt <- c(cEnrolledPats$lPatOut[[nISA]]$vTrt, nTrt )

    #Now we need to get the patient outcomes from lPatOut and remove that data from lPatOut
    nRemainOut <- length( lPatOut[[ nISA ]]$vPatTrt )
    vIndx <- 1:nRemainOut
    vIndx <- vIndx[ lPatOut[[ nISA ]]$vPatTrt== nTrt  ]  #This now contain only patients indexes from the correct ISA, Trt

    # Note: In case SimulatePatientOutcome created the lPatOut simulated patients in a predictable manner the next
    # block of code will randomly select from the patient that are int he correct ISA/Trt.
    #You cannot simply use sample( vIndx, 1) because if vIndx is of length = 1 and > 1 then sample would select 1 value
    #from 1:vIndx.  Thus, we sample the index of the element we want then select it.

    nLen  <- length( vIndx )
    nTmp  <- sample( 1:nLen, 1 )
    nIndx <-  vIndx[ nTmp ] #  Use to be vIndx[1], however if the simulate patient outcome functions creates the list of patient
                                 # based on a particular order, this could cause a pattern in the patients if we just take them in order.
    #print( paste( "nIndex ", nIndx))

    lPatOut$vPatTrt <- lPatOut[[ nISA ]]$vPatTrt[ -nIndx ]

    i <- 1
    #Note in this for loop we move the row from the lPatOut to the lEnrolledPatOut and use structure(, class=..) to make sure the class of each
    #outcome is maintained, if you did just the rbind() the class attribute is removed
    for( i in 1:lPatOut[[ nISA ]]$nQtyOut )
    {
        strOut    <-  paste("mOut",i,sep="" )
        strSimOut <-  paste("mSimOut",i,sep="" )
        #lPatOutG <<- lPatOut

        #print( lPatOut[[strOut]])
        cEnrolledPats$lPatOut[[nISA]][[ strOut ]] <- structure(  rbind(cEnrolledPats$lPatOut[[nISA]][[strOut ]], lPatOut[[ nISA ]][[ strSimOut ]][nIndx, ]), class=class(cEnrolledPats$lPatOut[[nISA]][[strOut ]]) )
        lPatOut[[ nISA ]][[ strSimOut ]]            <- structure(lPatOut[[ nISA ]][[ strSimOut ]][-nIndx, , drop=FALSE], class=class( lPatOut[[ nISA ]][[ strSimOut ]]))   # The , drop = false keeps the matrix from being converted to a vector when we drop the second to last row

        strObsTime <- paste( "vObsTime",i,sep="")
        cEnrolledPats$lPatOut[[nISA]][[ strObsTime ]] <- lPatOut[[ nISA ]][[ strObsTime ]]
    }
    #Copy any covariate
    strCov      <- "vCov"
    iCov        <- 1
    strCovName  <- paste( strCov, iCov, sep="" )

    #TODO: Check if this has a bug the cEnrolledPats$lPatOut[[1]][[strCovName]] looks like it should be
    # cEnrolledPats$lPatOut[[nISA]][[strCovName]]
    while( strCovName %in% names( lPatOut[[ nISA ]] ) )
    {
        cEnrolledPats$lPatOut[[1]][[strCovName]] <- c( cEnrolledPats$lPatOut[[1]][[strCovName]], lPatOut[[1]][[strCovName ]][nIndx])
        lPatOut[[1]][[strCovName ]] <- lPatOut[[1]][[strCovName ]][-nIndx]
        iCov        <- iCov + 1
        strCovName  <- paste( strCov, iCov, sep="" )
    }


    lPatOut[[ nISA ]]$vPatTrt  <- lPatOut[[ nISA ]]$vPatTrt[ -nIndx ]

    return( list( cEnrolledPats = cEnrolledPats, lPatOut = lPatOut ))

    #Now we have the index to the outcomes so we need to copy it to the cEnrolledPats and remove it from the lPatOut
}



