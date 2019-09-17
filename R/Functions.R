##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#AddPatient( lPatOut, dCurrentTime,   nISA <-cRandUpdate$nISA, nTrt <-cRandUpdate$nTrt, cEnrolledPats <- cEnrolledPats )

#' @name AddPatint
#' @title AddPatient
#' @description {This function uses the lPatOut as a database of patients, it pulls 1 patient out and adds it to lEnrolledPats}
#' @export
AddPatient <- function( lPatOut, dCurrentTime, nISA, nTrt,  dfCov, cEnrolledPats, nPrintDetail = 0 )
{
    if( nPrintDetail >= 15  | gnPrintDetail >= 15)
    {
        strCov <- " No covariates "
        if( !is.null( dfCov ) )
        {
            strCov <- paste( "Pat. Covariates ", paste( dfCov, collapse= ", " ))
        }
        print(paste( "....AddPatient() Time ", round( dCurrentTime,3), strCov, " Patient Randomized to ISA ", nISA, " Treatment ", nTrt, " # In ISA ", cEnrolledPats$vCurrentQtyPatsISA[ nISA ]))
    }
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

    #TODO(Covs) - Select a paient with the covs = dfCov
    lTmpList       <- lPatOut[[ nISA ]][c(names( dfCov), "vPatTrt")]
    lColValues     <- cbind( dfCov, nTrt )
    if( length( lTmpList[[1]]) == 0 )
    {
        return( -1  )
        # No patients on the
    }
    vMatchCriteria <- SelectList( lTmpList, lColValues )
    vIndx          <- vIndx[ vMatchCriteria  ]  #This now contain only patients indexes from the correct ISA, Trt
    if( length( vIndx ) == 0 )
    {
        return( -1  )
    }
    # Note: In case SimulatePatientOutcome created the lPatOut simulated patients in a predictable manner the next
    # block of code will randomly select from the patient that are in the correct ISA/Trt.
    #You cannot simply use sample( vIndx, 1) because if vIndx is of length = 1 and > 1 then sample would select 1 value
    #from 1:vIndx.  Thus, we sample the index of the element we want then select it.

    nLen  <- length( vIndx )
    nTmp  <- sample( 1:nLen, 1 )
    nIndx <- vIndx[ nTmp ] #  Use to be vIndx[1], however if the simulate patient outcome functions creates the list of patient
                                 # based on a particular order, this could cause a pattern in the patients if we just take them in order.
    #print( paste( "nIndex ", nIndx))

    #lPatOut$vPatTrt <- lPatOut[[ nISA ]]$vPatTrt[ -nIndx ]

    i <- 1
    #Note in this for loop we move the row from the lPatOut to the lEnrolledPatOut and use structure(, class=..) to make sure the class of each
    #outcome is maintained, if you did just the rbind() the class attribute is removed
    for( i in 1:lPatOut[[ nISA ]]$nQtyOut )
    {

        strOut    <-  paste("mOut", i, sep="" )
        strSimOut <-  paste("mSimOut", i, sep="" )
        #lPatOutG <<- lPatOut

        #print( lPatOut[[strOut]])
        cEnrolledPats$lPatOut[[nISA]][[ strOut ]]   <- structure(  rbind(cEnrolledPats$lPatOut[[nISA]][[strOut ]], lPatOut[[ nISA ]][[ strSimOut ]][nIndx, ]), class=class(cEnrolledPats$lPatOut[[nISA]][[strOut ]]) )
        lPatOut[[ nISA ]][[ strSimOut ]]            <- structure(lPatOut[[ nISA ]][[ strSimOut ]][-nIndx, , drop=FALSE], class=class( lPatOut[[ nISA ]][[ strSimOut ]]))   # The , drop = false keeps the matrix from being converted to a vector when we drop the second to last row

        strObsTime <- paste( "vObsTime",i,sep="")
        cEnrolledPats$lPatOut[[nISA]][[ strObsTime ]] <- lPatOut[[ nISA ]][[ strObsTime ]]
    }

    #Copy any covariate
    if( is.null( dfCov ) == FALSE  )
    {
        strCov      <- "vCov"
        iCov        <- 1
        strCovName  <- paste( strCov, iCov, sep="" )


        while( strCovName %in% names( lPatOut[[ nISA ]] ) )
        {
            cEnrolledPats$lPatOut[[ nISA ]][[strCovName]] <- c( cEnrolledPats$lPatOut[[ nISA ]][[strCovName]], lPatOut[[ nISA ]][[strCovName ]][nIndx])
            lPatOut[[ nISA ]][[strCovName ]] <- lPatOut[[ nISA ]][[strCovName ]][-nIndx]
            iCov        <- iCov + 1
            strCovName  <- paste( strCov, iCov, sep="" )
        }
    }


    lPatOut[[ nISA ]]$vPatTrt  <- lPatOut[[ nISA ]]$vPatTrt[ -nIndx ]

    return( list( cEnrolledPats = cEnrolledPats, lPatOut = lPatOut ))

    #Now we have the index to the outcomes so we need to copy it to the cEnrolledPats and remove it from the lPatOut
}

#' @name AppendPatientLists
#' @title AppendPatientLists
#' @description {This function takes two lists of simulated outcome (eg from SimulateAllPatientOutcomes) and appends such that the structure of the returned list is
#' the same as the source lists.  For elements that are a matrix it will utilize rbind, for vectors c() is used.
#' The main purpose of this function is during simulaitons if the original SimulateAllPatients did not generate enought patients}
#' @param lList1 - The first elment list where lList2 is appended to.
#' @param lList2 - The second list
#' @param vNamesToExclude a vector of the names to include in the appending.  Names not included are coppied from lList1 with nothing appended.
#' @export
AppendPatientLists <- function( lList1, lList2, vNamesToExclude = NULL )
{
    append <- function( l1, l2)
    {

        if( is.matrix( l1 ) )
        {
            #Want to use rbind
            lOut <- rbind( l1, l2)
        }
        else
        {
            lOut <- c( l1, l2 )
        }
        return( lOut )
    }

    for( i in 1:length( lList1 ) )
    {
        #Want to exclude the vObsTime variable from being appended to for a outcomes (eg vObsTime1, vObsTime2,...)
        vNamesExclude <- c( paste("vObsTime", 1:lList1[[ i ]]$nQtyOut, sep=""), "nQtyOut", vNamesToExclude)
        vNames        <- names( lList1[[1]] )
        vNames        <- vNames[ !(vNames %in% vNamesExclude)]


        l1 = lList1[[ i ]]
        l2 = lList2[[ i ]]
        for( strName in vNames )
        {
            l1[[ strName ]] <- append(l1[[ strName ]], l2[[ strName ]])
        }
        lList1[[ i ]] <- l1


    }
    return( lList1 )

}

#' @name SelectList
#' @title SelectList
#' @description {Determine if the elements of lData have the values of lValue, this is like SQL select statement with AND for all values}
#' @param lData - The dataset (as a list) that you want to select from
#' @param lValue - A list with the desired values for each column in lData
#' @export
SelectList <- function( lData, lValue )
{
    if( length(lValue) == 1)
    {
        vResults <-  ( lData[[1]]==rep( lValue, length((lData[[1]]))) )
        return( vResults )
    }
    subsetColumn <- function( l1, value )
    {
        return( l1 == value )
    }
    mColumnResults <- mapply( FUN = subsetColumn, lData, lValue )   #This will create a matrix with a row for each row in lData and a column ofr each column with TRUE/FALSE if it equals the desired value
    vResults       <- apply( mColumnResults, 1, all )
    return( vResults )
}



#' @name CheckGlobalVariables
#' @title CheckGlobalVariables
#' @description {Determine if the global variables that are needed have been defined and if not define them. Use of global variables
#' should be restricted to the variables that are used throughout the code and only in VERY limited use.}
#' @param lData - The dataset (as a list) that you want to select from
#' @param lValue - A list with the desired values for each column in lData
#' @export
CheckGlobalVariables <- function()
{
    if( exists( "gDebug" ) == FALSE   )
    {
        gDebug <<- FALSE
    }
    if( exists( "gnPrintDetail") == FALSE )
    {
        gnPrintDetail <<- 0
    }
}

