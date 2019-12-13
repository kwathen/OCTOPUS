##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.
#' @name CheckISAEnrollmentStatus
#' @title  CheckISAEnrollmentStatus
#' @description CheckISAEnrollmentStatus{ }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CheckISAEnrollmentStatus.R}{View Code on GitHub} }
#' @export
CheckISAEnrollmentStatus <- function( cRandomizer, vISAStatus, dfCov    )
{
    UseMethod( "CheckISAEnrollmentStatus", cRandomizer )
}

#' @name CheckISAEnrollmentStatus.default
#' @title CheckISAEnrollmentStatus.default
#' @description CheckISAEnrollmentStatus.default { This function will check the cRandomzier to make sure if an ISA is open for the covariate group dfCov
#' Two cases
#'  If No covaraite - dfCov = NULL then  vEnrollmentStatus = vISAStatus
#'  If Covariates - dfCov != NULL then vEnrollmentStatus = vISAStatus but if an ISA does not enroll for dfCov the value will be 0, indicating it is not open.
#'  Essentially, if an ISA is open for dfCov the value in vRetISAStatus = 1, if not it will be a value != 1}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CheckISAEnrollmentStatus.R}{View Code on GitHub} }
#' @export
CheckISAEnrollmentStatus.default <- function( cRandomizer, vISAStatus, dfCov  )
{

    ValidateEnrollmentStatus( cRandomizer, dfCov )    #This function will stop the program if input is not valid.

    vRetISAStatus <- vISAStatus

    if( !is.null( dfCov ) )  #Only check if a patients covaraites were supplied
    {
        for( iISA in 1:length( vISAStatus ))
        {
            nColStatus <- ncol( cRandomizer[[ iISA ]]$dfSubGroupEnrollmentStatus)
            if( vRetISAStatus[ iISA ] == 1 )
            {
                #If the enrollment status is anything other than 1 patients will not be enrolled to it, thus if it != before no need to check
                vElement              <- SelectList( cRandomizer[[ iISA ]]$dfSubGroupEnrollmentStatus[,-nColStatus], dfCov)
                nISAStatusEnrollment  <- cRandomizer[[ iISA ]]$dfSubGroupEnrollmentStatus[ vElement, nColStatus]
                if( is.na(nISAStatusEnrollment) | is.null( nISAStatusEnrollment) | length(nISAStatusEnrollment ) != 1 )
                {
                    browser()
                }
                vRetISAStatus[ iISA ] <- nISAStatusEnrollment
            }
        }
    }
    return( vRetISAStatus )
}



############################# Helper Functions################################################################################################

ValidateEnrollmentStatus <- function( cRandomizer, dfCov )
{
    lNull <- AreEnrollmentStatusNull( cRandomizer )

    if( is.null( dfCov ) && !lNull$bAllNull )  #One of the randomizer lists enrollment by covariate but the patient's covariates was not supplied --> ERROR
    {
        stop( "If a randomizer provides dfSubGroupEnrollmentStatus then you must supply dfCov, however, dfCov= NULL ")
    }

    if( !is.null( dfCov ) && lNull$bAnyNull )  #One of the randomizer does not list enrollment by covariate but the patient's covariates was supplied --> ERROR
    {
        stop( "A patient covariate was supplied, however, one of he randomizers does not provide dfSubGroupEnrollmentStatus.")
    }

}
GetSubGroupEnrollmentStatus <- function( cRandomizer )
{
    return( cRandomizer$dfSubGroupEnrollmentStatus )
}
AreEnrollmentStatusNull <- function( cRandomizer )
{
    lEnrollmentStatus <- lapply( cRandomizer, FUN = GetSubGroupEnrollmentStatus )
    vIsNull           <- unlist( lapply( lEnrollmentStatus, FUN=is.null) )
    return( list( bAllNull = all( vIsNull ), bAnyNull = any( vIsNull )  ) )
}

