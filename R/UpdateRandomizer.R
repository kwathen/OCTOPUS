##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name UpdateRandomizer
#' @title  UpdateRandomizer
#' @description UpdateRandomizer{ }
#' @export
UpdateRandomizer <- function( lDecision, lResAnalysis, cRandomzier  )
{
    UseMethod( "UpdateRandomizer", lDecision )
}

#' @name UpdateRandomizer.default
#' @title UpdateRandomizer.default
#' @description UpdateRandomizer.default { }
#' @export
UpdateRandomizer.default <- function( lDecision, lResAnalysis, cRandomzier  )
{
    cReturnRandomizer <- cRandomzier
    if( length( lResAnalysis ) == 2 )  #There is always a bISAAnalysis run element, hence 2 elements --> 1 analysis
    {
        if( !is.null( lResAnalysis[[ "lAnalysis1" ]]$cRandomizer ) )
            cReturnRandomizer <- lResAnalysis[[ "lAnalysis1" ]]$cRandomizer
    }
    else
    {
        # By default, if there are multiple analysis and a randomizer is provided
        # If any analysis has a group as open, it will be open in the retured randomizer,
        # If all analysis have a group as closed then it will be closed
        nQtyAnalysis <- length( lResAnalysis ) - 1

        dfSubGroupEnrollmentStatus <- lResAnalysis[[ "lAnalysis1" ]]$cRandomizer$dfSubGroupEnrollmentStatus

        if( !is.null( dfSubGroupEnrollmentStatus  ) )  #Using subgroups, need to update
        {
            nStatusCol      <- ncol( dfSubGroupEnrollmentStatus )
            vUpdatedSatus   <- dfSubGroupEnrollmentStatus[, nStatusCol ]

            for( i in 2:nQtyAnalysis )
            {
                dfSubGroupEnrollmentStatus <- lResAnalysis[[ paste("lAnalysis", iAna, sep="")]]$dfSubGroupEnrollmentStatus
                vUpdatedSatus <- (vUpdatedSatus | dfSubGroupEnrollmentStatus[, nStatusCol ])
                #TODO(Covs) - Finish this

            }
            cReturnRandomizer$dfSubGroupEnrollmentStatus[ ,nStatusCol ] <- as.integer( vUpdatedSatus )
        }
    }
    return( cReturnRandomizer )

}


#' @name UpdateRandomizer.Outcome1Only
#' @title UpdateRandomizer.Outcome1Only
#' @description UpdateRandomizer.Outcome1Only { }
#' @export
UpdateRandomizer.Outcome1Only <- function( lDecision, lResAnalysis, cRandomzier  )
{
    cReturnRandomizer <- cRandomzier

    if( !is.null( lResAnalysis[[ "lAnalysis1" ]]$cRandomizer ) )
        cReturnRandomizer <- lResAnalysis[[ "lAnalysis1" ]]$cRandomizer

    return( cReturnRandomizer )

}

#' @name UpdateRandomizer.Outcome1Only
#' @title UpdateRandomizer.Outcome1Only
#' @description UpdateRandomizer.Outcome1Only { }
#' @export
UpdateRandomizer.Outcome2Only <- function( lDecision, lResAnalysis, cRandomzier  )
{
    cReturnRandomizer <- cRandomzier

    if( !is.null( lResAnalysis[[ "lAnalysis2" ]]$cRandomizer ) )
        cReturnRandomizer <- lResAnalysis[[ "lAnalysis2" ]]$cRandomizer

    return( cReturnRandomizer )

}
