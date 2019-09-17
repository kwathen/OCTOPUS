##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


RunAnalysis.TestWithCovariates <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer  )
{
    #The purpose of this RunAnalyis is to close subgroups of an arm so a test can be created to make sure patients
    #are with specific covarate vectors are not randomized.
    dfTmpSubGroupEnrollmentStatus <- cRandomizer$dfSubGroupEnrollmentStatus

    #The next line will make it to where no patients with Cov1 = 1 are enrolled to the ISA
    dfTmpSubGroupEnrollmentStatus[c(1,3,5),3] <-0

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    vUniqT      <- sort(vUniqT[ vUniqT != 1 ])

    lRetAll <- vector("list", length( vUniqT))   #The list that will be retured with a list for each dose
    nIndx   <- 1
    nQtyTrt <- length( vUniqT)
    for( i in 1:nQtyTrt )
    {

        lCalcs <- list( dPrGrtTV       = 0.5,
                        dPrGrtMAV      = 0.5,
                        dPLowerCutoff  = 0.1,
                        dPUpperCutoff  = 0.9
                        )

        lRet <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )

        lRetAll[[ i ]] <- lRet
    }
    if( nQtyTrt == 1 )
    {
        lRetAll <- lRetAll[[1]]
    }

    cRandomizer$dfSubGroupEnrollmentStatus <- dfTmpSubGroupEnrollmentStatus

    lRetAll$cRandomizer <- cRandomizer
    return( lRetAll )


}




