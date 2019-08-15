##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


SetupTrialDesign <- function()
{
    dConvWeeksToMonths <- 12/52


    strBorrow <- "AllControls"
    strModel  <- "TestWithCovariates"
    vPUpper   <- c( 1.0, 1.0 )
    vPLower   <- c( 0.0, 0.0 )
    dFinalPUpper <- 0.45
    dFinalPLower <- 0.05

    vQtyPats     <- c( 30, 60 )  # Control, Treatment

    ## Set the Interim Analysis.   Start conducting IAs when 30 patients have 24 weeks of FU, monthly thereafter with FA when 90 patients have 24 weeks FU
    vMinQtyPats  <- c( 15, 90 )
    vMinFUTime   <- c( 0, 24* dConvWeeksToMonths)
    dQtyMonthsBtwIA  <- 1.0

    #TODO(Covs) - Is this sufficient?  When a general verison of SetupTrialDesign is developed this need to be an argument, and if not present don't add
    cCovariates  <- SetupCovariates( c( 2, 3 )  )

    dfSubgroupEnrollmentStatus <- SetupSubgroupEnrollmentStatus( cCovariates )

    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.

    cISA1Info <- Create1DosePh2AISA( vQtyPats     = vQtyPats,
                                     vTrtLab      = c( 1, 2 ),
                                     vPUpper      = vPUpper,
                                     vPLower      = vPLower,
                                     dFinalPUpper = dFinalPUpper,
                                     dFinalPLower = dFinalPLower,
                                     strBorrow    = strBorrow,
                                     strModel     = strModel,
                                     vMinQtyPats  = vMinQtyPats,
                                     vMinFUTime   = vMinFUTime,
                                     dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                     cCovariates     = cCovariates,
                                     dfSubgroupEnrollmentStatus = dfSubgroupEnrollmentStatus)
    cISA1Info <<- cISA1Info # make a global version to view


    ###############################################################################################################.
    #  ISA 2 Information - - Note the vTrtLab has 1 (control) and 3,4 for treatment 1 is ALWAYS control                                                ####
    ###############################################################################################################.
    dfSubgroupEnrollmentStatus[ c(2,4,6), 3 ] <- 0  #For ISA2 only allow patients with Cov1 = 1

    cISA2Info <- Create2DosePh2AISA( vQtyPats     = c(30, 20, 40 ),
                                     vTrtLab      = c( 1, 3, 4 ),
                                     vPUpper      = vPUpper,
                                     vPLower      = vPLower,
                                     dFinalPUpper = dFinalPUpper,
                                     dFinalPLower = dFinalPLower,
                                     strBorrow    = strBorrow,
                                     strModel     = strModel,
                                     vMinQtyPats  = vMinQtyPats,
                                     vMinFUTime   = vMinFUTime,
                                     dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                     cCovariates     = cCovariates,
                                     dfSubgroupEnrollmentStatus = dfSubgroupEnrollmentStatus
                                     )




    cTrialDesign <- NewTrialDesign( list( cISA1Info, cISA2Info ), strISARandomizer = "EqualRandomizer" )



    return( cTrialDesign )

}

SetupCovariates <- function( vLevels )
{
    nQtyCovs       = length( vLevels )

    lCovs = list( )
    i = 1
    for( i in 1:nQtyCovs )
    {
        lCovs[[ paste("Cov", i, sep="" ) ]] = c(1:vLevels[ i ])
    }
    cCovariates = structure( lCovs, class="default")
    return( cCovariates )
}

#This function creates a list of of the possible covaraites subgroups and set the enrollment status for each,
# 0 --> no open for enrollment, 1-->open for enrollment
 SetupSubgroupEnrollmentStatus <- function( cCovariates )
 {
     dfCovCombinations          <- data.frame(cbind(expand.grid( cCovariates ), 1) )
     names( dfCovCombinations ) <- c( names( cCovariates ), "EnrollmentStatus" )
     return( dfCovCombinations )

 }








