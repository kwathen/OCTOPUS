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
    strModel  <- "BayesianNormalAR1"
    vPUpper   <- c( 0.99, 1.0 )
    vPLower   <- c( 0.00, 0.0 )
    dFinalPUpper <- 0.80
    dFinalPLower <- 0.1

    vQtyPats     <- c( 30, 60 )  # Control, Treatment
    vMinQtyPats  <- c( 30, 90 )
    vMinFUTime   <- c( 24* dConvWeeksToMonths, 24* dConvWeeksToMonths)
    dQtyMonthsBtwIA  <- 0.0


    ########################################################################
    #  ISA 1 Information
    ########################################################################

    cISA1Info <- Create1DoseISA( vQtyPats        = vQtyPats,
                                 vTrtLab         = c( 1, 2 ),
                                 vPUpper         = vPUpper,
                                 vPLower         = vPLower,
                                 dFinalPUpper    = dFinalPUpper,
                                 dFinalPLower    = dFinalPLower,
                                 strBorrow       = strBorrow,
                                 strModel        = strModel,
                                 vMinQtyPats     = vMinQtyPats,
                                 vMinFUTime      = vMinFUTime,
                                 dQtyMonthsBtwIA = dQtyMonthsBtwIA )


    cTrialDesign <- NewTrialDesign( list( cISA1Info ), strISARandomizer = "EqualRandomizer" )



    return( cTrialDesign )

}