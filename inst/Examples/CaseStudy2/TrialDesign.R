##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

SetupTrialDesign <- function()
{
    dConvWeeksToMonths <- 12/52


    strBorrow    <- "AllControls"
    strModel     <- "BayesianNormalAR1"
    vPUpper      <- c( 0.99, 1.0 )
    vPLower      <- c( 0.00, 0.0 )
    dFinalPUpper <- 0.80
    dFinalPLower <- 0.1

    vTrtLab      <- c( 1, 2 )    # 1 = treagtment or control, then labels for each treatment
    vQtyPats     <- c( 30, 60 )  # Number of patients on Control (30) andTreatment (60)

    # How do we want to monitor the ISA and check decision rule
    # The monitoring plan is based on a number of patients with a minimum of follow-up (FU).
    # In this example, monitoring starts at 90 patients with 24 weeks of FU which is the
    # maximum number of patients and FU so there is no interim analyisis (IA).
    # For more information see ?CheckTrialMonitor

    vMinQtyPats      <- c( 90, 90 )
    vMinFUTime       <- c( 24* dConvWeeksToMonths, 24* dConvWeeksToMonths)
    dQtyMonthsBtwIA  <- 0.0



    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.

    cISA1Info <- Create1DoseISA( vQtyPats        = vQtyPats,
                                 vTrtLab         = vTrtLab,
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
