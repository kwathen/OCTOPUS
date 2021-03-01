##### File Description ######################################################################################################
#   This file provides the function SetupTrialDesign to help create the trial design object for the OCTOPUS package
#   It is intended as a starting point to help the user develop and modify as needed.
#
#   Input:
#       strAnalysisModel     - The analysis to be used in the simulation for analyzing data.
#       strBorrowing         - A string variable to indicate of control patients should be shared across ISAs.
#                              Options are "NoBorrowing" or "AllControls"
#        mPatientsPerArm     - A matrix of samples sizes. Each row represents an ISA, column 1 is control column 2,.. are treatments
#                              This functions assumes the same number of treatments in each ISA, to change this
#                              the mPatientsPerArm could be replaced with something that allows for a vector for each ISA where the
#                              first element is the number of patient on control.
#
#
#############################################################################################################################.

SetupTrialDesign <- function( strAnalysisModel, strBorrowing, mPatientsPerArm, dQtyMonthsFU,
                              mMinQtyPats = NULL, vMinFUTime = NULL, dQtyMonthsBtwIA = 0,
                              vPUpper = NULL, vPLower = NULL, dFinalPUpper = 1.0, dFinalPLower = 0.0 )
{
    dConvWeeksToMonths <- 12/52

    # Options for borrowing "NoBorrowing" or "AllControls"
    strBorrow         <- strBorrowing
    strModel          <- strAnalysisModel
    bIncreaseParam    <- TRUE
    dMAV              <- 0.1

    # By default this functions sets up a trial with only a Final Analysis(FA).
    # However, in the OCTOPUS package interim analysis (IAs) are specified in one of two ways:
    # 1. Number of patients with given follow-up (any number of IAs can be set this way)
    # 2. The first IA is conducted when a specified number of patients have a specified follow-up with
    #    additional IAs conducted based on a time frequency, eg every 2 months.
    # see help for CheckTrialMonitor for more detail

    #Default - Only a FA is utilized NO Interim Analysis (IAs) are utilized
    if( is.null( mMinQtyPats ) | is.null( vMinFUTime ) )
    {
        mMinQtyPats       <- apply( mPatientsPerArm, 1, sum )
        vMinFUTime        <- c( dQtyMonthsFU )
        dQtyMonthsBtwIA   <- 0
        #The boundaries of the decision - If a vector is provided for vUpper and vLower it must be either length 1 with No IAs,
        # 2 if you have 2 IAs or you use the option to specify the IAs as a specified time period or equal to the length of vMintQtyPats
        vPUpper           <- c( 1.0 )
        vPLower           <- c( 0.0 )
        dFinalPUpper      <- 0.8
        dFinalPLower      <- 0.1
    }

    #Interim Analysis - Example 1 - To include an IA the following code could be utilized where the first IA is conducted when half the patients of an ISA are enrolled with
    # dQtyMonthsFU months of follow-up,  To add additional
    # mMinQtyPats      <- cbind( floor(apply( mQtyPats , 1, sum )/2),  apply( mQtyPats , 1, sum ) )
    # vMinFUTime       <- rep( dQtyMonthsFU, ncol( mMinQtyPats) )
    # dQtyMonthsBtwIA  <- 0

    # vPUpper           <- c( 0.99,0.99 )
    # vPLower           <- c( 0.01, 0.01 )
    # dFinalPUpper      <- 0.8
    # dFinalPLower      <- 0.1

    #Interim Analysis - Example 2 - The following code could be utilized to have the first IA is conducted when half the patients of an ISA are enrolled with
    # dQtyMonthsFU months of follow-up, then perform an IA every month thereafter
    # mMinQtyPats       <- cbind( floor(apply( mQtyPats , 1, sum )/2),  apply( mQtyPats , 1, sum ) )
    # vMinFUTime        <- rep( dQtyMonthsFU, 2)
    # dQtyMonthsBtwIA   <- 1

    # vPUpper           <- c( 0.99,0.99 )
    # vPLower           <- c( 0.01, 0.01 )
    # dFinalPUpper      <- 0.8
    # dFinalPLower      <- 0.1


    # It is often necessary to include when patients will have outcomes observed, especially in cases like a repeated measure.
    # This next variable is included as an example but is not required for an analysis.
    vObsTimeInMonths  <- c( 6 )  # patients have outcome observed at 6 months, change as needed to match the project.

    # Important Note ####
    #   The vObsTimeInMonths is used to describe when outcomes are observed, however, if dQtyMonthsFU < vObsTimeInMonths
    #   then you will have the last patients enrolled followed less than the time required to observe the outcome
    #   so they would could be contributing nothing to the analysis.
    #

    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.

    # Prior parameters for Control, Treatment.  For this example, the priors are added to the analysis object list
    # and is intended to show an example of how additional parameters are included if needed in the analysis function.
    vPriorA   <- c( 0.2, 0.2 )
    vPriorB   <- c( 0.8, 0.8 )

    # Create ISAs
    nQtyISAs      <- nrow( mPatientsPerArm )
    lISAs         <- list()
    nCurrentTrtID <- 2

    for( iISA in 1:nQtyISAs)
    {

        #Depending on the options for IA need to set vMinQtyPats differently
        if( is.matrix( mMinQtyPats ) )
        {
            #Case when IAs are included
            vMinQtyPats <- mMinQtyPats[ iISA, ]
        }
        else
        {
            vMinQtyPats <- mMinQtyPats[ iISA ]
        }

        #TODO(Kyle) - Generalize vTrtLab to use the number of columns in the matrix in-case the user has more than treatment and control in trial

        vTrtLab  <- c( 1,  nCurrentTrtID )
        cISAInfo <- CreateISA(  vQtyPats         = mPatientsPerArm[ iISA, ],
                                vTrtLab          = vTrtLab,
                                vObsTimeInMonths = vObsTimeInMonths,
                                dMAV             = dMAV,
                                vPUpper          = vPUpper,
                                vPLower          = vPLower,
                                dFinalPUpper     = dFinalPUpper,
                                dFinalPLower     = dFinalPLower,
                                bIncreaseParam   = bIncreaseParam,
                                strBorrow        = strBorrow,
                                strModel         = strModel,
                                vMinQtyPats      = vMinQtyPats,
                                vMinFUTime       = vMinFUTime,
                                dQtyMonthsBtwIA  = dQtyMonthsBtwIA,
                                vPriorA          = vPriorA,
                                vPriorB          = vPriorB )

        nCurrentTrtID <- nCurrentTrtID + 1

        lISAs[[ paste( "cISA", iISA, "Info", sep="" ) ]] <- cISAInfo
    }




    # THe new trial design will use the EqualRandomizer to determine how patients are randomized amoung concurent ISAs.
    # This means that if 2 or more ISAs are open at the same time then there will be an equal chance of the patient being
    # randomized to each ISA.  WIthing the ISA the patients are randomized according to the vQtyPats above
    cTrialDesign <- NewTrialDesign( lISAs , strISARandomizer = "EqualRandomizer" )



    return( cTrialDesign )
}
