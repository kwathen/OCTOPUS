##### File Description ######################################################################################################
#   This file will setup a trial design that will have the following charteristics
#   3 ISAs - each with control (C) and treatment (T)
#   Binary outcome with probability of response Q_C and Q_T for control and treatment respectively
#   Beta-Binomial analysis model
#   Priors are Q_C ~ Beta( vPriorA[1], vPriorB[2] ) and Q_T ~ Beta( vPriorA[2], vPriorB[2])
#
#############################################################################################################################.

SetupTrialDesign <- function( strAnalysisModel, strBorrowing )
{
    dConvWeeksToMonths <- 12/52
    
    # Options for borrowing "NoBorrowing" or "AllControls"
    strBorrow         <- strBorrowing
    strModel          <- strAnalysisModel
    bIncreaseParam    <- TRUE
    dMAV              <- 0.1
    vPUpper           <- c( 1.0, 1.0 )
    vPLower           <- c( 0.0, 0.0 )
    dFinalPUpper      <- 0.8
    dFinalPLower      <- 0.1
    
    vQtyPats          <- c( 50, 50 )  # Control, Treatment
    vMinQtyPats       <- c( 25, 100 )
    vObsTimeInMonths  <- c( 6 )  # patients have outcome observed at 6 months
    vMinFUTime        <- c( 6, 6)
    dQtyMonthsBtwIA   <- 1
    
    
    ########################################################################.
    #  ISA 1 Information                                                ####
    ########################################################################.
    
    # Prior parameters for Control, Treatment
    vPriorA   <- c( 0.2, 0.2 )
    vPriorB   <- c( 0.8, 0.8 )
    
    cISA1Info <- CreateISA( vQtyPats         = vQtyPats,
                            vTrtLab          = c( 1, 2 ),
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
    
    
    ########################################################################.
    #  ISA 2 Information                                                ####
    ########################################################################.
    
    # Control, Treatment - For ISA 2 we want more patient on Treatment since we can borrow
    vQtyPats     <- c( 25, 75 )  
    
    # Prior parameters of the Beta( A, B) for Control { Beta( vPriorA[1], vPriorB[1]) } and  
    # Treatment { Beta( vPriorA[ 2 ], vPriorB[ 2 ] ) ]
    vPriorA   <- c( 0.2, 0.2 )
    vPriorB   <- c( 0.8, 0.8 )
    
    cISA2Info <-CreateISA( vQtyPats         = vQtyPats,
                           vTrtLab          = c( 1, 3 ),
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
                           vPriorB          = vPriorB  )
    
    
    ########################################################################.
    #  ISA 3 Information                                                ####
    ########################################################################.
    
    # Control, Treatment - For ISA 2 we want more patient on Treatment since we can borrow
    vQtyPats     <- c( 25, 75 )  
    
    # Prior parameters for Control, Treatment
    vPriorA   <- c( 0.2, 0.2 )
    vPriorB   <- c( 0.8, 0.8 )
    
    cISA3Info <-CreateISA( vQtyPats         = vQtyPats,
                           vTrtLab          = c( 1, 4 ),
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
                           vPriorB          = vPriorB  )
    
    
    
    
    # THe new trial design will use the EqualRandomizer to determine how patients are randomized amoung concurent ISAs.
    # This means that if 2 or more ISAs are open at the same time then there will be an equal chance of the patient being 
    # randomized to each ISA.  WIthing the ISA the patients are randomized according to the vQtyPats above
    cTrialDesign <- NewTrialDesign( list( cISA1Info, cISA2Info, cISA3Info ) , strISARandomizer = "EqualRandomizer" )
    
    
    
    return( cTrialDesign )
    
}
