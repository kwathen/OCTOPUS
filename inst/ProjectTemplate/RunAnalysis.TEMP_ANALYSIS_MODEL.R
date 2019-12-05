##### File Description ######################################################################################################
#   This file is to create the new RunAanlysis that calculates based on a beta-binomial
#   Inputs:
#     cAnalysis - The class( cAnalysis ) determines the specific version of RunAnalysis that is called. It contains
#                 the details about the analysis such as the priors, MAV, TV, decision cut-off boundaries.
#     lDataAna  - The data that is used int he analysis.  Typically contains vISA (the ISA for the patient),
#                 vTrt (treatment for each patient), vOut (the outcome for each patient)
#     nISAAnalysisIndx - index of the analysis used for changing boundaries)
#     bIsFinaISAAnalysis - TRUE or FALSE, often we change the value of the cut-off at the final analysis for an ISA
#     cRandomizer - The randomizer, mainly used for cases with covariates
#
#############################################################################################################################.
RunAnalysis.TEMP_ANALYSIS_MODEL <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    print( paste( "RunAnalysis.TEMP_ANALYSIS_MODEL"))

   # Prior are in cAnalysis$vPriorA and cAnalysis$vPriorB

    vISA <- lDataAna$vISA
    vTrt <- lDataAna$vTrt
    vOut <- lDataAna$vOut

    # Using the is.na because it could be that a patient has not had the outcome observed at the time of the analysis
    vTrt <- vTrt[ !is.na( vOut ) ]
    vOut <- vOut[ !is.na( vOut ) ]

    # Set the Posterior parameters to the prior parameters
    dPostACtrl <- cAnalysis$vPriorA[ 1 ]
    dPostBCtrl <- cAnalysis$vPriorB[ 1 ]
    dPostATrt  <- cAnalysis$vPriorA[ 2 ]
    dPostBTrt  <- cAnalysis$vPriorB[ 2 ]

    #Compute Posterior Parameters - Control treatment
    nNCtrl       <- length( vTrt[ vTrt == 1 ] )           # Number of patient on Control
    nQtyRespCtrl <- sum( vOut[ vTrt == 1] )               # Number of Responses (vOut == 1)
    dPostACtrl   <- dPostACtrl + nQtyRespCtrl             # Posterior A = Prior A + Number of responders
    dPostBCtrl   <- dPostBCtrl + nNCtrl - nQtyRespCtrl    # Posterior B = Prior B + Number of non-responders = Prior B + Number of Patients - # of Responders

    #Compute Posterior Parameters - Treatment
    nNTrt        <- length( vTrt[ vTrt != 1 ] )
    nQtyRespTrt  <- sum( vOut[ vTrt != 1] )
    dPostATrt    <- dPostATrt + nQtyRespTrt
    dPostBTrt    <- dPostBTrt + nNTrt - nQtyRespTrt

    # Want to calculate Pr( Q_T > Q_C + MAV | Data ) - need to compute posterior parameters
    # ProbX1GrX2PlusDelta
    if( is.na( dPostACtrl) | is.na( dPostBCtrl ) | is.na( dPostATrt ) | is.na( dPostBTrt ) )
    {
      # This is for debugging and should not be hit
      browser()
    }
    print( paste( "Number of patients: ", length( vTrt )," Q_C ~ Beta( ",dPostACtrl, dPostBCtrl , "), Q_T ~ Beta( ", dPostATrt, ", ", dPostBTrt, ")"))

    dPrGrtMAV  <- ProbX1GrX2PlusDelta( dPostATrt,  dPostBTrt,
                                       dPostACtrl, dPostBCtrl,
                                       cAnalysis$dMAV )

    lCutoff    <- GetBayesianCutoffs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis )


    lCalcs     <- list( dPrGrtMAV      = dPrGrtMAV,
                        dPUpperCutoff  = lCutoff$dPUpperCutoff,
                        dPLowerCutoff  = lCutoff$dPLowerCutoff )

    lRet       <- MakeDecisionBasedOnPostProb(cAnalysis, lCalcs )

    lRet$cRandomizer <- cRandomizer  # Needed because the main code will pull the randomizer off just in-case this function were to close a covariate group
    return( lRet )


}







