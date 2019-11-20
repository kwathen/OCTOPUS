
################################################################################################################## #
# Create an ISA structure
#   Arguments:
#       vQtyPats:  The number of patients on Control, Treatment 1, ... Treatment X in in this ISA
#                   length( vQtyPats) >= 2; typically there is control and 1 treatment in an ISA --> length(vQtyPats)==2
#       vTrtLab:   Control ALWAYS has a label 1, then labels for other treatments in this ISA
#                   length( vQtyPats ) = length( vTrtLab )
#       vObsTimeInMonths: A vector of time (in months) that patient outcomes are observed.
#       bIncreaseParam: TRUE or FALSE, is an increase in parameter value the goal? eg higher response rate 
#           TRUE --> The MAV is in terms of Q_Trt - Q_Control and use Pr( Q_Trt - Q_Control > MAV ) for decision making
#           FALSE --> The MAV is in terms of Q_Control - Q_trt and use Pr( Q_Control-Q_Trt > MAV ) for decision making
#       strBorrow: "AllControls" or "NoBorrowing"
#       strModel:  "BetaBinomial","BayesianNormalAR1", "BayesianNormal"
################################################################################################################## #
CreateISA <- function(   vQtyPats, 
                         vTrtLab,
                         vPriorA,
                         vPriorB,
                         vObsTimeInMonths,
                         dMAV,
                         vPUpper, 
                         vPLower, 
                         dFinalPUpper,   
                         dFinalPLower,
                         bIncreaseParam  = TRUE,
                         strBorrow       = "AllControls", 
                         strModel        = "BetaBinomial",
                         vMinQtyPats     = c(-1), 
                         vMinFUTime      = c(-1), 
                         dQtyMonthsBtwIA = 0 )
{
    
    
    
    bNoIA              <- FALSE
    nMaxQtyPats        <- sum( vQtyPats )
    
    if( all(vMinQtyPats == -1))
    {
        vMinQtyPats        <- c( nMaxQtyPats, nMaxQtyPats )# nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped
        
        vMinFUTime         <- c(0, 0 )
    }
    
    strBorrow          <- strBorrow
    strRandomizer      <- "EqualRandomizer"
    
    if( length( vQtyPats ) == 2 )  #This ISA will have Control and 1 treatment
        lDecisionOut       <- structure(list(strApproachIA = "default", strApproachFA="default"), class = "General")
    else
        lDecisionOut       <- structure(list(strApproachIA = "AtLeastOne", strApproachFA="AtLeastOne"), class = "GeneralDoses")
    
    
    #Define the MAV and TV
    #Outcome 1
    vAnalysisInfo1      <- c( strModel, "MAVOnly", "ProcessSingleTimeOutcome" )
    
    bPlaceMinusTrt1     <- !bIncreaseParam
    
    cISA1Info <- NewISAInfo( vTrtLab,
                             vQtyPats,
                             vMinQtyPats,
                             vMinFUTime,
                             dQtyMonthsBtwIA,
                             strRandomizer,
                             lDecisionOut,
                             strBorrow )
    
    cISAInfo <- AddBayesianOutcome( cISAInfo      = cISA1Info,
                                    vAnalysisInfo = vAnalysisInfo1,
                                    vTrtLab       = vTrtLab,
                                    vPriorA,
                                    vPriorB, 
                                    vObsTime      = vObsTimeInMonths,
                                    dMAV          = dMAV,
                                    vPUpper       = vPUpper,
                                    vPLower       = vPLower,
                                    dFinalPUpper  = dFinalPUpper,
                                    dFinalPLower  = dFinalPLower,
                                    bPlaceMinusTrt= bPlaceMinusTrt1 )
    return( cISAInfo )
}
################################################################################################################## #
#  Setup the trial design structure
################################################################################################################## #

NewTrialDesign <- function( lISADesigns, strISARandomizer )
{
    nQtyISAs  <- length( lISADesigns )
    vISANames <- paste( "cISA", 1:nQtyISAs, sep="" )
    names( lISADesigns ) <- vISANames
    cISADesigns <- structure( lISADesigns )
    
    nMaxQtyPats      <- 0
    vMaxQtyPatsInISA <-  rep( 0, nQtyISAs )
    
    
    vTrtLab <- vector()   # Used to help keep track of the number of patients on
    vISALab <- vector()
    for( nISA in 1:nQtyISAs)
    {
        nMaxQtyPatsISA           <- sum( cISADesigns[[ nISA ]]$vQtyPats )
        nMaxQtyPats              <- nMaxQtyPats + nMaxQtyPatsISA
        
        vMaxQtyPatsInISA[ nISA ] <- nMaxQtyPatsISA
        vTrtLab               <- c( vTrtLab, cISADesigns[[ nISA ]]$vTrtLab )
        vISALab               <- c( vISALab, rep( nISA, length( cISADesigns[[ nISA ]]$vTrtLab ) ) )
    }
    
    
    cTrialDesign   <- structure( list(
        nQtyISAs          = nQtyISAs,
        nMaxQtyPats       = nMaxQtyPats,
        vMaxQtyPatsInISA  = vMaxQtyPatsInISA,
        vISALab           = vISALab,
        vTrtLab           = vTrtLab,
        cISADesigns       = cISADesigns ), class=strISARandomizer )
    return( cTrialDesign )
    
}


################################################################################################################## #
#  Create the basic ISA structure
################################################################################################################## #
NewISAInfo <- function( vTrtLab,
                        vQtyPats,
                        vMinQtyPats,
                        vMinFUTime,
                        dQtyMonthsBtwIA,
                        strRandomizer,
                        lDecisionOut,
                        strBorrow)
{
    cISAAnalysis <- structure( list(   vAnalysis = list( )), class=c(strBorrow))# ,  "Independent"))
    
    cISAInfo <- structure( list(  vQtyPats        = vQtyPats,
                                  vTrtLab         = vTrtLab,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                  lDecision       = lDecisionOut,
                                  cISAAnalysis    = cISAAnalysis ), class=strRandomizer )
    
    return( cISAInfo )
    
}


################################################################################################################## #
#  Add an outcome to the cISAInfo
################################################################################################################## #
AddOutcome  <- function( cISAInfo ,
                         vAnalysisInfo,
                         vTrtLab,
                         vObsTime ,
                         dMAV,
                         dTV,
                         vLowerCI,
                         vUpperCI ,
                         dFinalLowerCI,
                         dFinalUpperCI,
                         bPlaceMinusTrt )
{
    #Analysis object for outcome 1
    cAnalysis <-  structure( list( dMAV          = dMAV,
                                   dTV           = dTV,
                                   vUpperCI      = vUpperCI,
                                   vLowerCI      = vLowerCI,
                                   dFinalLowerCI = dFinalLowerCI,
                                   dFinalUpperCI = dFinalUpperCI,
                                   bPlacMinusTrt = bPlaceMinusTrt,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab,
                                   vObsTime      = vObsTime),
                             class= vAnalysisInfo)
    
    
    
    cISAAnalysis <- cISAInfo$cISAAnalysis
    nOut <- length( cISAAnalysis$vAnalysis ) + 1
    
    cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
    cISAInfo$cISAAnalysis = cISAAnalysis
    return( cISAInfo )
    
}

################################################################################################################## #
#  Add an outcome to the cISAInfo - Based ona Bayesian analysis (eg vPUpper)
################################################################################################################## #
AddBayesianOutcome  <- function( cISAInfo ,
                                 vAnalysisInfo,
                                 vTrtLab,
                                 vPriorA,
                                 vPriorB, 
                                 vObsTime ,
                                 dMAV,
                                 vPUpper,
                                 vPLower,
                                 dFinalPUpper,
                                 dFinalPLower,
                                 bPlaceMinusTrt )
{
    #Analysis object for outcome 1
    cAnalysis <-  structure( list( dMAV          = dMAV,
                                   vPUpper       = vPUpper,
                                   vPLower       = vPLower,
                                   dFinalPUpper  = dFinalPUpper,
                                   dFinalPLower  = dFinalPLower,
                                   bPlacMinusTrt = bPlaceMinusTrt,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab,
                                   vObsTime      = vObsTime,
                                   vPriorA       = vPriorA,
                                   vPriorB       = vPriorB ),
                             class= vAnalysisInfo)
    
    
    
    cISAAnalysis <- cISAInfo$cISAAnalysis
    nOut <- length( cISAAnalysis$vAnalysis ) + 1
    
    cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
    cISAInfo$cISAAnalysis = cISAAnalysis
    return( cISAInfo )
    
}



########################################################################### #
#  The remainder of this code can likely be removed
########################################################################### # 


















# 
# 
# 
# ################################################################################################################## #
# # P2B type design - 1 Dose ISA retured
# #   Arguments:
# #       strModel:  "BayesianNormalAR1", "BayesianNormalMultiDose"
# #
# #   Note the BayesianNormalMultiDose is used so this model is faster as
# #       we are not using the HBsAg decline in 2b ISAs at the moment
# ################################################################################################################## #
# 
# Create1DosePh2BISA <- function(  vQtyPats, vTrtLab, vPUpper, vPLower, dFinalPUpper,   dFinalPLower ,
#                                  strModel = "BayesianNormal" )
# {
#     
#     dConvWeeksToMonths <- 12/52
#     
#     nMaxQtyPats        <- sum( vQtyPats )
#     
#     vMinQtyPats        <- c( nMaxQtyPats, nMaxQtyPats )# nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped
#     vMinFUTime         <- c(48 * dConvWeeksToMonths, 48 * dConvWeeksToMonths)
#     dQtyMonthsBtwIA    <- 0
#     lDecisionOut       <- structure(list(strApproachIA = "Outcome2Only", strApproachFA="Outcome2Only"), class = "General")
#     #lDecisionOut3       <- structure(list(strApproachIA = c("Outcome2Only", "AtLeastOne"), strApproachFA=c("Outcome2Only", "AtLeastOne")), class = "GeneralDoses2Outcome")
#     
#     
#     strBorrow          <- "NoBorrowing"
#     
#     strRandomizer      <- "EqualRandomizer"
#     vObsTime <- c( 0,  4,  8, 12, 16, 20, 24) * dConvWeeksToMonths
#     
#     
#     #Outcome 1
#     vAnalysisInfo1      <- c( strModel, "MAVOnly", "ProcessReptMeasChngBaseline" )
#     
#     dMAV1               <- 0.5
#     
#     
#     bPlaceMinusTrt1     <- TRUE
#     
#     cISAInfo <- NewISAInfo( vTrtLab,
#                             vQtyPats,
#                             vMinQtyPats,
#                             vMinFUTime,
#                             dQtyMonthsBtwIA,
#                             strRandomizer,
#                             lDecisionOut,
#                             strBorrow )
#     
#     
#     cISAInfo <- AddBayesianOutcome( cISAInfo      = cISAInfo,
#                                     vAnalysisInfo = vAnalysisInfo1,
#                                     vTrtLab       = vTrtLab,
#                                     vObsTime      = vObsTime,
#                                     dMAV          = dMAV1,
#                                     vPUpper       = vPUpper,
#                                     vPLower       = vPLower,
#                                     dFinalPUpper  = dFinalPUpper,
#                                     dFinalPLower  = dFinalPLower,
#                                     bPlaceMinusTrt= bPlaceMinusTrt1 )
#     
#     #Outcome 2
#     vAnalysisInfo2      <- c( "BayesianBetaBinom", "MAVOnly", "ProcessSingleTimeOutcome" )
#     
#     dMAV2               <- 0.15
#     
#     bPlaceMinusTrt2     <- FALSE
#     vObsTimeOutPh2B        <- c(48) *  dConvWeeksToMonths
#     
#     cISAInfo <- AddBayesianOutcome( cISAInfo       = cISAInfo,
#                                     vAnalysisInfo = vAnalysisInfo2,
#                                     vTrtLab       = vTrtLab,
#                                     vObsTime      = vObsTimeOutPh2B,
#                                     dMAV          = dMAV2,
#                                     vPUpper       = vPUpper,
#                                     vPLower       = vPLower,
#                                     dFinalPUpper  = dFinalPUpper,
#                                     dFinalPLower  = dFinalPLower,
#                                     bPlaceMinusTrt= bPlaceMinusTrt2 )
#     return( cISAInfo )
# }
# 
# ####################################################################### #
# # P2B type design - 2 Dose ISA
# #       strModel:  "BayesianNormalAR1", "BayesianNormalMultiDose"
# #
# #   Note the BayesianNormalMultiDose is used so this model is faster as
# #       we are not using the HBsAg decline in 2b ISAs at the moment
# ####################################################################### #
# Create2DosePh2BISA <- function(  vQtyPats, vTrtLab, vPUpper, vPLower, dFinalPUpper,   dFinalPLower ,
#                                  strModel = "BayesianNormalMultiDose"  )
# {
#     
#     
#     dConvWeeksToMonths <- 12/52
#     
#     
#     nMaxQtyPats        <- sum( vQtyPats )
#     
#     vMinQtyPats        <- c( nMaxQtyPats, nMaxQtyPats )# nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped
#     vMinFUTime         <- c(48 * dConvWeeksToMonths, 48 * dConvWeeksToMonths)
#     dQtyMonthsBtwIA    <- 0
#     
#     lDecisionOut       <- structure(list(strApproachIA = c("Outcome2Only", "AtLeastOne"), strApproachFA=c("Outcome2Only", "AtLeastOne")), class = "GeneralDoses2Outcome")
#     
#     strRandomizer      <- "EqualRandomizer"
#     strBorrow          <- "NoBorrowing"
#     
#     vObsTime <- c( 0,  4,  8, 12, 16, 20, 24) * dConvWeeksToMonths
#     
#     
#     #Outcome 1
#     vAnalysisInfo      <- c( strModel, "MAVOnly", "ProcessSingleTimeOutcome" )
#     
#     dMAV1               <- 0.5
#     
#     
#     bPlaceMinusTrt1     <- TRUE
#     
#     cISAInfo <- NewISAInfo( vTrtLab,
#                             vQtyPats,
#                             vMinQtyPats,
#                             vMinFUTime,
#                             dQtyMonthsBtwIA,
#                             strRandomizer,
#                             lDecisionOut,
#                             strBorrow )
#     
#     
#     cISAInfo <- AddBayesianOutcome(  cISAInfo      = cISAInfo,
#                                      vAnalysisInfo = vAnalysisInfo,
#                                      vTrtLab       = vTrtLab,
#                                      vObsTime      = vObsTime,
#                                      dMAV          = dMAV1,
#                                      vPUpper       = vPUpper,
#                                      vPLower       = vPLower,
#                                      dFinalPUpper  = dFinalPUpper,
#                                      dFinalPLower  = dFinalPLower,
#                                      bPlaceMinusTrt= bPlaceMinusTrt1 )
#     
#     #Outcome 2
#     vAnalysisInfo2      <- c( "BayesianBetaBinom", "MAVOnly", "ProcessSingleTimeOutcome" )
#     
#     dMAV2               <- 0.15
#     
#     
#     bPlaceMinusTrt2     <- FALSE
#     vObsTimeOutPh2B        <- c(48) *  dConvWeeksToMonths
#     
#     cISAInfo <- AddBayesianOutcome(  cISAInfo      = cISAInfo,
#                                      vAnalysisInfo = vAnalysisInfo2,
#                                      vTrtLab       = vTrtLab,
#                                      vObsTime      = vObsTimeOutPh2B,
#                                      dMAV          = dMAV2,
#                                      vPUpper       = vPUpper,
#                                      vPLower       = vPLower,
#                                      dFinalPUpper  = dFinalPUpper,
#                                      dFinalPLower  = dFinalPLower,
#                                      bPlaceMinusTrt= bPlaceMinusTrt2 )
#     
#     return( cISAInfo )
# }

