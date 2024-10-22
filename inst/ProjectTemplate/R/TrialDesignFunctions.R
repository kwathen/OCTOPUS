
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
                         nISA,
                         vTrtLab,
                         vObsTimeInMonths,
                         dMAV            = NULL,
                         dTV             = NULL,
                         vUpperCI        = NULL,
                         vLowerCI        = NULL,
                         dFinalLowerCI   = NULL,
                         dFinalUpperCI   = NULL,
                         vPUpper         = NULL, 
                         vPLower         = NULL, 
                         dFinalPUpper    = NULL,   
                         dFinalPLower    = NULL,
                         bIncreaseParam  = TRUE,
                         strBorrow       = "AllControls", 
                         strModel        = "BetaBinomial",
                         vMinQtyPats     = c(-1), 
                         vMinFUTime      = c(-1), 
                         dQtyMonthsBtwIA = 0,
                         lAnalysis       = NULL )
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
    
    cISA1Info <- NewISAInfo( nISA, 
                             vTrtLab,
                             vQtyPats,
                             vMinQtyPats,
                             vMinFUTime,
                             dQtyMonthsBtwIA,
                             strRandomizer,
                             lDecisionOut,
                             strBorrow )
    
    
    cISAInfo <- AddOutcome( cISAInfo      = cISA1Info,
                            vAnalysisInfo = vAnalysisInfo1,
                            nISA          = nISA,
                            vTrtLab       = vTrtLab,
                            vObsTime      = vObsTimeInMonths,
                            dMAV          = dMAV,
                            dTV           = dTV,
                            vUpperCI      = vUpperCI,
                            vLowerCI      = vLowerCI,
                            dFinalLowerCI = dFinalLowerCI,
                            dFinalUpperCI = dFinalUpperCI,
                            vPUpper       = vPUpper,
                            vPLower       = vPLower,
                            dFinalPUpper  = dFinalPUpper,
                            dFinalPLower  = dFinalPLower,
                            bPlaceMinusTrt= bPlaceMinusTrt1,
                            lAnalysis     = lAnalysis )
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
NewISAInfo <- function( nISA,
                        vTrtLab,
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
                                  nISA            = nISA,
                                  vTrtLab         = vTrtLab,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                  lDecision       = lDecisionOut,
                                  cISAAnalysis    = cISAAnalysis), class=strRandomizer )
    
    return( cISAInfo )
    
}


# ################################################################################################################## #
# #  Add an outcome to the cISAInfo
# ################################################################################################################## #
# AddOutcome  <- function( cISAInfo ,
#                          vAnalysisInfo,
#                          vTrtLab,
#                          vObsTime ,
#                          dMAV,
#                          dTV,
#                          vLowerCI,
#                          vUpperCI ,
#                          dFinalLowerCI,
#                          dFinalUpperCI,
#                          bPlaceMinusTrt )
# {
#    
#     cISAAnalysis <- structure( list(   vAnalysis = list), class=c(strBorrow))# ,  "Independent"))
#     
#     
#     #Analysis object for outcome 1
#     cAnalysis <-  structure( list( dMAV          = dMAV,
#                                    dTV           = dTV,
#                                    vUpperCI      = vUpperCI,
#                                    vLowerCI      = vLowerCI,
#                                    dFinalLowerCI = dFinalLowerCI,
#                                    dFinalUpperCI = dFinalUpperCI,
#                                    bPlacMinusTrt = bPlaceMinusTrt,
#                                    nVerboseOutput= 1,
#                                    vTrtLab       = vTrtLab,
#                                    vObsTime      = vObsTime,
#                                    unlist( lAnalysis )),
#                              class= vAnalysisInfo)
#     
#     
#     
#     cISAAnalysis <- cISAInfo$cISAAnalysis
#     nOut <- length( cISAAnalysis$vAnalysis ) + 1
#     
#     cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
#     cISAInfo$cISAAnalysis = cISAAnalysis
#     return( cISAInfo )
#     
# }
################################################################################################################## #
#  Add an outcome to the cISAInfo - Based ona Bayesian analysis (eg vPUpper)
################################################################################################################## #
AddOutcome  <- function( cISAInfo ,
                         vAnalysisInfo,
                         nISA,
                         vTrtLab,
                         vObsTime ,
                         dMAV,
                         dTV,
                         vUpperCI,
                         vLowerCI,
                         dFinalLowerCI,
                         dFinalUpperCI,
                         vPUpper,
                         vPLower,
                         dFinalPUpper,
                         dFinalPLower,
                         bPlaceMinusTrt,
                         lAnalysis = NULL )
{
    
   
    
    
    
    cAnalysis <-  structure( list( dMAV          = dMAV,
                                   dTV           = dTV,
                                   vUpperCI      = vUpperCI,
                                   vLowerCI      = vLowerCI,
                                   dFinalLowerCI = dFinalLowerCI,
                                   dFinalUpperCI = dFinalUpperCI,
                                   vPUpper       = vPUpper,
                                   vPLower       = vPLower,
                                   dFinalPUpper  = dFinalPUpper,
                                   dFinalPLower  = dFinalPLower,
                                   bPlacMinusTrt = bPlaceMinusTrt,
                                   nVerboseOutput= 1,
                                   nISA          = nISA,
                                   vTrtLab       = vTrtLab,
                                   vObsTime      = vObsTime),
                             class= vAnalysisInfo) 
    #Analysis object for outcome 1
    if( !is.null( lAnalysis ) )
    {
        for( i in 1:length( lAnalysis))
        {
            cAnalysis[[ names( lAnalysis)[i]  ]] <- lAnalysis[[ i ]]
        }
    }
    
    
    
    cISAAnalysis <- cISAInfo$cISAAnalysis
    nOut <- length( cISAAnalysis$vAnalysis ) + 1
    
    cISAAnalysis$vAnalysis[[ nOut  ]] <- cAnalysis
    cISAInfo$cISAAnalysis <- cISAAnalysis
    return( cISAInfo )
    
}
