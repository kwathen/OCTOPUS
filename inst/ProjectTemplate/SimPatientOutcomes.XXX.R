
################################################################################################### #
#   Description - This file will add a new patient outcome where patient outcomes are binary
#
#   TODO: Test this function.
################################################################################################### #

SimPatientOutcomes.Binary <- function(  cSimOutcomes, cISADesign, dfPatCovISA )
{
    print( "Executing SimPatientOutcomes.Binary ...")
    if( !is.null(  dfPatCovISA  ) )
        stop( "SimPatientOutcomes.Binary is not designed to incorporate patient covariates and  dfPatCovISA  is not NULL.")
    
    
    mOutcome        <- NULL
    
    vProbResponse   <- cSimOutcomes$vProbResponse
    vQtyPats        <- cISADesign$vQtyPats
    
    vPatTrt         <- rep( cISADesign$vTrtLab, vQtyPats )
    iArm            <- 1
    for( iArm in 1:length( vQtyPats ) )
    {
        
        vPatientOutcomes <- rbinom( vQtyPats[ iArm ], 1, vProbResponse[ iArm ] )
        #print( paste( "iArm ", iArm, " Prob Resp ", vProbResponse[ iArm ], " # Pats ", vQtyPats[ iArm ], " % resp ", sum(vPatientOutcomes)/vQtyPats[iArm]))
        mOutcome         <- rbind( mOutcome, matrix( vPatientOutcomes , ncol = 1) ) 
    }
    
    
    lSimDataRet <- structure( list( mSimOut1 = mOutcome, vObsTime1 = cISADesign$cISAAnalysis$vAnalysis[[1]]$vObsTime ), class= class(cSimOutcomes) )
    
    
    lSimDataRet$nQtyOut  <- 1#length( cSimOutcomes )
    lSimDataRet$vPatTrt  <- vPatTrt
    
    return( lSimDataRet )
    
}