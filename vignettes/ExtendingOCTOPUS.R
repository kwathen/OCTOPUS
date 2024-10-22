## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----setp, eval=FALSE---------------------------------------------------------
#  SimPatientOutcomes.Categorical <- function(  cSimOutcomes, cISADesign, dfPatCovISA )
#  {
#      # Often it is good to print out a message when you first use so it is clear the correct function was called
#      #print( "Executing SimPatientOutcomes.Categorical ...")
#      if( !is.null(  dfPatCovISA  ) )
#          stop( "SimPatientOutcomes.Categoricaly is not designed to incorporate patient covariates and  dfPatCovISA  is not NULL.")
#  
#  
#      mOutcome        <- NULL
#  
#      vProbResponse   <- cSimOutcomes$vProbResponse
#      vQtyPats        <- cISADesign$vQtyPats
#  
#      vPatTrt         <- rep( cISADesign$vTrtLab, vQtyPats )
#      iArm            <- 1
#      for( iArm in 1:length( vQtyPats ) )
#      {
#          # Note: in this example only a binomial outcome is simulated.  This could easily be altered to be multi-nomial or whatever is needed.
#          vPatientOutcomes <- rbinom( vQtyPats[ iArm ], 1, vProbResponse[ iArm ] )
#          mOutcome         <- rbind( mOutcome, matrix( vPatientOutcomes , ncol = 1) )
#      }
#  
#  
#      lSimDataRet <- structure( list( mSimOut1 = mOutcome, vObsTime1 = cISADesign$cISAAnalysis$vAnalysis[[1]]$vObsTime ), class= class(cSimOutcomes) )
#  
#  
#      lSimDataRet$nQtyOut  <- 1
#      lSimDataRet$vPatTrt  <- vPatTrt
#  
#      return( lSimDataRet )
#  
#  }

