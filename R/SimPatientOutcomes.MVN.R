##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name SimPatientOutcomes.MVN
#' @title SimPatientOutcomes.MVN
#' @description  Simulate a MVN outcomes.  This one will likely be used by outcomes that are simulated as correlated.
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/SimPatientOutcomes.MVN.R}{View Code on GitHub} }
#' @export
SimPatientOutcomes.MVN <- function(  cSimOutcomes, cISADesign, dfPatCovISA )
{
    if( !is.null(  dfPatCovISA  ) )
        stop( "SimPatientOutcomes.MVN is not designed to incorporate patient covariates and  dfPatCovISA  is not NULL.")

    #TODO: Check
    # The length( vQtyPats ) should = nrow(cSimOutcomes)
    #print( "SimPatientOutcomes.MVN 2")
    #vQtyPats <- cTrialDesign$vQtyPats
    mOutcome <- NULL

    vPatTrt  <- vector() #rep( cTrialDesign$vTrtLab, cTrialDesign$vQtyPats )
    #vPatISA  <- vector() #rep( cTrialDesign$vISA, cTrialDesign$vQtyPats )

    mVarCov  <- cSimOutcomes$mVarCov

    vQtyPats  <- cISADesign$vQtyPats

    vPatTrt  <- c( vPatTrt, rep( cISADesign$vTrtLab, vQtyPats ) )
    #vPatISA  <- c(vPatISA,  rep( iISA, sum(vQtyPats) ) )
    iArm <-1
    for( iArm in 1:length( vQtyPats ) )
    {
        vMean    <- cSimOutcomes$mMean[ iArm, ]
        #print( vMean)


        #Each row is a patient, each column is a time point
        mOutcome <- rbind( mOutcome, MASS::mvrnorm( n=vQtyPats[ iArm ], vMean, mVarCov, tol = .1 ) )
    }


    lSimDataRet <- structure( list( mSimOut1 = mOutcome, vObsTime1 = cSimOutcomes$vObsTime ), class= class(cSimOutcomes) )


    lSimDataRet$nQtyOut  <- 1#length( cSimOutcomes )
    lSimDataRet$vPatTrt  <- vPatTrt
    #lSimDataRet$vPatISA  <- vPatISA

    return( lSimDataRet )

}
