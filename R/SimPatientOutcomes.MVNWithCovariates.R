##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#####################################################################################
#' @name SimPatientOutcomes.MVNWithCovariates
#' @aliases{SimPatientOutcomes.MVNWithCovariates}
#' @title SimPatientOutcomes.MVNWithCovariates
#' @description  In this SimPatientOutcomes
#' @export
SimPatientOutcomes.MVNWithCovariates <- function( cSimOutcomes,  cISADesign,  dfPatCovISA )
{
    #print("SimPatientOutcomes.MVNWithCovariates" )
    vQtyPats        <- cISADesign$vQtyPats
    nMaxQtyPats     <- sum( vQtyPats )
    nQtyGroups      <- length( cSimOutcomes$lSimArm1 )
    vPatTrt         <- rep( cISADesign$vTrtLab, vQtyPats )
    nQtyArms        <- length( vQtyPats )

    nQtyCovs        <- length( cISADesign$cCovariates )
    vCovNames       <- names( cISADesign$cCovariates )

    vstrSimArm      <- paste( "lSimArm", 1:nQtyArms, sep="" )

    #Setup the return list
    #Could use the length( )
    lSimArm     <- cSimOutcomes[[ vstrSimArm[ 1 ]]]
    lGroupInfo    <- lSimArm[[ 1 ]]
    vMean         <- lGroupInfo[[ "vMean" ]]

    lSimDataRet <- structure( list( mSimOut1  = matrix(NA, nrow = nMaxQtyPats, ncol= length( vMean) ),
                                    vObsTime1 = cSimOutcomes$vObsTime,
                                    nQtyOut   = 1,
                                    vPatTrt   = vPatTrt ), class= class(cSimOutcomes))

    lSimDataRet[names( dfPatCovISA) ] <- dfPatCovISA[names( dfPatCovISA)]
    dfPatCovISA$Trt  <- vPatTrt   #Append the treatment so it can be used in selecting the correct rows below

    #mPatsPerGroup   <- table( lPatCovISA )

    ###################################################################
    #TODO: Validation to be moved to a Validate phase?
    bValid <- TRUE

    #TODO(Covs) - Validate info
    mCovCombinations <- expand.grid( cISADesign$cCovariates )  # This will create all possible values of the covaraites
    if( nrow( mCovCombinations ) != nQtyGroups )
        bValid <- FALSE

    ### End Validation
    ###################################################################

    mOutcome        <- NULL
    vIndGrp         <- NULL   #Will be a vector of what group a patient is in

    iGrp <- 1
    iArm <- 1

    repeat  # loop over the arms
    {

        nQtyPats    <- vQtyPats[ iArm ]    # This is the number of patients on the arm, now need to know how many are IR and NV
        lSimArm     <- cSimOutcomes[[ vstrSimArm[ iArm ]]]

        #vProbGroup  <- lSimArm$vProbGroup
        #vPatsPerGrp <- rmultinom( 1, size= nQtyPats, prob = vProbGroup )[,1]  #rmultinom returns a matrix with column for each sample, only need 1 column
        #vIndGrp     <- c( vIndGrp, rep( 1:nQtyGroups, vPatsPerGrp) )

        # Now need to simulate outcomes from each group
        iGrp       <- 1

        repeat  #Loop over the groups
        {
            lGroupInfo    <- lSimArm[[ iGrp ]]
            vMean         <- lGroupInfo[[ "vMean" ]]
            mVarCov       <- lGroupInfo[[ "mVarCov"]]  #var-cov only depends on the group
            lCov          <- data.frame( lGroupInfo[ paste( "Cov", 1:nQtyCovs, sep="" )] )
            lCov$Trt      <- cISADesign$vTrtLab[ iArm ]
            vSubset       <- SelectList( dfPatCovISA , lCov )  # Create a vector of TRUE/FALSE to assign the simulated values to as outcomes,

            nQtyPatsInGrp <- sum( vSubset )
            #print( paste( "iArm ", iArm, " iGrp ", iGrp, " nQtyPatsInGrp ", nQtyPatsInGrp ))
            if( nQtyPatsInGrp > 0 )
            {
                mOutcomeTmp <-  MASS::mvrnorm( n= nQtyPatsInGrp, vMean, mVarCov, tol = .1 )

                ## Need to check the minimums and maximums
                if( is.null( lGroupInfo$dMinimum ) == FALSE )  # A minimum was supplied
                {
                    mOutcomeTmp[ mOutcomeTmp <= lGroupInfo$dMinimum  ] <- lGroupInfo$dMinimum
                }

                if( is.null( lGroupInfo$dMaximum ) == FALSE )  # A maximum was supplied
                {
                    mOutcomeTmp[ mOutcomeTmp >= lGroupInfo$dMaximum  ] <- lGroupInfo$dMaximum
                }
                #print( paste( "nQtyPatsInGrp ", nQtyPatsInGrp, " sum(vSubset) ", sum(vSubset)))
                #print( vSubset )
                #print( mOutcomeTmp )
                lSimDataRet$mSimOut1[ vSubset, ] <- mOutcomeTmp
            }

            if( iGrp == nQtyGroups )
                break
            iGrp <- iGrp + 1
        } #End of for loop over the groups

        if( iArm == nQtyArms )
            break

        iArm <- iArm + 1

    } #End of loop over the arms

    return( lSimDataRet )
}
