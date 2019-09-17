##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#####################################################################################
#   The ability to simulate from a MVN with a covariate has been included since Case Study 3 was originally developed.
#   Thus, for Case Study 3 we name the new outcome MVNWithCovariateCS3 but please note this code was largely copied from
#   MVNWithCovariate in the base package.
#
#   In this SimPatientOutcomes function we simulate patients from a MVN where there is a group covariate.
#   For each arm in the ISA you specify a lSimArm that specifies how the patients in that
#   arm are to be simulated.
#   The cSimoutcomes is a structure( list(lSimArm1, lSimArm2,..., lSimArmXX), class="MVNWithCovariateCS3" )
#   Assuming the number of groups you want to simulate from is J, each lSimArm will have the following elements
#
#   Required for XX in 1:J :
#       $vMeanXX - The mean vector for group XX
#       $mVarCovXX - The var-cov matrix for group XX
#
#       $vProbGroup - Probability a patient is in each group
#       $vObsTime   - The times each of the outcomes are observed length( vObsTime ) == length( vMeanXX ); for all XX
#
#   Optional Argument to constrain the simulated results if needed:
#       $dMinimum - All simulated values must be >= dMinimum
#       $dMaximum - All simulated values must be <= dMaximum
#
#
# ## This example contains 2 arms and each arm will simulate from 2 populaitons.
# ## Assuming vMeanR1, vMeanNR2 are mean vector for arm 1 defined elsewhere in the code and have length = 3
# ## Assuming mVarCovR1, mVarCovNR1 are var-cov for arm 1 defined elsewhere in the code and are a 3x3 matrix
# ## Assuming vMeanR2, vMeanNR2 are mean vector for arm 2 defined elsewhere in the code and have length = 3
# ## Assuming mVarCovR2, mVarCovNR2 are var-cov for arm 2 defined elsewhere in the code and are a 3x3 matrix
#
# ## The following example would simulate patient with 3 measurements.  For arm 1, on average, 20% would
# ## be from group 1 and 80% from group 2 with the respective mean and var-cov.  Patients in arm 2 would be simulated
# ## with 30%, on average, from group 1 and 70%, on average, from group 2 with the respective mean and var cov.
# vMeanR1    <- c( 10, 15 )
# mVarCovR1  <- diag(1, nrow=2)
# vMeanNR1   <- c( 5, 8 )
# mVarCovNR1 <- diag( 1, nrow=2)
# lSimArm1   <- list( vMean1     = vMeanR1,
#                     mVarCov1   = mVarCovR1,
#                     vMean2     = vMeanNR1,
#                     mVarCov2   = mVarCovNR1,
#                     vProbGroup = c( 0.2, .8 ),
#                     dMinimum   = 0,
#                     vObsTime   = c( 0, 2, 4 ))
#
# vMeanR2    <- c( 10, 15 )
# mVarCovR2  <- diag(1, nrow=2)
# vMeanNR2   <- c( 5, 8 )
# mVarCovNR2 <- diag( 1, nrow=2)
# lSimArm2   <- list( vMean1     = vMeanR2,
#                     mVarCov1   = mVarCovR2,
#                     vMean2     = vMeanNR2,
#                     mVarCov2   = mVarCovNR2,
#                     vProbGroup = c( 0.3, .7 ),
#                     dMinimum   = 0,
#                     vObsTime   = c( 0, 2, 4 ))
#
#  cSimOutcomes <- structure( list(lSimArm1 = lSimArm1, lSimArm2 = lSimArm2), class =  "MVNWithCovariateCS3" )
SimPatientOutcomes.MVNWithCovariateCS3 <- function( cSimOutcomes,  cISADesign )
{
    print("SimPatientOutcomes.MVNWithCovariateCS3" )

    vQtyPats        <- cISADesign$vQtyPats
    nQtyGroups      <- length( cSimOutcomes$lSimArm1$vProbGroup)
    vPatTrt         <- rep( cISADesign$vTrtLab, vQtyPats )
    nQtyArms        <- length( vQtyPats )

    vstrMeanName    <- paste( "vMean", 1:nQtyGroups, sep="")
    vstrVarCovName  <- paste( "mVarCov", 1:nQtyGroups, sep="")
    vstrSimArm      <- paste( "lSimArm", 1:nQtyArms, sep="" )

    ###################################################################
    #TODO: Validation to be moved to a Validate phase?
    bValid <- TRUE
    strErr <- ""
    strErrPrfx <- "SimPatientOutcomes.MVNWithCovariateCS3 - Validation Error: "
    if( length( cSimOutcomes ) != nQtyArms )
    {
        bValid <- FALSE
        strErr <- paste( strErrPrfx, "The number of arms in ISA ( ", nQtyArms, " )")
        strErr <- paste( strErr, " != number of lSimArms ( ", length( cSimoutcomes), " in cSimOutcomes object.")
    }

    for( iArm in 1:nQtyArms )
    {
        lSimArm <- cSimOutcomes[[ vstrSimArm[ iArm ]]]
        if( sum( lSimArm$vProbGroup )  != 1 )
        {
            bValid <- FALSE
            strErr <- paste( strErr, strErrPrfx,  "For lSimArm", iArm, " sum( vProbGroup ) != 1.")

        }
        if( length( lSimArm$vMean1 ) != length( lSimArm$vObsTime ) )
        {
            bValid <- FALSE
            strErr <- paste( strErr, strErrPrfx,  "For lSimArm", iArm, "  length( lSimArm$vMean1 ) != legnth( lSimArm$vObsTime )")
        }

        #TODO: More validation
        # 1.  Make sure the number of vMean and mVarCov supplied = length( vProbGroup )
        # 2.  Make sure length( vMeanX ) = nrow( mVarCovX ) = ncol( mVarCov )
    }

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

        vProbGroup  <- lSimArm$vProbGroup
        vPatsPerGrp <- rmultinom( 1, size= nQtyPats, prob = vProbGroup )[,1]  #rmultinom returns a matrix with column for each sample, only need 1 column
        vIndGrp     <- c( vIndGrp, rep( 1:nQtyGroups, vPatsPerGrp) )

        # Now need to simulate outcomes from each group
        iGrp       <- 1

        repeat  #Loop over the groups
        {
            vMean      <- lSimArm[[ vstrMeanName[ iGrp ] ]]
            mVarCov    <- lSimArm[[ vstrVarCovName[ iGrp ] ]]  #var-cov only depends on the group

            if(vPatsPerGrp[ iGrp ] > 0 )
            {
                mOutcomeTmp <-  MASS::mvrnorm( n= vPatsPerGrp[ iGrp ], vMean, mVarCov, tol = .1 )

                ## Need to check the minimums and maximums
                if( is.null( lSimArm$dMinimum ) == FALSE )  # A minimum was supplied
                {
                    mOutcomeTmp[ mOutcomeTmp <= lSimArm$dMinimum  ] <- lSimArm$dMinimum
                }

                if( is.null( lSimArm$dMaximum ) == FALSE )  # A maximum was supplied
                {
                    mOutcomeTmp[ mOutcomeTmp >= lSimArm$dMaximum  ] <- lSimArm$dMaximum
                }
                mOutcome    <- rbind( mOutcome, mOutcomeTmp)
            }

            if( iGrp == nQtyGroups )
                break
            iGrp <- iGrp + 1
        } #End of for loop over the groups

        if( iArm == nQtyArms )
            break

        iArm <- iArm + 1

    } #End of loop over the arms

    lSimDataRet <- structure( list( mSimOut1 = mOutcome, vObsTime1 = cSimOutcomes[[1]]$vObsTime, vCov1 = vIndGrp  ), class= class(cSimOutcomes) )

    lSimDataRet$nQtyOut  <- 1
    lSimDataRet$vPatTrt  <- vPatTrt
    #lSimDataRet$vPatISA  <- rep( cTrialDesign$vISA, cTrialDesign$vQtyPats )
    return( lSimDataRet )
}
