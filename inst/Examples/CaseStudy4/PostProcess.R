##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


###########################################################################
#   This file will post process the results from the simulation
#   The the PostProcess functions are used for a calculating summary statistics on
#   simulations that are run on the grid
#   This functions are not generic (have not had time for this) and hence are based on
#   number of ISAs in the simulation
#
###########################################################################

PostProcess1ISA  <- function( mSims, nScen )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen, ]
    print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp)))
    dAvePlacISA1  <- mean( mSimsTmp$ISA1Trt1 )
    dAveTrtISA1   <- mean( mSimsTmp$ISA1Trt2 )


    dAveN     <- dAvePlacISA1 + dAveTrtISA1

    #ISA 1 status
    vISAStatus <- c(mSimsTmp$ISA1Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA1 <- sum(vTableStatus[1:2])
    dProbFAISA1       <- sum( vTableStatus[3:5])
    dProbGoISA1       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA1     <- vTableStatus[2]+vTableStatus[4]
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrolment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA


    return( c( dAvePlacISA1=dAvePlacISA1,
                  dAveTrtISA1 = dAveTrtISA1,
                  dProbClosedIAISA1 = dProbClosedIAISA1,
                  dProbFAISA1 = dProbFAISA1,
                  dProbGoISA1 = dProbGoISA1,
                  dProbNoGoISA1 = dProbNoGoISA1))
}


PostProcess2ISA  <- function( mSims, nScen )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen, ]
    print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp)))
    dAvePlacISA1  <- mean( mSimsTmp$ISA1Trt1 )
    dAveTrtISA1   <- mean( mSimsTmp$ISA1Trt2 )

    dAvePlacISA2   <- mean( mSimsTmp$ISA2Trt1 )
    dAveTrtISA2    <- mean( mSimsTmp$ISA2Trt3 )

    dAveN     <- dAvePlacISA1 + dAveTrtISA1 + dAvePlacISA2 + dAveTrtISA2

    #ISA 1 status
    vISAStatus <- c(mSimsTmp$ISA1Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA1 <- sum(vTableStatus[1:2])
    dProbFAISA1       <- sum( vTableStatus[3:5])
    dProbGoISA1       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA1     <- vTableStatus[2]+vTableStatus[4]
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrolment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #ISA 2 status
    vISAStatus <- c(mSimsTmp$ISA2Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA2 <- sum(vTableStatus[1:2])
    dProbFAISA2       <- sum( vTableStatus[3:5])
    dProbGoISA2       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA2     <- vTableStatus[2]+vTableStatus[4]

    return( list( dAvePlacISA1=dAvePlacISA1,
                  dAveTrtISA1 = dAveTrtISA1,
                  dAvePlacISA2 = dAvePlacISA2,
                  dAveTrtISA2 = dAveTrtISA2,
                  dProbClosedIAISA1 = dProbClosedIAISA1,
                  dProbFAISA1 = dProbFAISA1,
                  dProbGoISA1 = dProbGoISA1,
                  dProbNoGoISA1 = dProbNoGoISA1,
                  dProbClosedIAISA2 = dProbClosedIAISA2,
                  dProbFAISA2 = dProbFAISA2,
                  dProbGoISA2 = dProbGoISA2,
                  dProbNoGoISA2= dProbNoGoISA2))
}


PostProcess3ISA  <- function( mSims, nScen )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen, ]
    print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp)))
    dAvePlacISA1  <- mean( mSimsTmp$ISA1Trt1 )
    dAveTrtISA1   <- mean( mSimsTmp$ISA1Trt2 )

    dAvePlacISA2   <- mean( mSimsTmp$ISA2Trt1 )
    dAveTrtISA2    <- mean( mSimsTmp$ISA2Trt3 )

    dAvePlacISA3   <- mean( mSimsTmp$ISA3Trt1 )
    dAveTrtISA3    <- mean( mSimsTmp$ISA3Trt4 )

    dAveN     <- dAvePlacISA1 + dAveTrtISA1 + dAvePlacISA2 + dAveTrtISA2 + dAvePlacISA3 + dAveTrtISA3

    #ISA 1 status
    vISAStatus <- c(mSimsTmp$ISA1Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA1 <- sum(vTableStatus[1:2])
    dProbFAISA1       <- sum( vTableStatus[3:5])
    dProbGoISA1       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA1     <- vTableStatus[2]+vTableStatus[4]
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrollment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #ISA 2 status
    vISAStatus <- c(mSimsTmp$ISA2Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA2 <- sum(vTableStatus[1:2])
    dProbFAISA2       <- sum( vTableStatus[3:5])
    dProbGoISA2       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA2     <- vTableStatus[2]+vTableStatus[4]

    #ISA 3 status
    vISAStatus <- c(mSimsTmp$ISA3Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA3 <- sum(vTableStatus[1:2])
    dProbFAISA3       <- sum( vTableStatus[3:5])
    dProbGoISA3       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA3     <- vTableStatus[2]+vTableStatus[4]

    vRet <- c( dAvePlacISA1,
               dAveTrtISA1,
               dAvePlacISA2,
               dAveTrtISA2,
               dAvePlacISA3,
               dAveTrtISA3,
               dProbClosedIAISA1,
               dProbFAISA1,
               dProbGoISA1,
               dProbNoGoISA1,
               dProbClosedIAISA2,
               dProbFAISA2,
               dProbGoISA2,
               dProbNoGoISA2,
               dProbClosedIAISA3,
               dProbFAISA3,
               dProbGoISA3,
               dProbNoGoISA3)
    return( vRet)

}


PostProcess4ISA  <- function( mSims, nScen )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen, ]
    print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp)))
    dAvePlacISA1  <- mean( mSimsTmp$ISA1Trt1 )
    dAveTrtISA1   <- mean( mSimsTmp$ISA1Trt2 )

    dAvePlacISA2   <- mean( mSimsTmp$ISA2Trt1 )
    dAveTrtISA2    <- mean( mSimsTmp$ISA2Trt3 )

    dAvePlacISA3   <- mean( mSimsTmp$ISA3Trt1 )
    dAveTrtISA3    <- mean( mSimsTmp$ISA3Trt4 )

    dAvePlacISA4   <- mean( mSimsTmp$ISA4Trt1 )
    dAveTrtISA4    <- mean( mSimsTmp$ISA4Trt5 )

    dAveN     <- dAvePlacISA1 + dAveTrtISA1 + dAvePlacISA2 + dAveTrtISA2 + dAvePlacISA3 + dAveTrtISA3 + dAvePlacISA4 + dAveTrtISA4

    #ISA 1 status
    vISAStatus <- c(mSimsTmp$ISA1Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA1 <- sum(vTableStatus[1:2])
    dProbFAISA1       <- sum( vTableStatus[3:5])
    dProbGoISA1       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA1     <- vTableStatus[2]+vTableStatus[4]
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrolment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #ISA 2 status
    vISAStatus <- c(mSimsTmp$ISA2Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA2 <- sum(vTableStatus[1:2])
    dProbFAISA2       <- sum( vTableStatus[3:5])
    dProbGoISA2       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA2     <- vTableStatus[2]+vTableStatus[4]

    #ISA 3 status
    vISAStatus <- c(mSimsTmp$ISA3Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA3 <- sum(vTableStatus[1:2])
    dProbFAISA3       <- sum( vTableStatus[3:5])
    dProbGoISA3       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA3     <- vTableStatus[2]+vTableStatus[4]


    #ISA 4 status
    vISAStatus <- c(mSimsTmp$ISA4Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA4 <- sum(vTableStatus[1:2])
    dProbFAISA4       <- sum( vTableStatus[3:5])
    dProbGoISA4       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA4     <- vTableStatus[2]+vTableStatus[4]

    vRet <- c( dAvePlacISA1,
               dAveTrtISA1,
               dAvePlacISA2,
               dAveTrtISA2,
               dAvePlacISA3,
               dAveTrtISA3,
               dAvePlacISA4,
               dAveTrtISA4,
               dProbClosedIAISA1,
               dProbFAISA1,
               dProbGoISA1,
               dProbNoGoISA1,
               dProbClosedIAISA2,
               dProbFAISA2,
               dProbGoISA2,
               dProbNoGoISA2,
               dProbClosedIAISA3,
               dProbFAISA3,
               dProbGoISA3,
               dProbNoGoISA3,
               dProbClosedIAISA4,
               dProbFAISA4,
               dProbGoISA4,
               dProbNoGoISA4)
    return( vRet)
}



PostProcess4ISA2Dose  <- function( mSims, nScen )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen, ]
    print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp)))
    dAvePlacISA1  <- mean( mSimsTmp$ISA1Trt1 )
    dAveTrt1ISA1   <- mean( mSimsTmp$ISA1Trt2 )
    dAveTrt2ISA1   <- mean( mSimsTmp$ISA1Trt3 )

    dAvePlacISA2   <- mean( mSimsTmp$ISA2Trt1 )
    dAveTrt1ISA2    <- mean( mSimsTmp$ISA2Trt4 )
    dAveTrt2ISA2    <- mean( mSimsTmp$ISA2Trt5 )

    dAvePlacISA3   <- mean( mSimsTmp$ISA3Trt1 )
    dAveTrt1ISA3    <- mean( mSimsTmp$ISA3Trt6 )
    dAveTrt2ISA3    <- mean( mSimsTmp$ISA3Trt7 )

    dAvePlacISA4   <- mean( mSimsTmp$ISA4Trt1 )
    dAveTrt1ISA4    <- mean( mSimsTmp$ISA4Trt8 )
    dAveTrt2ISA4    <- mean( mSimsTmp$ISA4Trt9 )

    dAveN     <- dAvePlacISA1 + dAveTrt1ISA1 + dAveTrt2ISA1 +
        dAvePlacISA2 + dAveTrt1ISA2 + dAveTrt2ISA2 +
        dAvePlacISA2 + dAveTrt1ISA3 + dAveTrt2ISA3 +
        dAvePlacISA4 + dAveTrt1ISA4 + dAveTrt2ISA4

    #ISA 1 status
    vISAStatus <- c(mSimsTmp$ISA1Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA1 <- sum(vTableStatus[1:2])
    dProbFAISA1       <- sum( vTableStatus[3:5])
    dProbGoISA1       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA1     <- vTableStatus[2]+vTableStatus[4]
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrolment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #ISA 2 status
    vISAStatus <- c(mSimsTmp$ISA2Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA2 <- sum(vTableStatus[1:2])
    dProbFAISA2       <- sum( vTableStatus[3:5])
    dProbGoISA2       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA2     <- vTableStatus[2]+vTableStatus[4]

    #ISA 3 status
    vISAStatus <- c(mSimsTmp$ISA3Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA3 <- sum(vTableStatus[1:2])
    dProbFAISA3       <- sum( vTableStatus[3:5])
    dProbGoISA3       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA3     <- vTableStatus[2]+vTableStatus[4]


    #ISA 4 status
    vISAStatus <- c(mSimsTmp$ISA4Status, 3:7)
    vTableStatus <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus <- vTableStatus/nrow( mSimsTmp )

    dProbClosedIAISA4 <- sum(vTableStatus[1:2])
    dProbFAISA4       <- sum( vTableStatus[3:5])
    dProbGoISA4       <- vTableStatus[1]+vTableStatus[3]
    dProbNoGoISA4     <- vTableStatus[2]+vTableStatus[4]

    vRet <- c( dAvePlacISA1,
               dAveTrt1ISA1,
               dAveTrt2ISA1,
               dAvePlacISA2,
               dAveTrt1ISA2,
               dAveTrt2ISA2,
               dAvePlacISA3,
               dAveTrt1ISA3,
               dAveTrt2ISA3,
               dAvePlacISA4,
               dAveTrt1ISA4,
               dAveTrt2ISA4,
               dProbClosedIAISA1,
               dProbFAISA1,
               dProbGoISA1,
               dProbNoGoISA1,
               dProbClosedIAISA2,
               dProbFAISA2,
               dProbGoISA2,
               dProbNoGoISA2,
               dProbClosedIAISA3,
               dProbFAISA3,
               dProbGoISA3,
               dProbNoGoISA3,
               dProbClosedIAISA4,
               dProbFAISA4,
               dProbGoISA4,
               dProbNoGoISA4)
    return( vRet)
}

library( OCTOPUS)


########################################################################
# Used for plots
########################################################################

vMeanR  <- c( 3.5, 3.4163, 2.9882, 2.486, 1.9641, 1.4423, 0.9204 )
vMeanNR <- c( 3.5, 3.4933, 3.4867, 3.48,  3.4733, 3.4667, 3.46 )

vProbGroupCtrl  <- c( 0.02, 0.98 )       #The percentage of R and NR in the ctrl group

dTruePlac <- (vMeanNR*vProbGroupCtrl[2] + vMeanR*vProbGroupCtrl[1])[7]
vTrueDiff <- c(0)
vTrueProb <-c(0.05,0.1,0.15,0.2,0.3,0.4)
for( i in 1:length(vTrueProb ))
    vTrueDiff <- c( vTrueDiff, dTruePlac - (vMeanNR[7]*(1-vTrueProb[i]) + vMeanR[7]*vTrueProb[i]))



########################################################################




########################################################################
#   Read in the data from the simulation with borrowing
#   make sure your working directory is the same as this file or
#   add the directory to the string below
########################################################################

load( "sims.RData")

#strMain <- "allmain.csv"
#vStrISA <- c("allisa1.csv", "allisa2.csv","allisa3.csv","allisa3.csv")
#vISANumber <- c(1,2,3,4)

#simsAll <- BuildResultsDataSet( strMain, vStrISA, vISANumber )  #This is a fct in the PTS Package

#write.table(simsAll, "combined.csv", quote=FALSE, sep="," , row.names=FALSE)
#mSimsTmp <- simsAll[ mSims$iScen == nScen, ]
#nrow( simsAll)

vScen <- unique(simsAll$iScen)

vRes <- c( 1, PostProcess1ISA( simsAll, 1))
for( iScen in 2:length(vScen) )
{
    vRes <- rbind( vRes, c(iScen, PostProcess1ISA( simsAll, iScen)))
}

colnames( vRes )<-c( "Scen", "Ave N Plac", "Ave N Trt", "Pr(Closed IA)", "Pr( FA )", "Pr( Go )", "Pr( No Go )")
vX <- c(0, 0.05,0.1,0.15,0.2,0.3,0.4)
plot( vX, vRes[,"Pr( Go )"], type='l', xlab="% R", ylab="Probability")
lines( vX, vRes[,"Pr( Go )"], lwd=2, col="green")
lines( vX, vRes[,"Pr( No Go )"], lwd=2, col="red")
lines( vX, 1.0-vRes[,"Pr( Go )"]-vRes[,"Pr( No Go )"], lwd=2, col="yellow")


abline( v= seq( 0.0, .5, .05), h=seq( 0,1, .1), lty=2, col=8)



