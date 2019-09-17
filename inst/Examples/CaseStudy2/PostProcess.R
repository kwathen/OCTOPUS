##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#############################################################################################################################.
#   This file will post process the results from the simulation
#   The the PostProcess functions are used for a calculating summary statistics on
#   simulations that are run on the grid
#   This functions are not generic (have not had time for this) and hence are based on
#   number of ISAs in the simulation
#############################################################################################################################.

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
    #   2 = met max enrollment,
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


library( OCTOPUS )


#########################################################################################################.
#   Process the simulation results in sims.RData                                                    #####
#   Read in the data from the simulation make sure your working directory is the same as this file or
#   add the directory to the string below
#########################################################################################################.

load( "sims.RData")


vScen <- unique(simsAll$iScen)

vRes <- c( 1, PostProcess1ISA( simsAll, 1))
for( iScen in 2:length(vScen) )
{
    vRes <- rbind( vRes, c(iScen, PostProcess1ISA( simsAll, iScen)))
}


#  Plot the results - Mean vectors taken from the SimulationDesign.R. #####
vMeanTrt  <- c( 3.5, 3.4, 3.0, 2.5,  2.9, 1.4, 0.9 )
vMeanCtrl <- c( 3.5, 3.5, 3.5, 3.5,  3.5, 3.5, 3.5 )
vEffectPercent <- c(0, 0.1, 0.2, 0.3, 0.4, 1.0)
vDiff <- -1 * (vMeanTrt - vMeanCtrl )[7] * ( vEffectPercent )

colnames( vRes )<-c( "Scen", "Ave N Plac", "Ave N Trt", "Pr(Closed IA)", "Pr( FA )", "Pr( Go )", "Pr( No Go )")
plot( vDiff, vRes[,"Pr( Go )"], type='l', xlab="Difference at Week 24", ylab="Probability")
lines( vDiff, vRes[,"Pr( Go )"], lwd=2, col="green")
lines( vDiff, vRes[,"Pr( No Go )"], lwd=2, col="red")
lines( vDiff, 1.0-vRes[,"Pr( Go )"]-vRes[,"Pr( No Go )"], lwd=2, col="yellow")
abline( v= 0.5, lwd=2, lty=3 ) # Plot the MAV

abline( v= seq( 0.0, 2.5, .25), h=seq( 0,1, .1), lty=2, col=8)



