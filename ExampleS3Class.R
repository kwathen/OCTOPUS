##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#############################################################################################################################.
#   Description: This file is intended to give an example of S3 classes and the main use for this library.
#   The main use of the S3 classes in this library will be for generic functions and method dispatch.
#   This example develops a generic function named RunCalculationEXAMPLE( lData ) that will be dispatched to specific
#   definitions based on the class of lData. For example, the following three implementations of RunCalculationEXAMPLE are developed:
#       1. RunCalculationEXAMPLE.Binomial
#       2. RunCalculationEXAMPLE.TTE
#       3. RunCalculationEXAMPLE.Normal
#   This example then demonstrates how each of the called.  The first line of ecach function is a print to make it clear which
#   version is called.
#
#   A good introduction to S3 classes can be found at http://adv-r.had.co.nz/S3.html
#
#
#####   Developer(s): J. Kyle Wathen, PhD                                                                              #####
############################################################################################################################.

#This next line declares a generic function called RunCalculation that will dispatch based on class( lData )
RunCalculationEXAMPLE <- function( lData )
{
    UseMethod("RunCalculationEXAMPLE", lData)
}

# If we implemented a .default function and a user gave an incorrect class then the default would execute and may give invalid results
# So, unless there is a natural default then the default should cause an error
RunCalculationEXAMPLE.default <- function( lData )
{
    print( paste("ERROR: The default for RunCalculation is NOT defined"  ))

    #It may be a better practice to make the default stop so you don't keep running a simulation
    #stop( "ERROR: The default for RunCalculation is NOT defined ")
}

# If class( lData) = Binomial this version will be called.
RunCalculationEXAMPLE.Binomial <- function( lData )
{
    print( paste( "Running the calculations for an object of class = Binomial"))
    return( mean( rbeta( 10000, lData$dPriorA + lData$nX, lData$dPriorB + lData$nN - lData$nX )))

}

# If class( lData) = TTE this version will be called.
RunCalculationEXAMPLE.TTE <- function( lData )
{
    print( paste( "Running the calculations for an object of class =TTE"))
    return( mean( 1/rgamma( 10000, lData$dPriorA + lData$nQtyEvents, lData$dPriorB + lData$dTTT )))
}

# If class( lData) = Normal this version will be called.
RunCalculationEXAMPLE.Normal<- function( lData )
{
    print( paste( "Running the calculations for an object of class =Normal"))
    #Note: This calculation is not intended as an example.
    return( mean( rnorm( 10000, mean = lData$dPriorMean, sd = lData$dSD ) ))
}

#Create an object that should call RunCalculationEXAMPLE.Binomial
lDataBin <- structure( list( dPriorA = 0.2, dPriorB = 0.8, nX = 3, nN=10 ), class= "Binomial" )

#Create an object that should call RunCalculationEXAMPLE.TTE
lDataTTE <- structure( list( dPriorA = 0.2, dPriorB = 0.8, nQtyEvents = 5, dTTT = 140.2 ), class= "TTE" )

#Create an object that has a class that does not have a RunCalculationEXAMPLE defined for it
lDataInvalid <- structure( list( dPriorA = 0.2, dPriorB = 0.8 ), class= "Normal1" )

#Notice that we call RunCalculationEXAMPLE but the method that actually gets called depends on the class of the variable it is called with

RunCalculationEXAMPLE( lDataBin )       #This should call the RunCalculationEXAMPLE.Binomial
RunCalculationEXAMPLE( lDataTTE )       #This should call the RunCalculationEXAMPLE.TTE
RunCalculationEXAMPLE( lDataInvalid)    #This should call the RunCalculationEXAMPLE.default which is not defined and would typically

#To call the RunCalculationEXAMPLE.Normal the class must be Normal
lData <- structure( list( dPriorMean = 0.2, dSD = 2.0 ), class= "Normal" )
RunCalculationEXAMPLE( lData )

#############################################################################.
#Create a generic function SimulateData used to simulate data
#############################################################################.

SimulateData <- function( lData )
{
    UseMethod("SimulateData", lData)
}
SimulateData.default <- function( lData )
{
    print( paste("ERROR: The default for SimulateData is NOT defined"  ))

    #It may be a better practice to make the default stop so you don't keep running a simulation
    #stop( "ERROR: The default for RunCalculation is NOT defined ")
}
SimulateData.Binary <- function( lData )
{

    print( paste("Calling SimulateData.Binary "  ))


    vY <- rbinom( 1, lData$nN, 0.2 )
    return( structure( list( vY = vY), class=class( lData ) ))
}

#Simulate the patients as a mixture based on the covariate.   First simulate the number of patients with Cov=1 based on
# lData$dProbCov1, then simulate the responses based on dProbSuccess0 and dProbSuccess1, the response probabilities
# for patient with the covariate = 0 and 1 respectively.
SimulateData.BinaryCov <- function( lData )
{

    print( paste("Calling SimulateData.BinaryCov "  ))

    #Simulate the patients as

    nNCov1 <- rbinom( 1, lData$nN, lData$dProbCov1 )
    vY1    <- rbinom( nNCov1, 1,  lData$dProbSuccess1 )
    vY0    <- rbinom( lData$nN - nNCov1, 1,  lData$dProbSuccess0)
    return( structure( list( vY = c( vY0, vY1 )), class=class( lData ) ))
}

#Create an object that should call RunCalculationEXAMPLE.Binomial
lDataBin <- structure( list( dPriorA = 0.2, dPriorB = 0.8,  nN=20, nX = 2,
                             dProbCov1 = 0.5, dProbSuccess0 = 0.2, dProbSuccess1 = 0.8 ), class= c("Binomial","BinaryCov") )

RunCalculationEXAMPLE( lDataBin )
SimulateData( lDataBin )



