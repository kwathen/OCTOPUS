##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#############################################################################################################################.
#   Description                                                                                                         #####
#   This file contains helper functions for tests
#####   Developer(s): J. Kyle Wathen, PhD                                                                               #####
#############################################################################################################################.

library( DoseFinding)
#The following function is used in connection with apply to get the pvalues of testing random samples
t.test.pval <-function( vSamp )
{
    t.test( vSamp )$p.value
}

# This function checks to make sure two lists have the same elements and the values match
# if bOrderMatters = FALSE then the items do not have to be in the same order.
AreListsEqual <- function( lList1, lList2, bOrderMatters = FALSE  )
{
    if( bOrderMatters )
    {
        bRet <- identical( lList1, lList2 )
    }
    else
    {
        bTest0 <- bTest1 <- bTest2 <- bTest3 <- bTest4 <- FALSE

        bTest0   <- all( is.list( lList1 ), is.list( lList2) )
        bLength  <- length( names( lList1 ) ) == length(  names( lList2 ) )

        if( bLength && bTest0 )
        {
            bTest1 <- all( names( lList1 ) %in% names( lList2 ) )
            bTest2 <- all( names( lList2 ) %in% names( lList1 ) )
            bTest3 <- all( unlist( lList1[ names( lList2 ) ] ) == unlist( lList2 ) )
            bTest4 <- all( unlist( lList2[ names( lList1 ) ] ) == unlist( lList1 ) )
        }
        bRet   <- all( bTest0, bTest1, bTest2, bTest3, bTest4)
    }
    return( bRet )
}



SetupTrialDesign1ISA <- function()
{

    dConvWeeksToMonths <- 12/52
    nDesign            <- 1

    bNoIA             <- FALSE
    vQtyPats          <- c(30,  60)  # Number of patients on control, treatment
    nMaxQtyPats       <- sum( vQtyPats )

    vMaxQtyPatsInISA  <- c( nMaxQtyPats )

    vMinQtyPats       <- c( 15, nMaxQtyPats ) # nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped
    vMinFUTime        <- c( 12 * dConvWeeksToMonths, 12 * dConvWeeksToMonths)
    dQtyMonthsBtwIA   <- 0 # 1 IA + FA

    vObsTime          <- c( 0, 2, 4, 8, 12 ) * dConvWeeksToMonths



    #ISA 1
    vISALab1  <- c( 1, 1)
    vTrtLab1  <- c( 1, 2)

    #ISA 1 specific stuff

    #With only 1 ISA it starts at the start of the platform
    lISAStart1   <- structure( list( dParam1=0), class="SetTime" )
    lSimISAStart <- list( lISAStart1 = lISAStart1)


    #Define the MAV and TV targets and what level of confidence to utilize
    dMAV1       <- 40
    dTV1        <- 60

    dMAV2       <- 1
    dTV2        <- 2


    vLowerCI1   <- c(0.1, 0.2)
    vUpperCI1   <- c( 0.9, 0.8)

    vLowerCI2   <- c( 0.1, 0.2)
    vUpperCI2   <- c( 0.9, 0.8)



    # Notes - Each ISA must have a cAnalysis defined for it.  By default they will use RunAnalysis.Independent
    # Each cAnalysis should have a list of analysis structures for each outcome.

    #Analysis object for outcome 1
    cAnalysis1 <- structure( list( dMAV          = dMAV1,
                                   dTV           = dTV1,
                                   vUpperCI      = vUpperCI1,
                                   vLowerCI      = vLowerCI1,
                                   bPlacMinusTrt = TRUE,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab1,
                                   vObsTime      = vObsTime),
                             class= c("ReptMeasAnalysis","ProcessReptMeasChngBaseline"))


    #Setup the list for the Outcome 2
    cAnalysis2 <- structure( list( dMAV           = dMAV2,
                                   dTV            = dTV2,
                                   vUpperCI       = vUpperCI2,
                                   vLowerCI       = vLowerCI2 ,
                                   bPlacMinusTrt  = FALSE,
                                   vTrtLab        = vTrtLab1,
                                   nVerboseOutput = 1,
                                   vObsTime       = vObsTime[5]),
                             class= c("Ranked","ProcessSingleTimeOutcome" ) )




    #For the "standard" type analysis we are assuming that you give the nISA to pull the data for.
    # when we get to do an analysis that uses control data from multiple ISA will need to write a new RunAnalysis that
    # has other options.
    cISA1Analysis <- structure( list(   vAnalysis = list( cAnalysis1 = cAnalysis1,
                                                          cAnalysis2 = cAnalysis2),
                                        nISA = 1), class= "Independent")


    lDecisionOut <- structure(list(strApproachIA = "TwoOutcomeOption3", strApproachFA="TwoOutcomeOption3"), class = "General")


    cISA1Info <- structure( list( vQtyPats        = vQtyPats,
                                  vTrtLab         = vTrtLab1,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  dQtyMonthsBtwIA = dQtyMonthsBtwIA,
                                  cISAAnalysis    = cISA1Analysis,
                                  lDecision       = lDecisionOut ), class="EqualRandomizer")


    #TODO: In validating the  cTrialDesign$cISADesigns need to make sure the ProcessDataXXX class type is the same in all ISAs


    cISADesigns <- structure( list( cISA1 = cISA1Info), class= "IndependentISA")


    cTrialDesign   <- structure( list( vObsTime          = vObsTime,
                                       nMaxQtyPats       = nMaxQtyPats,
                                       vMaxQtyPatsInISA  = vMaxQtyPatsInISA,
                                       vISALab           = vISALab1,
                                       vTrtLab           = vTrtLab1,
                                       nQtyISAs          = length( cISADesigns),
                                       cISADesigns       = cISADesigns ), class="EqualRandomizer" )

    return( cTrialDesign )

}

SetupTrialDesign2ISA <- function( strISA2Randomizer = "EqualRandomizer" )
{
    dConvWeeksToMonths <- 12/52
    bNoIA           <- FALSE
    #Originally min pats was 45 changing to 75 to make up for the second dose
    vQtyPats        <- c(70,60,60,70)
    vQtyPatsInit    <- c( 40,0,0,40 )   #This is for this version only, assuming we want 15 on dose 1 and 30 on dose 4 first with a total of 30 and 60 on those doses
    nMaxQtyPats     <- 260  #Setting this to the max that wil be run in this scenario file
    vMinQtyPats     <-  c(sum( vQtyPatsInit ),nMaxQtyPats) # nMaxQtyPats * 0.5  #The minimum number of patients at before a compound is dropped

    #mTrtStartTimes allows different doses to start after the ISA opens.  Below the mTrtStartTimes
    # specifices the first 3 doses to start when the ISA opens and the 4th dose to open 5-8 months after the
    # ISA opens.  Only valid for Randomizers of class type = "DelayedStartRandomizer"
    mTrtStartTimes  <- matrix( c( 0, 0,
                                  0, 0,
                                  0, 0,
                                  5, 8 ), byrow = TRUE, ncol = 2)


    # Check why this is needed
    vMinFUTime   <- c(12 * dConvWeeksToMonths, 12 * dConvWeeksToMonths)
    nQtyMonthsBtwIA <- 100 # 1 IA + FA

    vObsTime <- c( 0, 2, 4, 8, 12 ) * dConvWeeksToMonths



    #ISA 1 has 4 doses
    vISA     <- c( 1, 1,  1, 1 ) #Thing this will be removed
    vTrtLab1  <- c( 1, 2,  3, 4)
    vTrtLab2  <- c( 1, 4,  5, 6)

    #ISA 1 specific stuff
    vISA1Doses  <- c(0,100, 300, 900)
    dMAV1       <- 40
    dTV1        <- 60



    vPValTrend    <- c( 0.05,0.05 )
    vLowerCI1     <- c( 0.1, 0.2)
    vUpperCI1     <- c( 0.9, 0.8)


    dMAV2       <- 1
    dTV2        <- 2
    vLower2       <- c( 0.10, 0.2)
    vUpper2       <- c( 0.95, 0.9)



    maxDose<-max(vISA1Doses)
    models<-Mods(linear=NULL,
                 emax=c(50,150)*maxDose/900,
                 exponential=150*maxDose/900,
                 logistic=rbind(c(200,40),
                                c(100,23))*maxDose/320,
                 quadratic=-0.002*maxDose/900,
                 doses=vISA1Doses,
                 direction = 'decreasing')
    # Notes - Each ISA must have a cAnalysis defined for it.  By default they will use RunAnalysis.Independent
    # Each cAnalysis should have a list of analysis structures for each outcome.
    cAnalysis1 <- structure( list( dMAV          = dMAV1,
                                   dTV           = dTV1,
                                   vUpperCI      = vUpperCI1,
                                   vLowerCI      = vLowerCI1,
                                   bPlacMinusTrt = TRUE,
                                   lModels       = models,
                                   vDose         = vISA1Doses,
                                   nVerboseOutput= 1,
                                   vTrtLab       = vTrtLab1,
                                   vPValTrend    = vPValTrend,
                                   vObsTime      = vObsTime),
                             class= c("MCPModAnalysis", "AtLeastOne"))

    #Setup the list for the Outcome 2
    cAnalysis2 <- structure( list( dMAV           = dMAV2,
                                   dTV            = dTV2,
                                   vUpperCI       = vUpper2,
                                   vLowerCI       = vLower2 ,
                                   bPlacMinusTrt  = FALSE,
                                   vDose          = vISA1Doses,
                                   vTrtLab        = vTrtLab2,
                                   nVerboseOutput = 1,
                                   vObsTime       = vObsTime[5]),
                             class= c("RankedPerDoseExact","AtLeastOne" ) )





    #For the "standard" type analysis we are assuming that you give the nISA to pull the data for.
    # when we get to do an analysis that uses control data from multiple ISA will need to write a new RunAnalysis that
    # has other options.
    cISA1Analysis <- structure( list(   vAnalysis = list( cAnalysis1 = cAnalysis1,
                                                          cAnalysis2 = cAnalysis2),
                                        nISA = 1), class= "Independent")



    lDecisionOut1 <- structure(list(strApproachIA = "TwoOutcomeOption3", strApproachFA="TwoOutcomeOption3"), class = "General")


    ############## THE RANDOMIZER SHOULD BE ISA SPRCIFIC AND THIS IS NOT IMPLMEMENTED YET
    cISA1Info <- structure( list( vQtyPats        = vQtyPats,
                                  vQtyPatsInit    = vQtyPatsInit,
                                  vTrtLab         = vTrtLab1,
                                  bNoIA           = bNoIA,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  nQtyMonthsBtwIA = nQtyMonthsBtwIA,
                                  cISAAnalysis    = cISA1Analysis,
                                  lDecision       = lDecisionOut1), class="POCRandomizer")

    cISA2Info <- structure( list( vQtyPats        = vQtyPats,
                                  vQtyPatsInit    = vQtyPatsInit,
                                  vTrtLab         = vTrtLab2,
                                  bNoIA           = bNoIA,
                                  vMinQtyPats     = vMinQtyPats,
                                  vMinFUTime      = vMinFUTime,
                                  nQtyMonthsBtwIA = nQtyMonthsBtwIA,
                                  cISAAnalysis    = cISA1Analysis,
                                  lDecision       = lDecisionOut1,
                                  mStartTime      = mTrtStartTimes ), class=strISA2Randomizer)


    cISADesigns <- structure( list( cISA1 = cISA1Info, cISA2 =cISA2Info ), class= "IndependentISA")

    #The following could be moved to a Ctor for the trial design element.
    nMaxQtyPats    <- sum( cISA1Info$vQtyPats  ) + sum( cISA1Info$vQtyPats )
    vMaxQtyPatsInISA <- c( sum( cISA1Info$vQtyPats  ), sum( cISA1Info$vQtyPats ) )
    nQtyISA <- 2

    vTrtLab <- vector()   # Used to help keep track of the number of patients on
    vISALab <- vector()
    for( nISA in 1:nQtyISA)
    {
        vMaxQtyPatsInISA[ nISA ] <- sum( cISADesigns[[ nISA ]]$vQtyPats )
        vTrtLab               <- c( vTrtLab, cISADesigns[[ nISA ]]$vTrtLab )
        vISALab               <- c( vISALab, rep( nISA, length( cISADesigns[[ nISA ]]$vTrtLab ) ) )
    }
    cTrialDesign   <- structure( list( nQtyISAs        = length( cISADesigns),
                                       nMaxQtyPats       = nMaxQtyPats,
                                       vMaxQtyPatsInISA  = vMaxQtyPatsInISA,
                                       vISALab           = vISALab,
                                       vTrtLab           = vTrtLab,
                                       cISADesigns       = cISADesigns ), class="EqualRandomizer" )

    return( cTrialDesign )
}
