##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#############################################################################################################################.
#   Description                                                                                                         #####
#   This file tests the functionality of the AccrualMethods class.
#####   Developer(s): J. Kyle Wathen, PhD                                                                               #####
#############################################################################################################################.


#####  Test the AccrualMethods class  ######################################################################################
#
############################################################################################################################.


context("Test - AccrualMethods.R")
gDebug<- FALSE

#Helper function for testing the slots in the AccrualMethods
AddAccProSlotTests <- function(  ap, vExpPatsPerMonth, nExpMaxQtyPats, strExpDesc, nExpMaxMonthsOfAcc )
{
    expect_equal(ap@m.vQtyPatsPerMonth,     vExpPatsPerMonth,    label = "vPatsPerMonth: " )
    expect_equal(ap@m.nMaxQtyPatients,      nExpMaxQtyPats,      label = "nMaxQtyPats: " )
    expect_equal(ap@m.strDescription,       strExpDesc,          label = "m.strDescription: "  )
    expect_equal(ap@m.nMaxMonthsOfAccrual,  nExpMaxMonthsOfAcc,  label = "m.nMaxMonthsOfAccrual:"  )
}

# Error function used in some tests, just returns TRUE if an error is thrown.
Error<-function(e){TRUE}

#Create a test set

test_that("AccrualMethods Class",
    {
        vTestPatsPerMonth       <- c(1,10,20)
        nTestMaxQtyPats         <- 100
        strTestDesc             <- "Accrual will continue until the max qty of patients is reached.  Using a variable accrual rate"
        nTestMaxQtyMonths       <- 10

        #This should create an object that simulates until a max number of patients
        ap      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats )
        AddAccProSlotTests( ap, vTestPatsPerMonth, nTestMaxQtyPats, strTestDesc, -1)
    })
#

#Test to make sure that the number of simulated arrival times is equal to the max number requested.

test_that( "SimulateArrivalTimes (MaxQtyPats): ",
    {
        gDebug<- FALSE
        vTestPatsPerMonth       <- c( 1, 10, 20 )
        nTestMaxQtyPats         <- 100
        ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats )
        vTestAccTimes           <- SimulateArrivalTimes( ap )
        expect_equal( nTestMaxQtyPats, length( vTestAccTimes ) )
    })

test_that( "SimulateArrivalTimes (max number of months): ",
    {
        vTestPatsPerMonth   <- c( 1, 10, 20 )
        nTestMaxQtyMonths   <- 20
        nTestMaxQtyPats     <- -1
        strTestDesc         <- "Accrual will continue until  max months of accrual is reached.  Using a variable accrual rate"

        ap                  <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxMonthsOfAccrual = nTestMaxQtyMonths )
        AddAccProSlotTests( ap, vTestPatsPerMonth, nTestMaxQtyPats, strTestDesc, nTestMaxQtyMonths)
    })


test_that( "SimulateArrivalTimes (max number of months invalid): ",
    {
        gDebug<- FALSE
         vTestPatsPerMonth <- c(1,10,20) # The will cause an Error in SimualteAccrualTimes
         nTestMaxQtyMonths   <- 20
         nTestMaxQtyPats     <- -1
         ap                  <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxMonthsOfAccrual = nTestMaxQtyMonths )

         bErrThrown <- FALSE
         bErrThrown <- tryCatch(SimulateArrivalTimes( ap ), error=Error )
         expect_true( bErrThrown , label = "SimulateArrivalTimes (Invalid MaxMonthsOfAccrual): ")
    })

test_that( "SimulateArrivalTimes (max number of months): ",
    {
        vTestPatsPerMonth   <- c( 1, 10, rep( 12, 20 ) )
        nTestMaxQtyMonths   <- 20
        nTestMaxQtyPats     <- -1
        strTestDesc         <- "Accrual will continue until  max months of accrual is reached.  Using a variable accrual rate"

        ap                  <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxMonthsOfAccrual = nTestMaxQtyMonths )
        bTestPass           <- TRUE
        bErrThrown <- tryCatch(SimulateArrivalTimes( ap ), error=Error )
        for( i in 1:100 )  #Need to test multiple times
        {
             vTestAccTimes <- SimulateArrivalTimes( ap )
             if( vTestAccTimes[ length(vTestAccTimes )] > nTestMaxQtyMonths )
             {
                 bTestPass <- FALSE
                 break
             }
        }
        expect_true( bTestPass, label= "SimulateArrivalTimes (MaxMonthsOfAccrual)")
    })


#This should create an object that simulates until a max number of months and/or patient
test_that( "SimulateArrivalTimes (max number of months): ",
    {
        strTestDesc <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        nTestMaxQtyMonths       <- 10
        vTestPatsPerMonth       <- c(1,10,rep( 11, nTestMaxQtyMonths))
        nTestMaxQtyPats         <- 100
        strTestDesc             <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats, nMaxMonthsOfAccrual = nTestMaxQtyMonths )
        AddAccProSlotTests( ap, vTestPatsPerMonth, nTestMaxQtyPats, strTestDesc, nTestMaxQtyMonths)

        #With the current monthly accrual rates we should get the max number of patients before the max accrual time
        nQtyTest        <- 100
        nQtyTestsPassed <- 0
        for( i in 1:nQtyTest )
        {
            vTestAccTimes <- SimulateArrivalTimes( ap )
            if( length( vTestAccTimes ) <= nTestMaxQtyPats & vTestAccTimes[ length(vTestAccTimes )] <= nTestMaxQtyMonths  )
            {
                nQtyTestsPassed <- nQtyTestsPassed + 1
            }
        }
        expect_equal( nQtyTestsPassed, nQtyTest, label= "SimulateArrivalTimes (Max time and quantity of patients)")
    })


test_that( "SimulateArrivalTimes (max number of months): ",
    {
        strTestDesc <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        nTestMaxQtyMonths       <- 50
        vTestPatsPerMonth       <- c(1,10,rep( 11, nTestMaxQtyMonths))
        nTestMaxQtyPats         <- 100
        strTestDesc             <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats, nMaxMonthsOfAccrual = nTestMaxQtyMonths )

        #With the current monthly accrual rates we should get the max number of patients before the max accrual time
        nQtyTest        <- 100
        nQtyTestsPassed <- 0
        for( i in 1:nQtyTest )
        {
            vTestAccTimes <- SimulateArrivalTimes( ap )
            #With the current monthly accrual rates we should get the max number of patients before the max accrual time
            #so the next if statement has been updated to ==
            if( length( vTestAccTimes ) == nTestMaxQtyPats & vTestAccTimes[ length(vTestAccTimes )] <= nTestMaxQtyMonths  )
            {
                nQtyTestsPassed <- nQtyTestsPassed + 1
            }
        }
        expect_equal( nQtyTestsPassed, nQtyTest, label= "SimulateArrivalTimes (Max time and quantity of patients)")
    })



test_that( "SimulateArrivalTimes (max number of months): ",
    {
        strTestDesc <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        nTestMaxQtyMonths       <- 10
        vTestPatsPerMonth       <- rep(4,10)
        nTestMaxQtyPats         <- 100
        strTestDesc             <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats, nMaxMonthsOfAccrual = nTestMaxQtyMonths )

        #With the current monthly accrual rates we should get the max number of patients before the max accrual time
        nQtyTest        <- 500
        nQtyTestsPassed <- 0
        for( i in 1:nQtyTest )
        {
            vTestAccTimes <- SimulateArrivalTimes( ap )
            #With the current monthly accrual rates we should get the max number of patients before the max accrual time
            #so the next if statement has been updated to ==
            if( length( vTestAccTimes ) < nTestMaxQtyPats &
                    vTestAccTimes[ length(vTestAccTimes )] <= nTestMaxQtyMonths  &
                    ceiling(vTestAccTimes[ length(vTestAccTimes )]) <= nTestMaxQtyMonths )
            {
                nQtyTestsPassed <- nQtyTestsPassed + 1
            }
        }
        expect_equal( nQtyTestsPassed, nQtyTest, label= "SimulateArrivalTimes (Max time and quantity of patients)")
    })

test_that( "SimulateArrivalTimes (max number of months, single, constant rate): ",
{
    strTestDesc <- "Accrual will continue until  the max qty of months  of accrual is reached.  Using a FIXED accrual rate"

    nTestMaxQtyMonths       <- 24
    vTestPatsPerMonth       <- c(4)
    strTestDesc             <- "Accrual will continue until max months of accrual is reached.  Using a fixed rate"

    ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxMonthsOfAccrual = nTestMaxQtyMonths )

    #With the current monthly accrual rates we should get the max number of patients before the max accrual time
    nQtyTest        <- 100
    nQtyTestsPassed <- 0

    for( i in 1:nQtyTest )
    {
        vTestAccTimes <- SimulateArrivalTimes( ap )

        nQtyPatsMin   <- qpois( 0.001, vTestPatsPerMonth*nTestMaxQtyMonths )
        #With the current monthly accrual rates we should get the max number of patients before the max accrual time
        #so the next if statement has been updated to ==
        if( length( vTestAccTimes ) > nQtyPatsMin &
            vTestAccTimes[ length(vTestAccTimes )] <= nTestMaxQtyMonths  )
        {
            nQtyTestsPassed <- nQtyTestsPassed + 1
        }
    }
    expect_equal( nQtyTestsPassed, nQtyTest, label= "SimulateArrivalTimes (Max time )")
})
test_that( "SimulateArrivalTimes (max number of months): ",
    {
        strTestDesc <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        nTestMaxQtyMonths       <- 10
        vTestPatsPerMonth       <- vTestPatsPerMonth <- c(20)
        nTestMaxQtyPats         <- 100
        strTestDesc             <- "Accrual will continue until either the max qty of patients or max months  of accrual is reached, whichever comes first.  Using a variable accrual rate"

        ap                      <- NewAccrualProcess( vQtyPatsPerMonth = vTestPatsPerMonth, nMaxQtyPatients = nTestMaxQtyPats, nMaxMonthsOfAccrual = nTestMaxQtyMonths )

        #With the current monthly accrual rates we should get the max number of patients before the max accrual time
        nQtyTest        <- 100
        nQtyTestsPassed <- 0
        for( i in 1:nQtyTest )
        {
            tCounts <- rep(0,4)
            for( i in 1:100 )
            {
                vTestAccTimes <- SimulateArrivalTimes( ap )
                vTimes        <- floor( vTestAccTimes )
                tCounts       <- tCounts + table( vTimes)[1:4]
            }

            tCounts <- tCounts/100
        }
        bTestPass <- !( any( tCounts < 15) | any(tCounts > 25) )
        expect_true( bTestPass, label= "SimulateArrivalTimes expected number each month)")
    })

