## Test script for TrialMonitor.R
context("Test - InitializePatientList.R")

source("TestHelperFunctions.R")


#----------------------------------------------------------------------------------------------
#
# Test the function InitializePatientList( cTrialDesign )
#
# Note: I am using a variable name cTTrialDesign to make sure it does not appear accidentally anywhere
#       in the library and cause the code to work when it should not.
#----------------------------------------------------------------------------------------------

cTTrialDesign <- SetupTrialDesign2ISA( )
lRetPatList   <- InitializePatientList( cTTrialDesign )


test_that("Variables",
    {
        expect_equal( lRetPatList$vTrtLab , cTTrialDesign$vTrtLab )
        expect_equal( lRetPatList$vISALab , cTTrialDesign$vISALab  )
        expect_equal( lRetPatList$vCurrentQtyPatsISA , c(0, 0))
        expect_equal( lRetPatList$lPatOut[[1]]$vTrt, vector())
        expect_equal( lRetPatList$vQtyPatsArmISA, rep(0, length(cTTrialDesign$vTrtLab)))
    } )

test_that("Patient outcome list",
    {
        expect_equal( class( lRetPatList$lPatOut[[1]]$mOut1 ), class( cTTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[1]]))
        expect_equal( class( lRetPatList$lPatOut[[1]]$mOut2 ), class( cTTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[2]]))
        expect_equal( lRetPatList$lPatOut[[1]]$vObsTime1,  cTTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[1]]$vObsTime )
        expect_equal( lRetPatList$lPatOut[[1]]$vObsTime2,  cTTrialDesign$cISADesigns[[1]]$cISAAnalysis$vAnalysis[[2]]$vObsTime )
        expect_false( is.null( lRetPatList$lPatOut[[1]]$vObsTime1 ))
        expect_false( is.null( lRetPatList$lPatOut[[1]]$vObsTime2 ))

        expect_equal( class( lRetPatList$lPatOut[[2]]$mOut1 ), class( cTTrialDesign$cISADesigns[[2]]$cISAAnalysis$vAnalysis[[1]]))
        expect_equal( class( lRetPatList$lPatOut[[2]]$mOut2 ), class( cTTrialDesign$cISADesigns[[2]]$cISAAnalysis$vAnalysis[[2]]))
        expect_equal( lRetPatList$lPatOut[[2]]$vObsTime1,  cTTrialDesign$cISADesigns[[2]]$cISAAnalysis$vAnalysis[[1]]$vObsTime )
        expect_equal( lRetPatList$lPatOut[[2]]$vObsTime2,  cTTrialDesign$cISADesigns[[2]]$cISAAnalysis$vAnalysis[[2]]$vObsTime )
        expect_false( is.null( lRetPatList$lPatOut[[2]]$vObsTime1 ))
        expect_false( is.null( lRetPatList$lPatOut[[2]]$vObsTime2 ))
    })



#----------------------------------------------------------------------------------------------
#
# TODO: Test the SimulateSingleTrial.default
#
#----------------------------------------------------------------------------------------------
