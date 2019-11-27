##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


context("CreateProject.R")


test_that("CreateProject - Test 1",
{
    gstrProjectDirectory <- paste( getwd(), "/Test1", sep="")
    gstrProjectName      <- "MyNewProject"
    strRes <- CreateProject( strProjectDirectory =  gstrProjectDirectory, strProjectName =  gstrProjectName,
                   strAnalysisName          = "MyBinaryAnalysis",
                   strSimPatientOutcomeName = "MyBinarySimulator",
                   strBorrowing             = "AllControls"  )
    bProjectCreated <- dir.exists( paste(gstrProjectDirectory, "/", gstrProjectName, sep=""))
    expect_true( bProjectCreated, label ="Directory created")
    unlink( paste( getwd(),"/Test1", sep=""), recursive = TRUE)
})
