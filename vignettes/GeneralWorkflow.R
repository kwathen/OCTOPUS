## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library( OCTOPUS )
#  strProjDir                 <- ""   # Since this is not a complete path, directory is created relative to working directory when this file is executed
#  strProjName                <- "ThreeISA"
#  strAnalysis                <- "BetaBinomial"
#  strSimPatOutcome           <- "Binary"
#  strBorrowing               <- "AllControls"
#  
#  # The matrix  mQtyPats contains the number of patients in each arm in each ISA.  Each row represents an ISA, each column is
#  # a treatment with the first column the number of patients in control.  The first ISA (row 1) has 50 patients on control
#  # 100 on treatment 2.  The second ISA (row 2) has 25 patients on control and 125 on treatment 2
#  # the third ISA has 25 on control and 125 on treatment.
#  mQtyPats                   <- matrix( c( 50, 100,
#                                           25, 125,
#                                           25, 125), byrow=TRUE, ncol=2 )
#  dQtyMonthsFU               <- 6
#  bCreateProjectSubdirectory <- TRUE
#  nQtyReps                   <- 10 # Small number of reps so you can run the default simulation quickly, increase as needed
#  vISAStartTimes             <- c( 0, 6, 12  )
#  
#  strResult <- CreateProject( strProjectDirectory        = strProjDir,
#                              strProjectName             = strProjName,
#                              strAnalysisName            = strAnalysis,
#                              strSimPatientOutcomeName   = strSimPatOutcome,
#                              strBorrowing               = strBorrowing,
#                              nQtyReps                   = nQtyReps,
#                              mQtyPatientsPerArm         = mQtyPats,
#                              dQtyMonthsFU               = dQtyMonthsFU,
#                              vISAStartTimes             = vISAStartTimes,
#                              bCreateProjectSubdirectory = bCreateProjectSubdirectory)
#  
#  cat( strResult)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  [1] "Simulating Design  1  of  3  Designs,  Scenario  1  of  4  scenarios ..."
#  [1] "Simulating Design  1  of  3  Designs,  Scenario  2  of  4  scenarios ..."
#  [1] "Simulating Design  1  of  3  Designs,  Scenario  3  of  4  scenarios ..."
#  [1] "Simulating Design  1  of  3  Designs,  Scenario  4  of  4  scenarios ..."
#  [1] "Simulating Design  2  of  3  Designs,  Scenario  1  of  4  scenarios ..."
#  [1] "Simulating Design  2  of  3  Designs,  Scenario  2  of  4  scenarios ..."
#  [1] "Simulating Design  2  of  3  Designs,  Scenario  3  of  4  scenarios ..."
#  [1] "Simulating Design  2  of  3  Designs,  Scenario  4  of  4  scenarios ..."
#  [1] "Simulating Design  3  of  3  Designs,  Scenario  1  of  4  scenarios ..."
#  [1] "Simulating Design  3  of  3  Designs,  Scenario  2  of  4  scenarios ..."
#  [1] "Simulating Design  3  of  3  Designs,  Scenario  3  of  4  scenarios ..."
#  [1] "Simulating Design  3  of  3  Designs,  Scenario  4  of  4  scenarios ..."

