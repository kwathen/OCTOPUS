
#### Description ################################################################################################
#   This project was created utilizing the OCTOPUS package located at https://kwathen.github.io/OCTOPUS/ .
#   There are several ReadMe comments in the document to help understand the next steps.
#   When this project was created from the template if an option was not supplied then there
#   may be variables with the prefix TEMP_ which will need to be updated.
################################################################################################### #

################################################################################################### #
#   ReadMe - Source the set up files                                                             ####
#   Rather than have the BuildMe.R create local variables, functions are used to avoid having
#   local variables. This helps to reduce the chance of a bug due to typos in functions.
#   The design files create functions that are helpful for setting up the trial design and
#   simulation design objects but are then removed as they are not needed during the simulations.
#   See below for the source command on the files that create new functions needed in the simulation.
#
#   TrialDesign.R - Contains a function to created the trial design option.
#       The main function in this file is SetupTrialDesign() which has a few arguments
#       to allow for options in the set up.  However, for finer control of the trial design see the
#       function and functions listed in TrialDesignFunctions.R
#
#   SimulationDesign.R - Helps to create the simulation design element.  The main function in this
#       file is SetupSimulations() which allows for some options to be sent in but for better control
#       and for adding scenarios please see this file.
#
#   BuildSimulationResult.R After you run a simulation this file provides an example of how to build
#   the results and create a basic graph with functions found in PostProcess.R
#
#   PostProcess.R - Basic graphing functions.
#
#   To add Interim Analysis - see Examples in TrialDesign.R
#
#   This file is setup to have 3 design options.  All designs have the same number of ISAs
#       Design 1 - Utilizes the number of patients that was used to create this file and has no interim analysis
#       Design 2 - Doubles the number of patients in each ISA
#       Design 3 - Same as design 1 but includes an interim analysis when half the patients have the desired follow-up
#
#   If the files RunAnalysis.XXX.R or SimPatientOutcomes.XXX.R are included they are working example
#   where the patient outcome is binary and the analysis is a Bayesian model.  You will need to update this per
#   your use case.
################################################################################################### #

# It is a good practice to clear your environment before building your simulation/design object then
# then clean it again before you run simulations with only the minimum variables need to avoid potential
# misuse of variables
remove( list=ls() )

# ReadMe - If needed, install the latest copy of OCTOPUS using the remotes package
#remotes::install_github( "kwathen/OCTOPUS")


library( "OCTOPUS" )
library( dplyr )

# ReadMe - Useful statements for running on a grid such as linux based grid
if (interactive() || Sys.getenv("SGE_TASK_ID") == "") {
    #The SGE_TASK_ID is used if you are running on a linux based grid
    Sys.setenv(SGE_TASK_ID=1)
}

source( "R/Functions.R")           # Contains a function to delete any previous results
#CleanSimulationDirectories( )   # only call when you want to erase previous results

gdConvWeeksToMonths <- 12/52     # Global variable to convert weeks to months, the g is for global as it may be used
# in functions


# Source any files needed to create the simulation objects ####
source( "R/TrialDesign.R")
source( "R/SimulationDesign.R")
source( "R/TrialDesignFunctions.R")

dQtyMonthsFU       <- TMP_QTY_MONTHS_FU
dTimeOfOutcome     <- 1 # The time at which an outcome is observed, in months.

mQtyPatientsPerArm <- matrix( c( TMP_MATRIX_DATA ), nrow=TMP_NROW, ncol = TMP_NCOL, byrow=TRUE )
vISAStartTimes     <- TEMP_ISA_START_TIME
nQtyReps           <- TEMP_QTY_REPS # How many replications to simulate each scenario

dMAV               <- 0
vPUpper            <- c( 1.0 )
vPLower            <- c( 0.0 )
dFinalPUpper       <- 0.99
dFinalPLower       <- 0.01

# If you need to add additional information into your analysis then you can supply a list for each ISA. lAnalysis is NOT required.
# Example: In the example analysis code, prior alpha and beta are required for each arm in the analysis.
# This example adds a vPriorA and a vPriorB to the analysis object for each analysis
# lAnalysis is a list of list.   lAnalysis must have one element for each ISA.  Each ISA list can contain additional parameters for the analysis
vPriorA      <- c( 0.2, 0.2 )
vPriorB      <- c( 0.8, 0.8 )
lCommonPrior <- list( vPriorA = vPriorA, vPriorB = vPriorB )
lAnalysis    <- replicate( TMP_NROW, lCommonPrior, simplify = FALSE)


#Setup simulation aspects - This is just an example, see how to access these variables in the SimulatePatientOutcomes file in the R directory
#   dfScenarios - A dataframe with a row for each scenario*ISA. The dataframe must have columns named Scenario and ISA.  Each additional column is added
#                 to the simulation object.  Each scenario must specify a row for each ISA.
#                 Example:
#                   dfScenarios <- data.frame( Scenario = c(1,1,2,2), ISA = c(1,2,1,2), ProbRespCtrl = c(0.2, 0.2, 0.2, 0.2), ProbRespExp =c( 0.2,0.2,0.4,0.4))
#   Would create 2 scenarios where scenario 1 would have th ProbResCtrl =0.2 ProbRespExp = 0.2 for both ISAs and scenario 2 would have ProbResCtrl =0.2 ProbRespExp = 0.4 for both ISAs

# Setup Simulation Scenarios ####
dfScenarios <- data.frame( Scenario = integer(), ISA  = integer(), ProbRespCtrl = double(), ProbRespExp = double() )
dfScenarios <- dfScenarios %>%  dplyr::add_row( Scenario = 1, ISA = 1:TMP_NROW, ProbRespCtrl = 0.2, ProbRespExp = 0.2 ) %>%
    dplyr::add_row( Scenario = 2, ISA = 1:TMP_NROW, ProbRespCtrl = 0.2, ProbRespExp = 0.4 )
vQtyOfPatsPerMonth <-  TEMP_PATIENTS_PER_MONTH

# Design Option 1 ####
cTrialDesign <- SetupTrialDesign( strAnalysisModel   = "TEMP_ANALYSIS_MODEL",
                                  strBorrowing       = "TEMP_BORROWING",
                                  mPatientsPerArm    = mQtyPatientsPerArm,
                                  dQtyMonthsFU       = dQtyMonthsFU,
                                  dMAV               = dMAV,
                                  vPUpper            = vPUpper,
                                  vPLower            = vPLower,
                                  dFinalPUpper       = dFinalPUpper,
                                  dFinalPLower       = dFinalPLower,
                                  dTimeOfOutcome     = dTimeOfOutcome,
                                  lAnalysis          = lAnalysis )

cSimulation  <- SetupSimulations( cTrialDesign,
                                  nQtyReps                  = nQtyReps,
                                  strSimPatientOutcomeClass = "TEMP_SIM_PATIENT_OUTCOME",
                                  vISAStartTimes            = vISAStartTimes,
                                  vQtyOfPatsPerMonth        = vQtyOfPatsPerMonth,
                                  nDesign                   = 1,
                                  dfScenarios               = dfScenarios )

nQtyDesigns    <- 1  # This is an increment that will be used to keep track of designs as they are added

# Start building a list of trial designs to be saved
lTrialDesigns <- list( cTrialDesign1 = cTrialDesign )

#Save the design file because we will need it in the RMarkdown file for processing simulation results
saveRDS( cTrialDesign, file="cTrialDesign1.Rds" )

# Additional Designs ####

# If you do not want to add additional designs begin commenting out or deleting this section of code
# Beginning of multiple design options - This code block could be removed.  It provides an example of
# how to add additional designs such as sample sizes, adding interim analysis or changing analysis methods.  This approach allow the
# graphs to display design options side-by-side.

# Design Option 2 ####
# Example 1 (Design Option 2): Additional Sample Size (more designs )
# Try another sample size double the original - To show the value of a larger sample size.

nQtyDesigns     <- nQtyDesigns + 1
cTrialDesignTmp <- SetupTrialDesign( strAnalysisModel   = "TEMP_ANALYSIS_MODEL",
                                     strBorrowing       = "TEMP_BORROWING",
                                     mPatientsPerArm    = 2*mQtyPatientsPerArm,
                                     dQtyMonthsFU       = dQtyMonthsFU,
                                     dMAV               = dMAV,
                                     vPUpper            = vPUpper,
                                     vPLower            = vPLower,
                                     dFinalPUpper       = dFinalPUpper,
                                     dFinalPLower       = dFinalPLower,
                                     dTimeOfOutcome     = dTimeOfOutcome,
                                     lAnalysis          = lAnalysis )


cSimulationTmp <- SetupSimulations( cTrialDesignTmp,
                                    nQtyReps                  = nQtyReps,
                                    strSimPatientOutcomeClass = "TEMP_SIM_PATIENT_OUTCOME",
                                    vISAStartTimes            = vISAStartTimes,
                                    vQtyOfPatsPerMonth        = vQtyOfPatsPerMonth,
                                    nDesign                   = nQtyDesigns,
                                    dfScenarios               = dfScenarios)

cSimulation$SimDesigns[[ nQtyDesigns ]] <- cSimulationTmp$SimDesigns[[1]]

# Save Rds for this design
saveRDS( cTrialDesignTmp, file = paste0( "cTrialDesign", nQtyDesigns, ".Rds" ) )


# Add design to the list of designs
lTrialDesigns[[ paste0( "cTrialDesign", nQtyDesigns )]] <- cTrialDesignTmp


# Design Option 3 ####

# Example 2 (Design Option 3): Add interim analysis ( IA ) where the IA is performed at half the patients.
# At the IA if the posterior probability that the difference between treatment and control is is greater than MAV is greater than 0.99
# then a Go decision is reached, if it is less than 0.01 a No Go decision is reached, otherwise the trial continues to the end.
# At the end of the trial if the posterior probability that the difference between treatment and control is is greater than MAV is greater than 0.8
# then a Go decision is reached, if it is less than 0.1 a No Go decision
nQtyDesigns       <- nQtyDesigns + 1
mMinQtyPats       <- cbind( floor(apply( mQtyPatientsPerArm , 1, sum )/2),  apply( mQtyPatientsPerArm , 1, sum ) )
vMinFUTime        <- rep( dQtyMonthsFU, ncol( mMinQtyPats) )
dQtyMonthsBtwIA   <- 0

vPUpper           <- c( 0.99,0.99 )
vPLower           <- c( 0.01, 0.01 )
dFinalPUpper      <- 0.8
dFinalPLower      <- 0.1

cTrialDesignTmp <- SetupTrialDesign( strAnalysisModel   = "TEMP_ANALYSIS_MODEL",
                                     strBorrowing       = "TEMP_BORROWING",
                                     mPatientsPerArm    = mQtyPatientsPerArm,
                                     mMinQtyPat         = mMinQtyPats,
                                     vMinFUTime         = vMinFUTime,
                                     dQtyMonthsBtwIA    = dQtyMonthsBtwIA,
                                     dMAV               = dMAV,
                                     vPUpper            = vPUpper,
                                     vPLower            = vPLower,
                                     dFinalPUpper       = dFinalPUpper,
                                     dFinalPLower       = dFinalPLower,
                                     dTimeOfOutcome     = dTimeOfOutcome,
                                     lAnalysis          = lAnalysis    )

cSimulationTmp <- SetupSimulations( cTrialDesignTmp,
                                    nQtyReps                  = nQtyReps,
                                    strSimPatientOutcomeClass = "TEMP_SIM_PATIENT_OUTCOME",
                                    vISAStartTimes            = vISAStartTimes,
                                    vQtyOfPatsPerMonth        = vQtyOfPatsPerMonth,
                                    nDesign                   = nQtyDesigns,
                                    dfScenarios               = dfScenarios )

cSimulation$SimDesigns[[nQtyDesigns]] <- cSimulationTmp$SimDesigns[[1]]

# Save Rds for this design
saveRDS( cTrialDesignTmp, file = paste0( "cTrialDesign", nQtyDesigns, ".Rds" ) )

# Add design to the list of designs
lTrialDesigns[[ paste0( "cTrialDesign", nQtyDesigns )]] <- cTrialDesignTmp



#Often it is good to keep the design objects for utilizing in a report
saveRDS( lTrialDesigns, file="lTrialDesigns.Rds")

# End of multiple design options - stop deleting or commenting out here if not utilizing example for multiple designs.

#  As a general best practice, it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object and gDebug, gnPrintDetail.
rm( list=(ls()[ls()!="cSimulation" ]))



#################################################################################################### .
# Setup of parallel processing                                                                  ####
#################################################################################################### .

library( "foreach")
library( "parallel" )
library( "doParallel" )
library( "iterators" )


# Source files needed for simulation, eg new analysis, patient simulation ect ####

# IMPORTANT NOTE: If you add more files that are sourced and needed in the simulation then make to source them in the RunParallelSimulations.R function####
source( "R/RunParallelSimulations.R" ) # This file has a version of simulations that utilize more cores

# Use 1 less than the number of cores available
nQtyCores  <- max( detectCores() - 1, 1 )

# The nStartIndex and nEndIndex are used to index the simulations and hence the output files see the RunParallelSimulations.R file
# for more details
RunParallelSimulations( nStartIndex = 1, nEndIndex = nQtyCores,  nQtyCores, cSimulation )


