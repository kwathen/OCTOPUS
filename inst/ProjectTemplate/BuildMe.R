
#### Description ################################################################################################
#   This project was created utilizing the OCTOPUS package located at https://kwathen.github.io/OCTOPUS/ .
#   There are several ReadMe comments in the document to help understand the next steps.
#   When this project was created from the template if an option was not supplied then there
#   may be variables with the prefix TEMP_ which will need to be updated.
################################################################################################### #

################################################################################################### #
#   ReadMe - Source the setup files                                                             ####
#   Rather than have the BuildMe.R create local variables, functions are used to avoid having
#   local variables. This helps to reduce the chance of a bug due to typos in functions.
#   The design files create functions that are helpful for setting up the trial design and
#   simulation design objects but are then removed as they are not needed during the simulations.
#   See below for the source command on the files that create new functions needed in the simulaiton.
#
#   TrialDesign.R - Contains a function to created the trial design option.
#       The main function in this file is SetupTrialDesign() which has a few arguments
#       to allow for options in the setup.  However, for finer control of the trial design see the
#       function and functions listed in TrialDesignFunctions.R
#
#   SimulationDesign.R - Helps to create the simulation design element.  The main function in this
#       file is SetupSimulations() which allows for some options to be sent in but for better control
#       and for adding scenarios please see this file.
#
#   After you run a simulation the file BuildSimulationResult.R shows an example of how to build
#   the results and create a basic graph with functions found in PostProcess.R
################################################################################################### #

remove( list=ls() )

# ReadMe - If needed, install the lattest copy of OCTOPUS using the remotes package
#remotes::install_github( "kwathen/OCTOPUS")

library( "OCTOPUS" )

# ReadMe - Useful statements for running on a grid such as linux based grid
if (interactive() || Sys.getenv("SGE_TASK_ID") == "") {
  #The SGE_TASK_ID is used if you are running on a linux based grid
    Sys.setenv(SGE_TASK_ID=1)
}

source( "Functions.R")           # Contains a function to delete any previous results
#CleanSimulationDirectories( )   # only call when you want to erase previous results

gdConvWeeksToMonths <- 12/52     # Global variable to convert weeks to months, the g is for global as it may be used
                                 # in functions



source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

mQtyPatientsPerArm <- matrix( c( TMP_MATRIX_DATA ), nrow=TMP_NROW, ncol = TMP_NCOL )
vISAStartTimes    <- TEMP_ISA_START_TIME

cTrialDesign <- SetupTrialDesign( strAnalysisModel   = "TEMP_ANALYSIS_MODEL",
                                  strBorrowing       = "TEMP_BORROWING",
                                  mPatientsPerArm    = mQtyPatientsPerArm,
                                  dQtyMonthsFU       = TMP_QTY_MONTHS_FU )

cSimulation  <- SetupSimulations( cTrialDesign,
                                  nQtyReps = TEMP_QTY_REPS,
                                  strSimPatientOutcomeClass = "TEMP_SIM_PATIENT_OUTCOME",
                                  vISAStartTimes = vISAStartTimes)

#Save the design file because we will need it in the RMarkdown file for processing simulation results
save( cTrialDesign, file="cTrialDesign.RData" )


#  As a general best practice, it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object and gDebug.
rm( list=(ls()[ls()!="cSimulation" ]))

gDebug <- FALSE

# Files specific for this project that were added and are not available in OCTOPUS.
# These files create new generic functions that are utilized during the simulation.
source( 'RunAnalysis.TEMP_ANALYSIS_MODEL.R' )
source( "SimPatientOutcomes.TEMP_SIM_PATIENT_OUTCOME.R")  # This will add the new outcome
source( "BinaryFunctions.R" )

# In this case study we utilize the MVNWithCovarite patient simulator from the base package
gnPrintDetail <-0
RunSimulation( cSimulation )



# If running on a single instance (computer) you could just increase the nQtyReps above and use code as is up to the RunSimulation() line.
# However, to "simulate" running this on the grid and getting multiple output files, combining them
# then creating an R markdown document

vSGETasks <- 2:20  # This will give us 100 reps (20 * 5)
for ( nSGETask in vSGETasks )
{
    gDebug <- FALSE
    Sys.setenv(SGE_TASK_ID= nSGETask )
    print( paste( "Simulating task ", nSGETask, " of ", length( vSGETasks ), "..."))
    RunSimulation( cSimulation )
}



