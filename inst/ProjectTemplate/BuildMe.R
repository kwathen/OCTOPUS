  

remove( list=ls() )

# Example of how to install the OCTOPUS package from a tar.gz if needed
# install.packages("OCTOPUS_1.3.0.4.tar.gz", repos = NULL, type = "source", lib="RLibrary")

library( "OCTOPUS", lib.loc = "RLibrary" )

if (interactive() || Sys.getenv("SGE_TASK_ID") == "") {
  #The SGE_TASK_ID is used if you are running on a linux based grid
    Sys.setenv(SGE_TASK_ID=1)
}

source( "Functions.R")           # Contains a function to delete any previous results 
#CleanSimulationDirectories( )   # only call when you want to erase previous results

gdConvWeeksToMonths <- 12/52    #Global variable to convert weeks to months

################################################################################################### #
#   Source the setup files                                                                       ####
################################################################################################### #
source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

cTrialDesign <- SetupTrialDesign( strAnalysisModel = "TEMP_ANALYSIS_MODEL",
                                  strBorrowing     = "TEMP_BORROWING" )

cSimulation  <- SetupSimulations( cTrialDesign, nQtyReps=5 )

#Save the design file because we will need it in the RMarkdown file for processing simulation results
save( cTrialDesign, file="cTrialDesign.RData" )


#  As a general best practice it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object.
rm( list=(ls()[ls()!="cSimulation" ]))

gDebug <- FALSE

#Files specific for specific example
source( 'RunAnalysis.TEMP_ANALYSIS_MODEL.R' )
source( "SimPatientOutcomes.Binary.R")  # This will add the new outcome 
source( "BinaryFunctions.R" )
# In this case study we utilize the MVNWithCovarite patient simulator from the base package
gnPrintDetail <-0
RunSimulation( cSimulation )



# If running on a single instance (computer) you could just increase the nQtyReps above and use code as is up to the RunSimulation() line.  
# However, to "simulate" running this on the grid and getting multiple output files, combining them 
# then creating an R markdown document 

# Due to a bug in the BuildSimulationResultsDataSet( ) we need to run at lease nSGETask = 2
vSGETasks <- 2:20  # This will give us 100 reps (20 * 5)
for ( nSGETask in vSGETasks )
{
    gDebug <- FALSE
    Sys.setenv(SGE_TASK_ID= nSGETask )
    print( paste( "Simulating task ", nSGETask, " of ", length( vSGETasks ), "..."))
    RunSimulation( cSimulation )
}
