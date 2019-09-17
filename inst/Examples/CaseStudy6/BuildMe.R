##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


remove( list=ls() )

library( R2jags)
#The next line is just so this example will print the headers on the output file if you are running it locally.  Otherwise leave it commented out
#Sys.setenv(SGE_TASK_ID=1)

library( OCTOPUS)

####################################################################################################
#   Source the setup files
####################################################################################################
source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

cTrialDesign <- SetupTrialDesign( )
cSimulation  <- SetupSimulations( cTrialDesign, nQtyReps=5 )

#  As a general best practice it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object.
rm( list=(ls()[ls()!="cSimulation" ]))

gDebug <- FALSE

#Files specific for specific example
source( "AnalysisModelBayesianAR1.R")

# In this case study we utilize the MVNWithCovarite patient simulator from the base package

RunSimulation( cSimulation )


