##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


remove( list=ls() )

library( R2jags)
#The next line is just so this example will print the headers on the output file if you are running it locally.  Otherwise leave it commented out
#Sys.setenv(SGE_TASK_ID=1)

library( PlatformTrialSimulator)

####################################################################################################
#   Source the setup files
####################################################################################################
source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")

#Files specific for specific example
source( "AnalysisModelBayesianAR1.R")


cTrialDesign <- SetupTrialDesign( )
cSimulation  <- SetupSimulations( cTrialDesign, nQtyReps=5 )

RunSimulation( cSimulation )


