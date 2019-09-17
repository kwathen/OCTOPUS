##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#Note - If you want to run locally you can execute this file.

remove( list=ls() )

library( OCTOPUS )
library( rjags )
####################################################################################################.
#   Source the setup files
####################################################################################################.
source( "TrialDesign.R")
source( "SimulationDesign.R")
source( "TrialDesignFunctions.R")



cTrialDesign <- SetupTrialDesign( )
cSimulation  <- SetupSimulations( cTrialDesign, nQtyReps=1 )

#  As a general best practice it is good to remove all objects in the global environment just to make sure they are not inadvertently used.
#  The only object that is needed is the cSimulation object.
rm( list=(ls()[ls()!="cSimulation" ]))

gDebug <- FALSE

#Files specific to this Case study that are needed for simulation.
source( "RunAnalysis.BayesianNormalAR1.R")    #This file implements a Bayesian AR(1) analysis.


RunSimulation( cSimulation )


