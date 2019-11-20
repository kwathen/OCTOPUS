# This file will process the simulation results and create several .RData files containing ISA results and the trial results

library( OCTOPUS )
source( "PostProcess.R")

BuildSimulationResultsDataSet( )

lResults <- ProcessSimulationResults( )
dfResSub <- lResults$mResults   #[ lResults$mResults$scenario <=7, ]
PlotResults( dfResSub)


#ProbX1GrX2PlusDelta( 44.2, 0.8, 14.2, 86.8, 0.1 )
#ProbX1GrX2PlusDelta( 75.2, 0.8, 14.2, 86.8, 0.1 )
