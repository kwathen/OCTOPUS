# This file will process the simulation results and create several .RData files containing ISA results and the trial results


library( OCTOPUS )
source( "PostProcess.R")

BuildSimulationResultsDataSet( )

lResults <- ProcessSimulationResults( )
dfResSub <- lResults$mResults   #[ lResults$mResults$scenario <=7, ]
PlotResults( dfResSub)  #Plots with Go, No Go, Pause but no IA specific info

PlotResultsWithIAInfo( dfResSub ) #Plots with Go, No Go, Pause but no IA specific info

