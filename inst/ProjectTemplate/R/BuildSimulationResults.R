# This file will process the simulation results and create several .RData files containing ISA results and the trial results


library( OCTOPUS )
source( "R/PostProcess.R")

# Step 1 - Combine the results into a data frame with 1 row per simulated trial ####
dfSimsCombined <- BuildSimulationResultsDataSet( )

lResults <- ProcessSimulationResults( )
dfResSub <- lResults$mResults   #[ lResults$mResults$scenario <=7, ]
PlotResults( dfResSub)  #Plots with Go, No Go, Pause but no IA specific info

PlotResultsWithIAInfo( dfResSub ) #Plots with Go, No Go, Pause but no IA specific info


# Example of timeline plots ####
source( "R/PlatformTrialPlots.R")
library( lubridate)


# When comparing timelines for multiple trial it can be useful for having the end date and make the comparison more meaningful
dStart         <- ymd( "2025/01/01")
dEnd           <- ymd( "2027/06/01")
vUniqueDesigns <- unique( dfResSub$design )

TimeLinePlot(dfResSub, dStart,dEnd, vDesigns = vUniqueDesigns[1], strTitle = "Design 1, Scenario 1", nScenario = 1 )
