## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library( kableExtra)
options(tinytex.verbose = TRUE)

## ----configGeneral,echo=FALSE,message=FALSE,warning=FALSE---------------------
#library(tidyverse)
#library(ggplot2)
#library( OCTOPUS )

source("PostProcess.R")  # This file contains the functionality to process the simulation results and plot


## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------
load( "cTrialDesign.RData")
load( "lTrialDesigns.RData") 

nQtyDesigns <- length( lTrialDesigns )
nQtyISAs    <- lTrialDesigns[[1]]$nQtyISAs
iDesign     <- 1
iISA        <- 1

strDesignDesc <- ""
for( iDesign in 1:nQtyDesigns )
{
    strDesignDesc <- paste( strDesignDesc,  "- **Design", iDesign," **\n" )
    for( iISA in 1:lTrialDesigns[[1]]$nQtyISAs )
    {
       
        strBorrow <- class( lTrialDesigns[[ iDesign ]]$cISADesigns[[ iISA ]]$cISAAnalysis )
        if( strBorrow == "NoBorrowing")
            strBorrow <- "No Borrowing"
        else
            strBorrow <- "Share Controls"
        vQtyPats  <- lTrialDesigns[[ iDesign ]]$cISADesigns[[ iISA ]]$vQtyPats
        
        strDesignDesc <- paste(strDesignDesc,    paste( "\t- *ISA", iISA, "*- Borrowing: ", strBorrow, ", # Patients on Control:", vQtyPats[1], ", # Patients on Treatment:", vQtyPats[2], "\n", sep="" ))
        
    }
    strDesignDesc <- paste( strDesignDesc, "\n \n \n")
}


## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------

# Now create the graphs based on the results####
lResults <- ProcessSimulationResults( )
#
# Get a subset of the results - We will use this for splitting scenarios into meaningful graphs
dfResSub <- lResults$mResults#[ lResults$mResults$scenario <=7, ]
vScenarioLabel <-c()
vScenarioLabel <- vScenarioLabel

PlotResultsWithIAInfo( dfResSub, vScenarioLabel = vScenarioLabel)

#PlotSubgroupResults( lResults$mSubgroupRes )



