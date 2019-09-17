##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


library( OCTOPUS )
strMain     <- "allmain.csv"
vStrISA     <- c("allISA1.csv" )
vISANumber  <- c(1)

simsAll     <- BuildResultsDataSet( strMain, vStrISA, vISANumber )  #This is a function in the PTS Package

save( simsAll, file="sims.RData")
