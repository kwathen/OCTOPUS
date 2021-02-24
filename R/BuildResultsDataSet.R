##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' This version has been replaced by BuildSimulationResultsDataSet() that does more by defaults and saves .RData files

#' BuildResultsDataSet <- function( strMainResultsFile, vStrISAResultsFile, vISANumber  )
#' {
#'     print( paste( "Reading main simulation results..."))
#'
#'     simsMain <- read.table( strMainResultsFile, header=TRUE, sep=",", quote="", colClasses= c("numeric") )
#'
#'     print( paste( "Reading ISA 1 simulation results...") )
#'
#'     simsISA <- read.table( vStrISAResultsFile[1], header=TRUE, sep=",", quote="", colClasses= c("numeric") )
#'     nQtyCol <- ncol( simsISA)
#'     names( simsISA )[3:nQtyCol] <- paste( "ISA", vISANumber[1], names(simsISA)[3:nQtyCol] , sep="")
#'
#'     print( paste( "Merging main results with ISA 1 results..."))
#'
#'     simsCombined <- merge( simsMain, simsISA, by=c("GridIndex", "TrialID"))
#'
#'     nQtyISA <- length( vStrISAResultsFile)
#'     if( nQtyISA > 1 )
#'     {
#'         for( i in 2:nQtyISA )
#'         {
#'             print( paste( "Adding ISA ", i, " results..."))
#'
#'             simsISA <- read.table( vStrISAResultsFile[i], header=TRUE, sep=",", quote="", colClasses= c("numeric") )
#'             nQtyCol <- ncol( simsISA)
#'             names( simsISA )[3:nQtyCol] <- paste( "ISA", vISANumber[i], names(simsISA)[3:nQtyCol] , sep="")
#'             simsCombined <- merge( simsCombined, simsISA, by=c("GridIndex", "TrialID"))
#'
#'
#'         }
#'
#'     }
#'     simsCombined <- simsCombined[order( simsCombined$iScen),]
#'     return( simsCombined )
#' }

