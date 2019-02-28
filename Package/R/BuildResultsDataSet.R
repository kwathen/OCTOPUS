##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name BuildResultsDataSet
#' @title BuildResultsDataSet
#' @description {This function is used to read in the input files and created a combined output file.    }
#' @param strMainResultsFile The main output file produced when running a simulation.  Typically, it is found in the out/ subdirectory.
#' @param vStrISAResultsFile The file that contains the output from in ISA.  For ISA XXX it is typically the ISAXXX/ subdirectory.
#' @param vISANumber {An integer that will be used to prefix all of the ISA output variables names.  For example, if nISANumber =1, then all variables names
#' in the file will be prefixed with ISA1.}
#' @export
BuildResultsDataSet <- function( strMainResultsFile, vStrISAResultsFile, vISANumber  )
{
    print( paste( "Reading main simulation results..."))

    simsMain <- read.table( strMainResultsFile, header=TRUE, sep=",", quote="", colClasses= c("numeric") )

    print( paste( "Reading ISA 1 simulation results...") )

    simsISA <- read.table( vStrISAResultsFile[1], header=TRUE, sep=",", quote="", colClasses= c("numeric") )
    nQtyCol <- ncol( simsISA)
    names( simsISA )[3:nQtyCol] <- paste( "ISA", vISANumber[1], names(simsISA)[3:nQtyCol] , sep="")

    print( paste( "Merging main results with ISA 1 results..."))

    simsCombined <- merge( simsMain, simsISA, by=c("GridIndex", "TrialID"))

    nQtyISA <- length( vStrISAResultsFile)
    if( nQtyISA > 1 )
    {
        for( i in 2:nQtyISA )
        {
            print( paste( "Adding ISA ", i, " results..."))

            simsISA <- read.table( vStrISAResultsFile[i], header=TRUE, sep=",", quote="", colClasses= c("numeric") )
            nQtyCol <- ncol( simsISA)
            names( simsISA )[3:nQtyCol] <- paste( "ISA", vISANumber[i], names(simsISA)[3:nQtyCol] , sep="")
            simsCombined <- merge( simsCombined, simsISA, by=c("GridIndex", "TrialID"))


        }

    }
    simsCombined <- simsCombined[order( simsCombined$iScen),]
    return( simsCombined )
}

