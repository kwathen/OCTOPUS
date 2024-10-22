##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name BuildSimulationResultsDataSet
#' @title BuildSimulationResultsDataSet
#' @description {This function is used to read the files in the out and ISAOut1, ISAOut2,..., directories and combine everything into one simulation dataset
#' that can be used for creating simulation results.  You should set the R working directory that contains the main, ISAOut1, ISOut2,...., directories.
#' This function is intended to streamline and replace the BuildResultsDataSet funciton.  }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/BuildSimulationResultsDataSet.R}{View Code on GitHub} }
#' @export
BuildSimulationResultsDataSet <- function( )
{


    strMainDir <- "out"
    print( paste( "Combining main output files in the out subdirectory..." ) )
    simsMain   <- CombineOutputFiles( "out" )

    print( paste( ".....Saving combined main output as simsMain.Rds"))
    #save( simsMain, file = "simsMain.RData")
    saveRDS( simsMain, file = "simsMain.Rds")
    simsCombined <- simsMain
    strISA <- "ISAOut1"
    iISA   <- 1

    while( file.exists( strISA ) )
    {
        print( paste( "Combining ISA", iISA, "output files in the", strISA, "subdirectory..." ) )
        simsISA    <- CombineOutputFiles( strISA )
        nQtyCol    <- ncol( simsISA)
        names( simsISA )[3:nQtyCol] <- paste( "ISA", iISA, names(simsISA)[3:nQtyCol] , sep="")

        #Save the object
        strObjName  <- paste( "simsISA", iISA, sep="" )
        strFileName <- paste( strObjName, ".Rds", sep="" )
        print( paste( ".....Saving combined ISA", iISA, "output as", strFileName ) )

        assign(  strObjName, simsISA )
        #save( list= strObjName, file= strFileName )
        saveRDS( simsISA, file= strFileName)

        print( paste( "Merging main results with ISA ", iISA, " results...", sep="" ))

        simsCombined <- merge( simsCombined, simsISA, by=c("GridIndex", "TrialID"))

        iISA   <- iISA + 1
        strISA <- paste( "ISAOut", iISA,sep="")
    }
    simsCombined <- simsCombined[order( simsCombined$iScen),]
    print( paste( "Saving combined main and ISA output as simsCombined.RData" ) )
    #save( simsCombined, file = "simsCombined.RData" )
    saveRDS( simsCombined, file = "simsCombined.Rds" )
    return( simsCombined )

}


CombineOutputFiles <- function( strDirectory )
{
    vFileNames  <- list.files( path=strDirectory, full.names=TRUE)
    nQtyFiles   <- length( vFileNames )
    dataset1    <- read.table( vFileNames[1], header=TRUE, sep=",")
    if( nQtyFiles > 1 )
    {
        vFileNames  <- vFileNames[-1]

        dataset     <- do.call("rbind", lapply(vFileNames , FUN = function(file) {
            read.table(file, header=FALSE, sep=",") }))

        colnames(dataset ) <- colnames(dataset1)

        dataset1 <- rbind( dataset1, dataset )

    }
    return( dataset1 )
}
