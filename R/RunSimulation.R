##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#' @name RunSimulation
#' @title RunSimulation
#' @description {Generic function to execute the simulation of a list of scenarios contained in lScenarios.  }
#' @export
RunSimulation  <- function( lSimulation )
{
    UseMethod( "RunSimulation", lSimulation )
}

#' @name RunSimulation.default
#' @title RunSimulation.RunSimulation
#' @description {Generic function to execute the simulation of a list of scenarios contained in lScenarios.  }
#' @export
RunSimulation.default <- function( lSimulation )
{
    CheckGlobalVariables( )  #Function to check, and declare if needed, variables like gDebug, gnPrintDetail

    lSimulation <- SetupSimulation( lSimulation )
    nGridIndex  <- ifelse( is.null( lSimulation$nGridIndex ), 1, lSimulation$nGridIndex )
    strOutFile  <- ifelse( is.null( lSimulation$strOutputFile ), "default.csv", lSimulation$strOutputFile )

    iScen <- 1

    #lTmpScen <- get0( paste( "lScen", iScen, sep=""))

    #vRes <- vector() #vector( length=(20 + length(vPatsPerMonthPerSite1 )))
    mResAll <- vector()  # This matrix will be the accumulation overall scenarios
    nTrialID <- 1
    nQtyDesigns     <- length( lSimulation$SimDesigns )

    for( iDes in 1:nQtyDesigns )
    {

        cSimulation <- lSimulation$SimDesigns[[ iDes ]]
        nQtyScen    <- length( cSimulation$lScenarios )
        nQtySave    <- max( floor( nQtyScen/4 ), 1)  #Save output every 25%, taking the max incase < 4 scenarios
        for( iScen in 1:nQtyScen )
        {
           if(  gnPrintDetail >= 1 )
                print( paste( "Simulating Design ", iDes, " of ", nQtyDesigns, " Designs,  Scenario ", iScen, " of ", nQtyScen, " scenarios ..."))

            if( is.null( cSimulation$lScenarios[[ iScen ]]$nPrintDetail ) == TRUE )  #If it was not supplied, default to 0
            {
                cSimulation$lScenarios[[ iScen ]]$nPrintDetail <- 0
            }
            mResScen <- SimulateScenario( cSimulation$lScenarios[[ iScen ]], cSimulation$cTrialDesign )
            mResScen <- cbind( cSimulation$lScenarios[[ iScen ]]$nDesign, iScen, mResScen )
            mResAll <- rbind( mResAll, mResScen )
            if( iScen %% nQtySave  == 0 )
            {
                colnames( mResAll)[c(1)]<-c( "Design")
                if( nGridIndex == 1){
                    write.table( mResAll, strOutFile, sep=", ", append=FALSE, col.name=TRUE, row.names = FALSE, quote=FALSE )
                }
                else{
                    write.table( mResAll, strOutFile, sep=", ", append=FALSE, col.name=FALSE, row.names=FALSE)
                }

            }
        }
    }

    colnames( mResAll)[c(1)]<-c( "Design")
    if( nGridIndex == 1){
        write.table( mResAll, strOutFile, sep=", ", append=FALSE, col.name=TRUE, row.names = FALSE, quote=FALSE )
    }else{
        write.table( mResAll, strOutFile, sep=", ", append=FALSE, col.name=FALSE, row.names=FALSE)
    }
}

SetupSimulation <- function( lSimulation )
{

    if( is.null( lSimulation$SimDesigns ) == TRUE )
    {
        if( is.null( lSimulation$lScenarios) ==TRUE )
        {
            print(paste( "Adding the SimDesigns element"))

            #lSimulaiton was a single design, eg list( lScenarios,cTrialDesign) but we expected a list of these elements
            lSimulation <- list( SimDesigns = lSimulation )
        }
        else if( is.null( lSimulation$lScenarios) == FALSE )
        {
            print(paste( "Adding the SimDesigns element - option 2"))

            lSimulation <- list( SimDesigns = list(lSimulation) )

        }
    }

    nQtyDesigns <- length( lSimulation$SimDesigns )
    job.id      <- as.integer(Sys.getenv("SGE_TASK_ID"))
    cmdArgs     <- commandArgs()
    lRunInfo    <- SetRunningEnvironment(job.id, cmdArgs )

    lSimulation$strOutputFile   <- lRunInfo$strOutFile
    lSimulation$nGridIndex      <- lRunInfo$nGridIndex
    if( dir.exists("out") == FALSE )
    {
        dir.create("out")
    }

    if( dir.exists("log") == FALSE )
    {
        dir.create("log")
    }
    if( dir.exists("enrollment" ) == FALSE )
    {
        dir.create( "enrollment")
    }

    #create output directories for all ISA outputs - We assume all designs have the same
    #ISAs but this is not validate.
    #TODO: Validate that all designs have the same ISAs
    nQtyISA                 <- length( unique( lSimulation$SimDesigns[[1]]$cTrialDesign$vISALab) )
    #lSimulation$nQtyISA     <- nQtyISA

    #create a directory for each ISA detailed output files
    for( i in 1:nQtyISA )
    {
        strDirName <-  paste( "ISAOut", i, sep="")
        if( dir.exists( strDirName ) == FALSE )
        {
            dir.create( strDirName )
        }

    }

    #set the starting ID for each simulated trial.  A unique simulated trial ID is a combination of the nGridIndex and nTrialID
    iScen           <- 1
    nTrialIDStart   <- 1

    for( iDes in 1:nQtyDesigns )
    {
        cSim        <- lSimulation$SimDesigns[[ iDes]]
        nQtyScen    <- length( cSim$lScenarios )
        for( iScen in 1:nQtyScen )
        {
            cSim$lScenarios[[iScen]]$nTrialIDStart <- nTrialIDStart
            cSim$lScenarios[[iScen]]$nGridIndex    <- lSimulation$nGridIndex
            nTrialIDStart <- nTrialIDStart + cSim$lScenarios[[iScen]]$nQtyReps
        }
        lSimulation$SimDesigns[[ iDes ]] <- cSim
    }

    return( lSimulation )

}

