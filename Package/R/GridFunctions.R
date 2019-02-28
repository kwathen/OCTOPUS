##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

##### TODO(Kyle) - Is this function needed? #####

#' @export
SetRunningEnvironment <- function(job.id, cmdArgs )
{
    #job.id <- as.integer(Sys.getenv("SGE_TASK_ID"))
    nGridIndex <--1
    #print( paste( "job ID ", job.id))
    if(is.na(job.id))
    {
        # The next block of code is just use so I can execute a batch file and run 4 copies instead of 1 and run the simulation in 1/4th the time.
        # Start Block
        #cmdArgs = commandArgs();

        nIndx <- length( cmdArgs )


        print( paste("Sim Set ", nIndx, " Command Args ", cmdArgs))
        if( nIndx > 2 ){
            nGridIndex <-cmdArgs[nIndx ]
            cat( "Sim Set i ", cmdArgs[nIndx ])
            strOutFile <- paste("out/out",cmdArgs[nIndx ], ".csv" , sep="" )
            #Previous version used cmdArgs - however if users environment is different this
            #caused crashes so it was removed
            #set.seed( as.integer(Sys.time())  + as.integer(cmdArgs[nIndx ]) )
            #set.seed( as.integer(Sys.time()) )
        }else
        {

            strOutFile <- "out/out.csv"
        }
        # End block
    }else
    {
        nGridIndex <- job.id

        if( nGridIndex == 1){
            strOutFile <- paste("out/1out",job.id, ".csv" , sep="" )  #Make name 1out1.csv so the headers can be printed and it is the first file to be appended

        }else{
            strOutFile <- paste("out/out",job.id, ".csv" , sep="" )
        }

    }
    return( list( strOutFile = strOutFile, nGridIndex = nGridIndex ) )
}
