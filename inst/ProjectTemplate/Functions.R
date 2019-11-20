

################################################################################################### #
#   Function to clear the simulation directories
################################################################################################### #

CleanSimulationDirectories <- function()
{
    if( length( dir("enrollment") ) > 0)
        file.remove( paste("enrollment/", dir("enrollment"), sep="") )
    
    if( length( dir( "log")) > 0 )
        file.remove( paste("log/", dir( "log") , sep="") )
    
    if( length( dir( "out")) > 0 )
        file.remove( paste("out/", dir( "out") , sep="") )
    
    
    vISADirectories <- dir( pattern="ISAOut")
    nQtyDirectories <- length( vISADirectories ) 
    if( nQtyDirectories >= 1 )
    {
        for( i in 1:nQtyDirectories )
        {
            print( paste( "Cleaning directory: ", vISADirectories[ i ] ))
            if( length( dir( vISADirectories[ i ] ) ) > 0 )
               file.remove( paste(vISADirectories[ i ], "/", dir( vISADirectories[ i ] ), sep="") )
        }
    }
    else
    {
        print( paste( "No ISA directories to clean."))
    }
}