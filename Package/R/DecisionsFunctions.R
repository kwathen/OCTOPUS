##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#The function will make the Go/NoGo decision for an outcome given the CI and the MAV, TV
# dLowerCI = lower limit of CI
# dUpper = upper limit of CI
# lAnaysis must have lAnalysis$dTV, and lAnalysis$dMAV
# Return a list of ( nSucces, nNoGo, nPause )

#' @export
MakeDecisionBasedOnCI <- function( dLowerCI, dUpperCI, lAnalysis )
{

    nGo <- nNoGo <- nPause <- 0
    if( is.na( dLowerCI) || is.na( dUpperCI ) )   #CI Could not but computed so a pause/continue is the decision
        return( list(nGo = 0, nNoGo = 0, nPause = 1) )
    if( dLowerCI > lAnalysis$dMAV )
        nGo <- 1
    else if( dLowerCI <= lAnalysis$dMAV && dUpperCI >= lAnalysis$dTV )
        nPause <- 1
    else
        nNoGo <- 1

    return(  list( nGo = nGo, nNoGo = nNoGo, nPause = nPause) )

}



