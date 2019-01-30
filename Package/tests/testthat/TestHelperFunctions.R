##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD Platform Trial Simulation License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


#############################################################################################################################.
#   Description                                                                                                         #####
#   This file contains helper functions for tests
#####   Developer(s): J. Kyle Wathen, PhD                                                                                  #####
#############################################################################################################################.


#The following function is used in conneciton with apply to get the pvalues of testing random samples
t.test.pval <-function( vSamp )
{
    t.test( vSamp )$p.value
}

# This funciton checks to make sure two lists have the same elements and the values match
# if bOrderMatters = FALSE then the items do not have to be in the same order.
AreListsEqual <- function( lList1, lList2, bOrderMatters = FALSE  )
{
    if( bOrderMatters )
    {
        bRet <- identical( lList1, lList2 )
    }
    else
    {
        bTest0 <- bTest1 <- bTest2 <- bTest3 <- bTest4 <- FALSE

        bTest0   <- all( is.list( lList1 ), is.list( lList2) )
        bLength  <- length( names( lList1 ) ) == length(  names( lList2 ) )

        if( bLength && bTest0 )
        {
            bTest1 <- all( names( lList1 ) %in% names( lList2 ) )
            bTest2 <- all( names( lList2 ) %in% names( lList1 ) )
            bTest3 <- all( unlist( lList1[ names( lList2 ) ] ) == unlist( lList2 ) )
            bTest4 <- all( unlist( lList2[ names( lList1 ) ] ) == unlist( lList1 ) )
        }
        bRet   <- all( bTest0, bTest1, bTest2, bTest3, bTest4)
    }
    return( bRet )
}
