#   This file contains the functions needed by the Binary GoNG versions.
#   All of these functions start with a . so they are not exported when the packages is created

# Generic functions to calculate the posterior probabilities of interest
ProbX1GrX2PlusDelta <- function(dA1,dB1,dA2,dB2, dDelta)
{
    ## The function calculates Pr( X1 > X2 + dDelta)
    ## Note: Pr( X1 > X2 + dDelta) = Pr( X2 < X1 - dDelta ) which is what is calculated so we can use pbeta
    #Note: Pr( X1 - X2 < dDelta ) = Pr( X2 > X1 - dDelta)

    res <- integrate(fBetaIneqCalc,0.001,0.999, dA1 = dA1, dB1 = dB1, dA2 = dA2, dB2 = dB2, dDelta)
    res$value
}

#Helper functions
fBetaIneqCalc <- function(x, dA1, dB1, dA2, dB2, dDelta ){dbeta( x, dA1, dB1 )* (pbeta(x-dDelta,dA2,dB2)) }

#This function calculate Pr( x2 > x1 + delta1) and Pr( x2 > x1 + delta2)
CalcPosteriorProbsBinom <- function( a1, b1, a2, b2, delta1, delta2)
{
    # Calculate Pr( p_E - p_S > dDelta1| data) = Pr( p_E > p_S+ dDelta1 )
    dPostProb1 <- ProbX1GrX2PlusDelta( a2, b2, a1, b1, delta1)

    #Calculate Pr( p_E - p_S > dDelta2 | data ) = Pr( p_E > p_S + dDelta2 | data )
    dPostProb2 <- ProbX1GrX2PlusDelta( a2, b2, a1, b1, delta2)

    return( list( dPPGrtDelta1=dPostProb1, dPPGrtDelta2 = dPostProb2) )
}

# Calculate the posterior credible interval for p2 - p1 from dLB, dUB
#dA1, dB1, dA2, dB2, dDelta1, dDelta2, dLB, dUB
CalcPosteriorCIBinom <- function( cCalc )
{
    dA1 <- cCalc$dPriorA1 + cCalc$lData$x1
    dB1 <- cCalc$dPriorB1 + cCalc$lData$y1
    dA2 <- cCalc$dPriorA2 + cCalc$lData$x2
    dB2 <- cCalc$dPriorB2 + cCalc$lData$y2
    vP1 <- rbeta( 10000, dA1, dB1 )
    vP2 <- rbeta( 10000, dA2, dB2 )
    vD  <- vP2 - vP1 #vP1 - vP2
    vCI <- quantile( vD, c( cCalc$dLB, cCalc$dUB))

    return( list( vCI = vCI ) )
}


