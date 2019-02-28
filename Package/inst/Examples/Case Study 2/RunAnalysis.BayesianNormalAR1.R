

#Note this version of BayesianNormalAR1 was modified to allow for multiple doses, see the for loop

RunAnalysis.BayesianNormalAR1 <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis )
{
    print( paste( "RunAnalysis.BayesianNormalAR1 "))

    vOut        <- lDataAna$vOut

    vBaseline   <- lDataAna$vBaseline
    vTrt        <- as.factor(lDataAna$vTrt)
    vTimeF      <- as.factor(lDataAna$vTime)
    vTime       <- lDataAna$vTime
    vIND        <- lDataAna$vIND

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    vUniqT      <- sort(vUniqT[ vUniqT != 1 ])
    nQtyTimePts <- length( lDataAna$vObsTime )
    dTimePt     <- lDataAna$vObsTime[ nQtyTimePts]

    #now We need to sample each arm and compare the samples to get the posterior probabilities we want
    nQtySamples <- 10000 # Make this an input in the analysis

    #vOut     <- vOut[ vTime == lDataAna$vObsTime[ nQtyTimePts] ]
    #vTrt     <- vTrt[ vTime == lDataAna$vObsTime[ nQtyTimePts] ]

    #Build the data for control, then sample the posterior
    vYPlac      <- vOut[ vTrt == 1  ]
    vINDPlac    <- vIND[ vTrt == 1 ]
    vTimePlac   <- vTime[ vTrt == 1 ]

    vTD         <- cbind(  vINDPlac ,vYPlac,vTimePlac )
    vTDOrd      <- vTD[ order( vTD[,1], vTD[, 3]),]

    #Make sure to use number of time points - 1 since change from baseline
    mYPlac      <- matrix( vTDOrd[,2], ncol = nQtyTimePts - 1, byrow=TRUE)  # The vector is sorted by ID, Time
    mYPlac      <- mYPlac[ is.na( mYPlac[,nQtyTimePts - 1] ) == FALSE,]     # Now this is only the completer

    vX          <- lDataAna$vObsTime

    nQtyPats    <- nrow( mYPlac )
    lDataArm    <- list( mY = mYPlac, nQtyPats = nQtyPats, vX = vX, nQtyTimePts= nQtyTimePts - 1 )

    mPostSampPlac   <- SamplePosteriorAR1( lDataArm, nQtySamples, title="Placebo" )
    vMeanPlac       <- CalcuateMeanAR1(  mPostSampPlac[,1], mPostSampPlac[,2], vX[nQtyTimePts - 1])

    gc()


    #Need to loop over each treatment (dose)

    lRetAll <- vector("list", length( vUniqT))   #The list that will be returned with a list for each dose
    nIndx   <- 1
    nQtyTrt <- length( vUniqT)
    for( i in 1:nQtyTrt )
    {
        #Create the data for vUniqT[i]
        vY          <-  vOut[ vTrt == vUniqT[i]  ]

        vYTrt       <- vOut[ vTrt == vUniqT[i] ]
        vINDTrt     <- vIND[ vTrt == vUniqT[i] ]
        vTimeTrt    <- vTime[ vTrt == vUniqT[i] ]

        vTD         <- cbind(  vINDTrt ,vYTrt,vTimeTrt )
        vTDOrd      <- vTD[ order( vTD[,1], vTD[, 3]),]

        #Make sure to use number of time points - 1 since change from baseline
        mYTrt       <- matrix( vTDOrd[,2], ncol = nQtyTimePts - 1, byrow=TRUE)  # The vector is sorted by ID, Time
        mYTrt       <- mYTrt[ is.na( mYTrt[,nQtyTimePts - 1] ) == FALSE,]       # Now this is only the completer

        nQtyPats    <- nrow( mYTrt )
        lDataArm    <- list( mY = mYTrt, nQtyPats = nQtyPats, vX = vX, nQtyTimePts= nQtyTimePts - 1 )

        #sample the posterior for this treatment
        mPostSampTrt    <- SamplePosteriorAR1( lDataArm, nQtySamples, title=paste( "Treatment ", i,sep ="") )
        vMeanTrt        <- CalcuateMeanAR1(  mPostSampTrt[,1], mPostSampTrt[,2], vX[nQtyTimePts - 1])

        #print( paste( "Mean plac ", mean( vMeanPlac ), "( ", mean(mYPlac[,6] ), ")  Mean Trt ", i , " ", mean( vMeanTrt ), " (", mean( mYTrt[,6]), ")" ))
        #print( paste( "Plac ", mean( mPostSampPlac[,1]), ", ", mean( mPostSampPlac[,2]), ", ", mean( mPostSampPlac[,3])))
        #print( paste( "Trt ", i, " ", mean( mPostSampTrt[,1]), ", ", mean( mPostSampTrt[,2]), ", ", mean( mPostSampTrt[,3])))

        lSamples <- list( vPostSampPlac = vMeanPlac, vPostSampTrt = vMeanTrt )
        lRetAll[[ i ]] <- ComputePosteriorProbs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples  )
        gc()
    }
    if( nQtyTrt == 1 )
    {
        lRetAll <- lRetAll[[1]]
    }



    return( lRetAll )


}



#Note: This is a normal model and with conjugate prior so we could calculate the posterior (normal) but I did it this way so
#the framework would be there to add a more complex placebo model
SamplePosteriorAR1<- function( lData, nQtySamplesPerChain, title =""  )
{
    strModelFile <- paste0( "BayesianNormalAR1.txt")
    #lDataJAGS <- list( vY = lData$vY, nQtyPats = lData$nQtyPats  )

    lInits    <- list( InitsNormalModelAR1(), InitsNormalModelAR1(), InitsNormalModelAR1())  # Going to run 3 chains

    m         <- jags.model( strModelFile, lData,lInits, n.chains=3, quiet = TRUE  )


    update(m,1000,progress.bar = "none",quiet = TRUE)
    mSamps <- coda.samples(m, c("alpha","beta", "gamma"), n.iter=nQtySamplesPerChain, quiet = TRUE,progress.bar = "none" )
    #plot( mSamps)
    #title( main= title)
    mSamps          <- rbind(mSamps[,][[1]],mSamps[,][[2]],mSamps[,][[3]])
    dPostMeanMu     <- mean( mSamps[,1])
    #print(paste( "Post Means  ", dPostMeanMu ))

    return(  mSamps )

}

InitsNormalModelAR1 <- function()
{
    list( alpha =runif( 1, -10,-.1),beta =runif( 1,0.01,.98), gamma=rnorm( 1, 0, 5),tau=runif( 1, 0.01, 5)  )

}

CalcuateMeanAR1<- function( vAlpha, vBeta,  dTime )
{
    # <-  exp(vAlpha) * (1 - exp( -vBeta * dTime ))
    dRet <- vAlpha + vBeta*dTime
    return( dRet)

}





