##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#Originally cAnalysis, lDataAna, nFinalAnalysis

#' @name RunAnalysis.BayesianNormal
#' @title RunAnalysis.BayesianNormal
#' @description {This is the method used for running the analysis of a BayesianNormal normal model.}
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RunAnalysis.BayesianNormal.R}{View Code on GitHub} }
#' @export
RunAnalysis.BayesianNormal <- function( cAnalysis, lDataAna,  nISAAnalysisIndx, bIsFinalISAAnalysis, cRandomizer )
{
    vOut        <- lDataAna$vOut

    vBaseline   <- lDataAna$vBaseline
    vTrt        <- as.factor(lDataAna$vTrt)
    vTime       <- as.factor(lDataAna$vTime)
    vIND        <- lDataAna$vIND

    # Assuming 1 is the control treatment
    vUniqT      <- unique( lDataAna$vTrt )
    vUniqT      <- sort(vUniqT[ vUniqT != 1 ])
    nQtyTimePts <- length( lDataAna$vObsTime )
    dTimePt     <- lDataAna$vObsTime[ nQtyTimePts]

    #now We need to sample each arm and compare the samples to get the posterior probabilities we want
    nQtySamples <- 10000 # Make this an input in the analysis

    vOut     <- vOut[ vTime == lDataAna$vObsTime[ nQtyTimePts] ]
    vTrt     <- vTrt[ vTime == lDataAna$vObsTime[ nQtyTimePts] ]

    vYPlac   <- vOut[ vTrt == 1  ]
    lDataArm <- list( vY = vYPlac, nQtyPats = length( vYPlac ) )
    #print( paste( "Mean plac ", mean( vYPlac, na.rm =TRUE  )))
    vPostSampPlac <-  SamplePosterior( lDataArm, nQtySamples )




    #for( i in vUniqT )
    #{

    #TODO: This is a problem for multiple ISAs
    vY        <-  vOut[ vTrt == vUniqT[1]  ]

    #print( paste( "Mean trt ", mean( vY,na.rm=TRUE )))
    #print( paste( "vYTrt"))
    #print( paste( vY, collapse=", "))
    lDataArm  <- list( vY = vY, nQtyPats = length( vY ) )
    vPostSampTrt <- SamplePosterior( lDataArm,nQtySamples)
    #print(paste( "bPlacMinusTrt ",cAnalysis$bPlacMinusTrt ))


    lSamples <- list( vPostSampPlac = vPostSampPlac, vPostSampTrt = vPostSampTrt )
    lRet <- ComputePosteriorProbs( cAnalysis, nISAAnalysisIndx, bIsFinalISAAnalysis, lSamples  )

    lRet$cRandomizer <- cRandomizer
    ###### TODO(Kyle) - Test this function #############
    #}
    return( lRet )


}



#Note: This is a normal model and with conjugate prior so we could calculate the posterior (normal) but I did it this way so
#the framework would be there to add a more complex placebo model
SamplePosterior<- function( lData, nQtySamplesPerChain  )
{
    strModelFile <- paste0( path.package( "OCTOPUS"), "/BayesianModelFiles/NormalModel.txt")
    lDataJAGS <- list( vY = lData$vY, nQtyPats = lData$nQtyPats  )

    lInits    <- list( InitsNormalModel(), InitsNormalModel(), InitsNormalModel())  # Going to run 3 chains

    m         <- jags.model( strModelFile, lDataJAGS,lInits, n.chains=3, quiet = TRUE  )


    update(m,1000,progress.bar = "none",quiet = TRUE)
    mSamps <- coda.samples(m, c("dMu"  ), n.iter=nQtySamplesPerChain, quiet = TRUE,progress.bar = "none" )
    mSamps          <- rbind(mSamps[,][[1]],mSamps[,][[2]],mSamps[,][[3]])
    dPostMeanMu     <- mean( mSamps[,1])
    #print(paste( "Post Means  ", dPostMeanMu ))

    return( as.vector( mSamps ) )

}


InitsNormalModel <- function()
{
    return( list( dMu = rnorm(1, 0, 10), dTau = rgamma( 1, 0.1,.1) ))
}



