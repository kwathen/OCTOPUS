##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#' @name GetLSDiffGLS
#' @title GetLSDiffGLS
#' @description {This function will produce the estiamtes comparing to treatment 0 for the LME models that are fit.
#'   The Z values will match to SAS, which is what is used in decision making.
#'   This version requires glsFit requires glsFit to be a fit for gls}
#' @param glsFit: The results of the lme() model fit, required to have the variable TRT and Time
#' @param nTrt:   Integer value for the Treatment to compare to 0 (placebo)
#' @param nTime:  Integer value for the time variable that should be use.  For example 24, for 24
#               months even if additional time points have been observed by some patient.
#' @export
GetLSDiffGLS <-  function( glsFit, nTrt, nTime, dMAV, dTV, dLowerCI, dUpperCI, bPlacMinusTrt )
{
    strWhichTrt     <- paste("vTrt" , nTrt, sep = "")
    strWhichTime    <- paste("vTime", nTime, sep = "")
    strIntercept    <- "(Intercept)"
    vCoeff          <- coef( glsFit )
    if( !any(names(vCoeff ) == strWhichTime ) )  #The time is the "baseline" so no need to include the interaction
    {
        vVarNames       <- strWhichTrt
        vVarNamesPlac   <- strIntercept
        vVarNamesTrt    <- c( strIntercept, strWhichTrt )
    }
    else  #The time is not the baseline so we need to include the trt*time interaction in the estimate
    {
        vVarNames       <- c(strWhichTrt, paste(strWhichTrt, strWhichTime, sep = ":"))

        vVarNamesPlac   <- c( strIntercept, strWhichTime )
        vVarNamesTrt    <- c( vVarNamesPlac,  vVarNames )
    }
    if( any( names( vCoeff) == "vBaseline") )
    {
        vVarNames <- c( vVarNames)
        vVarNamesPlac <- c( vVarNamesPlac, "vBaseline")
        vVarNamesTrt <- c( vVarNamesTrt, "vBaseline")
    }
    nDOF        <- diff(unlist(glsFit$dims)[2:1])
    #This gives an estimate of Exp - Plac (if you want Plac - Exp use a -sum(...))
    #print( paste( vVarNames ))
    dEst        <- sum(vCoeff[ vVarNames ])
    if( bPlacMinusTrt )
        dEst <- dEst*-1
    #dEstPlac    <- sum( vCoeff[ vVarNamesPlac ])
    #dEstTrt     <- sum( vCoeff[ vVarNamesTrt])
    dSE         <- sqrt(sum(vcov(glsFit)[ vVarNames, vVarNames ]))
    dTStat      <- dEst/dSE
    dPVal       <- pt( dTStat, nDOF )

    #print( paste( "Low CI ", dLowerCI, " nDOF ", nDOF))

    dCILow <- dEst - abs(qt(dLowerCI, nDOF)) * dSE #/sqrt( nDOF )

    dCIUp <- dEst + qt(dUpperCI, nDOF) * dSE #/sqrt( nDOF )


    lRet        <- MakeDecisionBasedOnCI( dCILow, dCIUp, lAnalysis = list( dMAV = dMAV, dTV = dTV ))
    #nSuccess    <- 0
    #nFutility   <- 0
    #nPause      <- 0
    #print( dCILow)
    #print( dMAV)
    #if( dCILow > dMAV )
    #    nSuccess <- 1
    #else if( dCIUp < dFV )
    #    nFutility <- 1
    #else
    #    nPause <- 1

    vRet <- list(dEstimate = dEst, dStdErr = dSE, dTStat = dTStat, dPVal = dPVal, nDOF = nDOF,
                 dCILow = dCILow, dCIUp = dCIUp, nGO = lRet$nGo, nNoGo = lRet$nNoGo,
                 nPause = lRet$nPause )
    names(vRet)[5:10] <- c("nDOF", "dCILow", "dCIUp", "nGo", "nNoGo","nPause")
    return( vRet )
}



# function MCPMod.anal performs MCPMod analysis given data from one single trial
# dose: column name for dose
# resp: column name for response
# data: input data frame
# models: candidate models
# mod.sel: 1=best fitted model, 2=model average

FitMCPMod<-function(dose,resp,data,models,mod.sel, bPlacMinusTrt, dLowerCI, dUpperCI) {
    selModel<-switch(mod.sel,'AIC','aveAIC')
    mod.fit<-MCPMod(dose=dose,resp=resp,data=data,models=models,type='normal',selModel=selModel,
                    alpha=0.99999,critV=NULL,doseType='TD',Delta=1e-6,alternative='one.sided', na.action = na.omit)

    maxT<-max(mod.fit$MCTtest$tStat)

    if (class(mod.fit)!='list') {  # if all candidate models converge
        if (mod.sel==1) {
            pred.disc<-predict(mod.fit,doseSeq=dose,predType='effect-curve',se.fit=T)[[mod.fit$selMod]]
            pred.disc.value<-pred.disc[['fit']]
            pred.disc.se<-pred.disc[['se.fit']]


            pred.cont<-predict(mod.fit,doseSeq=0:max(dose),predType='effect-curve',se.fit=T)[[mod.fit$selMod]]
            pred.cont.value<-pred.cont[['fit']]
            pred.cont.se<-pred.cont[['se.fit']]
        } else if (mod.sel==2) {
            modWeights<-mod.fit$selMod
            pred.disc<-predict(mod.fit,doseSeq=dose,predType='effect-curve',se.fit=T)
            pred.disc.value<-do.call('cbind',lapply(pred.disc,'[[',1))%*%modWeights
            pred.disc.se<-sqrt(do.call('cbind',lapply(lapply(pred.disc,'[[',2),FUN=function(x) x^2))%*%(modWeights^2))
            pred.cont<-predict(mod.fit,doseSeq=0:max(dose),predType='effect-curve',se.fit=T)
            pred.cont.value<-do.call('cbind',lapply(pred.cont,'[[',1))%*%modWeights
            pred.cont.se<-sqrt(do.call('cbind',lapply(lapply(pred.cont,'[[',2),FUN=function(x) x^2))%*%(modWeights^2))
        }

        vPredDiffPlac <- pred.disc.value[ dose != 0 ]
        vSEDiff <- pred.disc.se[ dose != 0]
        if( bPlacMinusTrt )
            vPredDiffPlac <- -1* vPredDiffPlac
        vPredDiffPlacCILow <- vPredDiffPlac - abs( qnorm( dLowerCI ) )*( vSEDiff )
        vPredDiffPlacCIUpper <- vPredDiffPlac + abs(qnorm( dUpperCI ) )*( vSEDiff )
    } else {   # if at least one candidate model does not converge
        #print( paste( "At least one model did not converge"))
        print(mod.fit )
        pred.disc.value<-rep(NA,length(dose))
        pred.disc.se<-rep(NA,length(dose))
        pred.cont.value<-rep(NA,max(dose)+1)
        pred.cont.se<-rep(NA,max(dose)+1)
    }

    discret.dose<-data.frame(
        dose=dose,
        pred=pred.disc.value,
        pred.sd=pred.disc.se
    )

    lCIDiff <- list( vPredDiffPlac        = vPredDiffPlac,
                     vPredDiffPlacCILow   = vPredDiffPlacCILow,
                     vPredDiffPlacCIUpper = vPredDiffPlacCIUpper )

    continueous.dose<-data.frame(
        dose=0:max(dose),
        pred=pred.cont.value,
        pred.sd=pred.cont.se
    )

    return(list(maxT=maxT, discrete=discret.dose,  mod.fit = mod.fit, lCIDiff = lCIDiff ))
}

