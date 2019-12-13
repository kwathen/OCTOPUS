##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.


# This file contains the necessary functions to simulate the comparison of consecutive POC trials and a Platform trial
# with an ISA for each POC

#' @name PlotPlatformRecruitment
#' @title PlotPlatformRecruitment
#' @description {This function creates a plot of recruitment comparing a platform with 2 ISAs and 2 consecutive POC studies.
#' Mainly used by the Shiny app execute by calling RunExample( "CompareRecruitment" ). }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RecruitmentComparitor.R}{View Code on GitHub} }
#' @export
PlotPlatformRecruitment <- function( nQtyReps, nMaxQtyPats, vPatsPerMonthPerSite1,
                                     dDelayToStartPOC, dDelayBetweenTrialsPOC,vQtySitesPOC,
                                     dDelayToStartPlat,dDelayBetweenTrialsPlat, vQtyOfSitesPlat1, vQtyOfSitesPlat2  )
{
    vQtyOfSitesPOC1  <- vQtySitesPOC
    vQtyOfSitesPOC2  <- vQtyOfSitesPOC1
    vQtyOfSitesPOC2  <- ifelse( vQtyOfSitesPOC2 > 35, 35, vQtyOfSitesPOC2 )



    #For the POC sending the same vPatsPerMonthPerSite1 for POC1 and 2 because there is no savings in the second

    lSimResPOC <- SimulateAccrual( nQtyReps, nMaxQtyPats, dDelayBetweenTrialsPOC,
                                   vPatsPerMonthPerSite1, vQtyOfSitesPOC1, vPatsPerMonthPerSite1,
                                   vQtyOfSitesPOC1, dDelayToStartPOC )





    lSimResPlat <- SimulateAccrual( nQtyReps, nMaxQtyPats, dDelayBetweenTrialsPlat,
                                    vPatsPerMonthPerSite1, vQtyOfSitesPlat1, vPatsPerMonthPerSite1,
                                    vQtyOfSitesPlat2, dDelayToStartPlat   )


    return( SummarizeSimple( lSimResPOC, lSimResPlat, "" ) )

}

#' @name ggPlotPlatformRecruitment
#' @title PlotPlatformRecruitment
#' @description {This function creates a plot of recruitment comparing a platform with 2 ISAs and 2 consecutive POC studies.
#' Mainly used by the Shiny app execute by calling RunExample( "CompareRecruitment" ).  This version returns a ggplot2 object. }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RecruitmentComparitor.R}{View Code on GitHub} }
#' @export
ggPlotPlatformRecruitment <- function( nQtyReps, nMaxQtyPats, vPatsPerMonthPerSite1,
                                     dDelayToStartPOC, dDelayBetweenTrialsPOC,vQtySitesPOC,
                                     dDelayToStartPlat,dDelayBetweenTrialsPlat, vQtyOfSitesPlat1, vQtyOfSitesPlat2  )
{
    vQtyOfSitesPOC1  <- vQtySitesPOC
    vQtyOfSitesPOC2  <- vQtyOfSitesPOC1
    vQtyOfSitesPOC2  <- ifelse( vQtyOfSitesPOC2 > 35, 35, vQtyOfSitesPOC2 )



    #For the POC sending the same vPatsPerMonthPerSite1 for POC1 and 2 because there is no savings in the second

    lSimResPOC <- SimulateAccrual( nQtyReps, nMaxQtyPats, dDelayBetweenTrialsPOC,
                                   vPatsPerMonthPerSite1, vQtyOfSitesPOC1, vPatsPerMonthPerSite1,
                                   vQtyOfSitesPOC1, dDelayToStartPOC )





    lSimResPlat <- SimulateAccrual( nQtyReps, nMaxQtyPats, dDelayBetweenTrialsPlat,
                                    vPatsPerMonthPerSite1, vQtyOfSitesPlat1, vPatsPerMonthPerSite1,
                                    vQtyOfSitesPlat2, dDelayToStartPlat   )


    return( ggSummarizeSimple( lSimResPOC, lSimResPlat ) )

}



ComputeMonthlyAccrual <- function( vPatsPerMonthPerSite, vQtyOfSites )
{
    #This function improves previous version because it assumes the ramp up in Pat/Month/Site is applied to each site as it opens.
    #Ex:
    #   vPatsPerMonthPerSite = c(2,4,8,16)  # This implies the first month a site is open it is expected to enroll 2, 2nd month 4, 3rd month 8, 4th month 16 patient
    #   vQtyOfSites = c( 1,11,31,61)  # This implies that 1 site opened month 1, 10 sites opened month 2, 20 opened month 3, and 30 opened month 4
    #In month 1: 1 site opens and enrolls 2
    #                                 # In month 2: 10 sites that open and are expected to enroll 2 patients in month 2 of the trial, and the site that opened in month 1 expected 4 patients: total = 24
    # In month 3: the 20 sites opened are expected to enroll 2,  10 sites are expected to enroll 4, 1 site enroll 8: Total = 88
    # In month 4: the 30 sites that open are expected to enroll 2
    #             20 sites are expected to enroll 4, 10 sites are expected to enroll 8 and 1 site enroll 16: Total = 236
    # In month 5: 30 sites expected to enroll 4, 20 sites enroll 8, 10 sites enroll 16 and  1 site enroll 16: Total 456
    # In month 6: 30 sites expected to enroll 8, 20 sites enroll 16, 10 sites enroll 16 and  1 site enroll 16: Total = 736
    # In month 7 (and after): all sites are at 16 thus total accrual 16*61=976
    #
    vSitesOpened <- c( vQtyOfSites[1], diff( vQtyOfSites ) )

    nLenQtyPats <- length( vPatsPerMonthPerSite )
    vPatsPerMonthPerSiteExt <- c( vPatsPerMonthPerSite, rep( vPatsPerMonthPerSite[ nLenQtyPats ], nLenQtyPats ) )


    mPatsPerMonth <- matrix( vPatsPerMonthPerSiteExt, byrow=TRUE, nrow=1)
    nLen <- length( vSitesOpened )

    for( i in 2:nLen )
    {
        vRow <- c( rep(0, i-1), vPatsPerMonthPerSiteExt )[1:length(vPatsPerMonthPerSiteExt)]
        mPatsPerMonth <- rbind( mPatsPerMonth, vRow )
    }
    vTotalAccrual <- vSitesOpened %*% mPatsPerMonth
    #Repeat the last element to make sure the vector is long enough for any accrual amount
    vTotalAccrual <- c(vTotalAccrual, rep( vTotalAccrual[ length( vTotalAccrual)], 1000))
    return( vTotalAccrual)

}

#' @name SimulateAccrual
#' @title SimulateAccrual
#' @description {This function simulates the accural for 2 ISAs.  Mainly used by the Shiny app execute by calling RunExample( "CompareRecruitment" ). }
#' @seealso { \href{https://github.com/kwathen/OCTOPUS/blob/master/R/RecruitmentComparitor.R}{View Code on GitHub} }
#' @export
SimulateAccrual <- function( nQtyReps,  nMaxQtyPats, dDelayBetweenTrials, vPatsPerMonthPerSite1,
                             vQtyOfSites1, vPatsPerMonthPerSite2, vQtyOfSites2, dDelayToStart)
{    #Make sure the QtyOfSites and PatsPerMonthPerSite have equal length
    dNewSites   <- diff(vQtyOfSites1)
    nQtyRampUp1 <- length(dNewSites[dNewSites>0]) + 1

    dNewSites   <- diff(vQtyOfSites2)
    nQtyRampUp2 <- length(dNewSites[dNewSites>0]) + 1

    strTimeToStartFirst <- paste( ", ", dDelayToStart, " months to start 1st compound", sep="")


    strAssumpTr1 <- paste("     N = ", nMaxQtyPats, ", Delay between trials: ", dDelayBetweenTrials, " months", strTimeToStartFirst, "\n     ", sep = "")
    strAssumpTr1 <- paste(strAssumpTr1, "Compound 1 - Pats/Month/Site: ",
                          paste( vPatsPerMonthPerSite1,collapse=","),
                          "\n     Compound 1 - Qty. Of Sites Open\n     ", paste( vQtyOfSites1[1:nQtyRampUp1], collapse=","), sep = "")
    strAssumpTr1 <- paste(strAssumpTr1, "\n     Compound 2 - Pats/Month/Site: ",
                          paste( vPatsPerMonthPerSite2,collapse=","),
                          "\n     Compound 2 - Qty. Of Sites Open\n     ", paste( vQtyOfSites2[1:nQtyRampUp2], collapse=","), sep = "")

    nAdd <- length( vQtyOfSites1) - length( vPatsPerMonthPerSite1)
    if( nAdd > 0 )
    {
        vPatsPerMonthPerSite1 <- c(vPatsPerMonthPerSite1, rep(vPatsPerMonthPerSite1[ length(vPatsPerMonthPerSite1)], nAdd ))
    }
    vQtyOfPatsPerMonth1  <-ComputeMonthlyAccrual( vPatsPerMonthPerSite1, vQtyOfSites1)  #vQtyOfSites1* vPatsPerMonthPerSite1

    nAdd <- length( vQtyOfSites2) - length( vPatsPerMonthPerSite2)
    if( nAdd > 0 )
    {
        vPatsPerMonthPerSite2 <- c(vPatsPerMonthPerSite2, rep(vPatsPerMonthPerSite2[ length(vPatsPerMonthPerSite2)], nAdd ))
    }
    vQtyOfPatsPerMonth2  <- ComputeMonthlyAccrual( vPatsPerMonthPerSite2, vQtyOfSites2)  # vQtyOfSites2*vPatsPerMonthPerSite2



    mStartPerMonth <- rep( 0, 100)
    vFinalMonth1   <- rep( 0, nQtyReps )
    vFinalMonth2   <- rep( 0, nQtyReps )
    mStartPerMonth <- matrix( 0, nrow=100, ncol = nQtyReps)

    ap             <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth1, nMaxQtyPatients = nMaxQtyPats )
    ap2            <- NewAccrualProcess( vQtyPatsPerMonth = vQtyOfPatsPerMonth2, nMaxQtyPatients = nMaxQtyPats )


    iRep <- 1
    for( iRep in 1:nQtyReps)
    {
        vUnif1 <- runif( length( vQtyOfPatsPerMonth1 ), 0.5, 1.25)
        vUnif2 <- runif( length( vQtyOfPatsPerMonth2 ), 0.5, 1.25)

        ap     <- NewAccrualProcess( vQtyPatsPerMonth = vUnif1 * vQtyOfPatsPerMonth1, nMaxQtyPatients = nMaxQtyPats )
        ap2    <- NewAccrualProcess( vQtyPatsPerMonth = vUnif2 * vQtyOfPatsPerMonth2, nMaxQtyPatients = nMaxQtyPats )

        lRet <- SimulateSingleTrialRecruitment( ap, ap2, nMaxQtyPats, dDelayBetweenTrials  )

        if( iRep%% (nQtyReps*0.1) == 0 )
            print( paste( "Rep ", iRep, "..."))
        vFinalMonth1[ iRep ]  <- lRet$nFinalMonth1
        vFinalMonth2[ iRep ]  <- lRet$nFinalMonth2
        mStartPerMonth[,iRep] <- lRet$vStartPerMonth

    }


    mStartPerMonthCI   <- apply( mStartPerMonth, 1, quantile,probs=c(0.025, 0.975) )
    vStartPerMonthMean <- apply( mStartPerMonth, 1, mean)

    vLower <- mStartPerMonthCI[1,]
    vUpper <- mStartPerMonthCI[2,]


    vStartPerMonthMean<- c(rep(0,dDelayToStart),vStartPerMonthMean,nMaxQtyPats,nMaxQtyPats)
    vLower <- c( rep(0,dDelayToStart), vLower, nMaxQtyPats, nMaxQtyPats)
    vUpper <- c( rep(0,dDelayToStart), vUpper, nMaxQtyPats, nMaxQtyPats)
    vFinalMonth1 <- vFinalMonth1+dDelayToStart
    vFinalMonth2 <- vFinalMonth2+dDelayToStart


    dMeanFinalMonth1 <- mean( vFinalMonth1)
    vCI1 <- as.vector(quantile( vFinalMonth1, c(0.025, 0.975)))

    dMeanFinalMonth2 <- mean( vFinalMonth2 - vFinalMonth1)
    vCI2   <- as.vector(quantile( vFinalMonth2 - vFinalMonth1, c(0.025, 0.975)))



    vMonth <- 1:68

    return( list( vMonth= vMonth, vLower = vLower, vUpper = vUpper, vStartPerMonthMean = vStartPerMonthMean,
                  dMeanFinalMonth1 = dMeanFinalMonth1, vCI1 = vCI1, dMeanFinalMonth2 = dMeanFinalMonth2, vCI2 = vCI2,
                  strAssumpTr1 = strAssumpTr1, vFinalMonth1 = vFinalMonth1, vFinalMonth2 = vFinalMonth2 ))
}

SimulateSingleTrialRecruitment <- function( ap, ap2, nMaxQtyPats, dDelayBetweenTrials  )
{


    vStartPerMonth <- rep(0,100)

    vStartTime  <- SimulateArrivalTimes( ap )
    nFinalMonth1 <- vStartTime[ nMaxQtyPats]

    vStartTime2  <- SimulateArrivalTimes( ap2 )
    vStartTime <- c(vStartTime, vStartTime2+nFinalMonth1+dDelayBetweenTrials)


    nFinalMonth2 <- vStartTime[length( vStartTime )]

    vStartTimeTble <- table(ceiling(vStartTime))
    vTmpIndex <-as.integer(names(vStartTimeTble))
    vStartTimeTble <- vStartTimeTble[ vTmpIndex <= 100]
    vTmpIndex     <- vTmpIndex[ vTmpIndex <= 100]
    vStartPerMonth[ vTmpIndex ] <- vStartPerMonth[ vTmpIndex ] +  vStartTimeTble


    vStartPerMonth <-  cumsum(vStartPerMonth)

    return( list( nFinalMonth1 = nFinalMonth1, nFinalMonth2 = nFinalMonth2, vStartPerMonth = vStartPerMonth ))
}

SummarizeComplete <- function( lResPOC, lResPlat, strFile, xlim = 66, ylim = 250 )
{
    if( strFile != "" )
    {
        pdf( file = strFile )
    }
    plot( c( 0,1), y = c(0,10), type='n',axes=FALSE, frame.plot=FALSE, ann=FALSE)
    strAssump <- paste( "Assumptions - POC\n", lResPOC$strAssumpTr1, "\n\n", "Assumptions - Platform\n", lResPlat$strAssumpTr1, sep="")
    strPOC <- paste( "     2 POC Studies \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPOC$dMeanFinalMonth1,1),
                     "( ", round(lResPOC$vCI1[1],1),", ", round(lResPOC$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPOC$dMeanFinalMonth2,1),
                     "( ", round(lResPOC$vCI2[1],1),", ", round(lResPOC$vCI2[2],1),")", sep="")

    strPlat <- paste("     Platform \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPlat$dMeanFinalMonth1,1),
                     "( ", round(lResPlat$vCI1[1],1),", ", round(lResPlat$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPlat$dMeanFinalMonth2,1),
                     "( ", round(lResPlat$vCI2[1],1),", ", round(lResPlat$vCI2[2],1), ")",
                     sep="")


    vPOCTime1 <- lResPOC$vFinalMonth1
    vPlatTime1 <- lResPlat$vFinalMonth1
    vPercentSavings1 <- (vPOCTime1 - vPlatTime1)/vPOCTime1

    vPercentSavingCI1   <- round( 100*as.vector(quantile(vPercentSavings1 , c(0.025, 0.975))),1)
    strSavings <- paste("Percent savings on first compound: ", round( mean(vPercentSavings1)*100,1), "% ( ", vPercentSavingCI1[1], "%, ", vPercentSavingCI1[2], "% )", sep="" )


    vPOCTime2 <- lResPOC$vFinalMonth2-lResPOC$vFinalMonth1
    vPlatTime2 <- lResPlat$vFinalMonth2-lResPlat$vFinalMonth1
    vPercentSavings2 <- (vPOCTime2 - vPlatTime2)/vPOCTime2

    vPercentSavingCI2   <- round( 100*as.vector(quantile(vPercentSavings2 , c(0.025, 0.975))),1)
    strSavings <- paste(strSavings, "\nPercent savings on second compound: ", round( mean(vPercentSavings2)*100,1), "% ( ", vPercentSavingCI2[1], "%, ", vPercentSavingCI2[2], "% )", sep="" )


    strDesc <- paste( strAssump,  "\n\n\nResults\n", strPOC, "\n\n", strPlat, "\n\n", strSavings, sep="")
    text( 0,10.25, strDesc, adj=c(0,1), cex=.75)

    vMonth <- lResPOC$vMonth
    #Plot 1 - just the means
    plot( vMonth, lResPOC$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=2, xlim=c(0,xlim), main="Mean Recruitment")
    nMaxQtyPats <- lResPOC$vUpper[length(vMonth)]
    abline( h=c( nMaxQtyPats,nMaxQtyPats/2 ), col=8, lty=9)
    abline( v = seq(0,100,5), col=8, lty=9)

    lines( vMonth, lResPlat$vStartPerMonthMean[vMonth] , lwd=2, col=3)
    legend( 0, ylim, legend =c( "2 POC",  "Platform"), lty=c(1,2,1,2), lwd=2, col=c(1,3),  cex=.75)

    #Plot 2 - Means + CI
    plot( vMonth, lResPOC$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=2, xlim=c(0,xlim), main="Mean Recruitment")
    abline( h=c( nMaxQtyPats,nMaxQtyPats/2 ), col=8, lty=9)
    abline( v = seq(0,100,5), col=8, lty=9)

    lines( vMonth, lResPlat$vStartPerMonthMean[vMonth] , lwd=2, col=3, lty=1)
    lines( vMonth, lResPlat$vLower[vMonth] , lwd=2, col=3, lty=2)
    lines( vMonth, lResPlat$vUpper[vMonth] , lwd=2, col=3, lty=2)

    lines( vMonth, lResPOC$vStartPerMonthMean[vMonth] , lwd=2, col=1, lty=1)
    lines( vMonth, lResPOC$vLower[vMonth] , lwd=2, col=1, lty=2)
    lines( vMonth, lResPOC$vUpper[vMonth] , lwd=2, col=1, lty=2)
    legend( 0, ylim, legend =c( "2 POC", "95% CI 2 POC", "Platform", "95% CI Platform"), lty=c(1,2,1,2), lwd=2, col=c(1,1,3,3), cex=.75 )

    #Plot 3 - POC Means + CI
    plot( vMonth, lResPOC$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=2, xlim=c(0,xlim), main="2 POC")
    abline( h=c( nMaxQtyPats,nMaxQtyPats/2 ), col=8, lty=9)
    abline( v = seq(0,100,5), col=8, lty=9)

    lines( vMonth, lResPOC$vStartPerMonthMean[vMonth] , lwd=2, col=1, lty=1)
    lines( vMonth, lResPOC$vLower[vMonth] , lwd=2, col=1, lty=2)
    lines( vMonth, lResPOC$vUpper[vMonth] , lwd=2, col=1, lty=2)

    #Plot 4 - POC Means + CI
    plot( vMonth, lResPlat$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=2, xlim=c(0,xlim), main="Platform Trial")
    abline( h=c( nMaxQtyPats,nMaxQtyPats/2 ), col=8, lty=9)
    abline( v = seq(0,100,5), col=8, lty=9)

    lines( vMonth, lResPlat$vStartPerMonthMean[vMonth] , lwd=2, col=3, lty=1)
    lines( vMonth, lResPlat$vLower[vMonth] , lwd=2, col=3, lty=2)
    lines( vMonth, lResPlat$vUpper[vMonth] , lwd=2, col=3, lty=2)


    if( strFile != "" )
    {
        dev.off()
    }
}

SummarizeSimple <- function( lResPOC, lResPlat, strFile, xlim = NA, ylim = NA )
{
    if( strFile != "" )
    {
        pdf( file = strFile )
        #jpeg( file= strFile, width=1300, height=1000, res=100 )
    }

    strAssump <- paste( "Assumptions - POC\n", lResPOC$strAssumpTr1, "\n\n", "Assumptions - Platform\n", lResPlat$strAssumpTr1, sep="")
    strPOC <- paste( "     2 POC Studies \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPOC$dMeanFinalMonth1,1),
                     "( ", round(lResPOC$vCI1[1],1),", ", round(lResPOC$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPOC$dMeanFinalMonth2,1),
                     "( ", round(lResPOC$vCI2[1],1),", ", round(lResPOC$vCI2[2],1),")", sep="")

    strPlat <- paste("     Platform \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPlat$dMeanFinalMonth1,1),
                     "( ", round(lResPlat$vCI1[1],1),", ", round(lResPlat$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPlat$dMeanFinalMonth2,1),
                     "( ", round(lResPlat$vCI2[1],1),", ", round(lResPlat$vCI2[2],1), ")",
                     sep="")


    vPOCTime1 <- lResPOC$vFinalMonth1
    vPlatTime1 <- lResPlat$vFinalMonth1
    vPercentSavings1 <- (vPOCTime1 - vPlatTime1)/vPOCTime1

    vPercentSavingCI1   <- round( 100*as.vector(quantile(vPercentSavings1 , c(0.025, 0.975))),1)
    strSavings <- paste("Percent savings on first compound: ", round( mean(vPercentSavings1)*100,1), "% ( ", vPercentSavingCI1[1], "%, ", vPercentSavingCI1[2], "% )", sep="" )


    vPOCTime2 <- lResPOC$vFinalMonth2-lResPOC$vFinalMonth1
    vPlatTime2 <- lResPlat$vFinalMonth2-lResPlat$vFinalMonth1
    vPercentSavings2 <- (vPOCTime2 - vPlatTime2)/vPOCTime2

    vPercentSavingCI2   <- round( 100*as.vector(quantile(vPercentSavings2 , c(0.025, 0.975))),1)
    strSavings <- paste(strSavings, "\nPercent savings on second compound: ", round( mean(vPercentSavings2)*100,1), "% ( ", vPercentSavingCI2[1], "%, ", vPercentSavingCI2[2], "% )", sep="" )


    strDesc <- paste( strAssump,  "\n\n\nResults\n", strPOC, "\n\n", strPlat, "\n\n", strSavings, sep="")

    vMonth <- lResPOC$vMonth
    #Plot 1 - just the means
    #plot( vMonth, lResPOC$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=2, xlim=c(0,xlim), main="Mean Recruitment")
    #nMaxQtyPats <- lResPOC$vUpper[length(vMonth)]
    #abline( h=c( nMaxQtyPats,nMaxQtyPats/2 ), col=8, lty=9)
    #abline( v = seq(0,100,5), col=8, lty=9)

    #lines( vMonth, lResPlat$vStartPerMonthMean[vMonth] , lwd=2, col=3)
    #legend( 0, ylim, legend =c( "2 POC",  "Platform"), lty=c(1,2,1,2), lwd=2, col=c(1,3),  cex=.75)

    #Plot 2 - Means + CI
    if( is.na( ylim ) )
        ylim <- max( lResPOC$vStartPerMonthMean,lResPOC$vStartPerMonthMean)

    if( is.na( xlim ) )
    {

        xPoc  <- vMonth[ lResPOC$vLower == max( lResPOC$vLower )]
        xPlat <- vMonth[ lResPlat$vLower ==  max( lResPlat$vLower)]
        xlim  <- max( xPoc[1], xPlat[1] )*1.05

    }

    plot( vMonth, lResPOC$vStartPerMonthMean[vMonth] , type ='l', xlab="Month", ylab="# of Patients", ylim = c(0,ylim), lwd=3, xlim=c(0,xlim), main="Mean Recruitment")
    abline( h=seq( 0, ylim, ylim/3), col=8, lty=9)
    abline( v = seq(0,100,5), col=8, lty=9)

    lines( vMonth, lResPlat$vStartPerMonthMean[vMonth] , lwd=3, col=3, lty=1)
    lines( vMonth, lResPlat$vLower[vMonth] , lwd=3, col=3, lty=2)
    lines( vMonth, lResPlat$vUpper[vMonth] , lwd=3, col=3, lty=2)

    lines( vMonth, lResPOC$vStartPerMonthMean[vMonth] , lwd=3, col=1, lty=1)
    lines( vMonth, lResPOC$vLower[vMonth] , lwd=3, col=1, lty=2)
    lines( vMonth, lResPOC$vUpper[vMonth] , lwd=3, col=1, lty=2)
    legend( 0, ylim, legend =c( "2 POC", "95% CI 2 POC", "Platform", "95% CI Platform"), lty=c(1,2,1,2), lwd=2, col=c(1,1,3,3), cex=.75)



    if( strFile != "" )
    {
        dev.off()
    }
}

# lResPOC <- SimulateAccrual( 100, 100,6,
#                               c(0.1,0.3,0.45, 0.5), c(5,10,15,20,25,35), c(0.1,0.3,0.45, 0.5),
#                               c(5,10,15,20,25,35), 3 )
#
#
#
#
#
# lResPlat <- SimulateAccrual( 100,100, 1,
#                                c(0.1,0.3,0.45, 0.5), c(3,8,15,35,50,70),  c(0.1,0.3,0.45, 0.5),
#                                c(7.5, 15, 23,30, 37.5, 70), 6   )
#
# ggSummarizeSimple( lResPOC, lResPlat, "" )
#  ggPlotPlatformRecruitment ( 100,100, c(0.1,0.3,0.45, 0.5),
#                                        6, 6, c(3,8,15,35,50,70),
#                                        6,2, c(3,8,15,35,50,70), c(7.5, 15, 23,30, 37.5, 70) )
#

ggSummarizeSimple <- function( lResPOC, lResPlat,  xlim = NA, ylim = NA )
{


    strAssump <- paste( "Assumptions - POC\n", lResPOC$strAssumpTr1, "\n\n", "Assumptions - Platform\n", lResPlat$strAssumpTr1, sep="")
    strPOC <- paste( "     2 POC Studies \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPOC$dMeanFinalMonth1,1),
                     "( ", round(lResPOC$vCI1[1],1),", ", round(lResPOC$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPOC$dMeanFinalMonth2,1),
                     "( ", round(lResPOC$vCI2[1],1),", ", round(lResPOC$vCI2[2],1),")", sep="")

    strPlat <- paste("     Platform \n     Ave. Time to Finish Compound 1 (95% CI): ", round(lResPlat$dMeanFinalMonth1,1),
                     "( ", round(lResPlat$vCI1[1],1),", ", round(lResPlat$vCI1[2],1), ")",
                     "\n     Ave. Time to Finish Compound 2 (95% CI): ", round(lResPlat$dMeanFinalMonth2,1),
                     "( ", round(lResPlat$vCI2[1],1),", ", round(lResPlat$vCI2[2],1), ")",
                     sep="")


    vPOCTime1 <- lResPOC$vFinalMonth1
    vPlatTime1 <- lResPlat$vFinalMonth1
    vPercentSavings1 <- (vPOCTime1 - vPlatTime1)/vPOCTime1

    vPercentSavingCI1   <- round( 100*as.vector(quantile(vPercentSavings1 , c(0.025, 0.975))),1)
    strSavings <- paste("Percent savings on first compound: ", round( mean(vPercentSavings1)*100,1), "% ( ", vPercentSavingCI1[1], "%, ", vPercentSavingCI1[2], "% )", sep="" )


    vPOCTime2 <- lResPOC$vFinalMonth2-lResPOC$vFinalMonth1
    vPlatTime2 <- lResPlat$vFinalMonth2-lResPlat$vFinalMonth1
    vPercentSavings2 <- (vPOCTime2 - vPlatTime2)/vPOCTime2

    vPercentSavingCI2   <- round( 100*as.vector(quantile(vPercentSavings2 , c(0.025, 0.975))),1)
    strSavings <- paste(strSavings, "\nPercent savings on second compound: ", round( mean(vPercentSavings2)*100,1), "% ( ", vPercentSavingCI2[1], "%, ", vPercentSavingCI2[2], "% )", sep="" )


    strDesc <- paste( strAssump,  "\n\n\nResults\n", strPOC, "\n\n", strPlat, "\n\n", strSavings, sep="")

    vMonth <- lResPOC$vMonth

    #Plot 2 - Means + CI
    if( is.na( ylim ) )
        ylim <- max( lResPOC$vStartPerMonthMean,lResPOC$vStartPerMonthMean)


    if( is.na( xlim ) )
    {

        xPoc  <- vMonth[ lResPOC$vLower == max( lResPOC$vLower )]
        xPlat <- vMonth[ lResPlat$vLower ==  max( lResPlat$vLower)]
        xlim  <- max( xPoc[1], xPlat[1] )*1.05
        xlimScale <- scale_x_continuous( limits=c( 0, round( xlim, 0)))

    }
    else
    {
        xlimScale <- NULL
    }

    df <- data.frame( Month = c(vMonth, vMonth),
                      startPatPerMonthMean = c( lResPOC$vStartPerMonthMean[vMonth],lResPlat$vStartPerMonthMean[vMonth]),
                      Type = c(rep( "2 POC", length(vMonth)), rep( "Platform", length( vMonth))),
                      Lower = c( lResPOC$vLower[vMonth],lResPlat$vLower[vMonth]),
                      Upper = c( lResPOC$vUpper[vMonth],lResPlat$vUpper[vMonth]))

    p1 = ggplot(data = df, aes( x = Month, y = startPatPerMonthMean, colour=Type) ) +
        geom_line(lwd =2) +
        xlimScale +
        labs( title ="Mean Recruitment", y="# of Patients") +
        theme_bw() +
        theme(plot.title =element_text(hjust=0.5), panel.grid.major.y = element_line(size=1.5)) +
        theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() ) +
        geom_ribbon( aes( x= Month, ymin=Lower, ymax=Upper, fill=Type), alpha=0.2, colour=NA) +
        scale_colour_manual( labels=c("2 POC ( 95% CI )","Platform ( 95% CI )"), values=c( "black", "green")) +

        scale_fill_manual(labels=c("2 POC ( 95% CI )","Platform ( 95% CI )"),values=c("gray", "green"))



    return( p1 )
}
