##### File Description ######################################################################
#   This functions are not generic (have not had time for this) and hence are based on
#   number of ISAs in the simulation
############################################################################################.

library( OCTOPUS )
library( ggplot2)

#####################################################################################################################.
#   CreateScenarioTables() - Source the Scenarios.R file and create scenario table to go into a report.
#   This is just an example for the user to see how one may add scenario info to a report template like R markdown.
#   Currently it is not a working example
#####################################################################################################################.

CreateScenarioTables <- function( )
{
    # If you creates a scenario file you could do something like the following
    #source( "Scenarios.R")
    vObsTime            <- c( 0,  2, 4, 8, 12)

    #Loop over the scenarios
    iScen = 2
    mMeanAltIR <- get0( paste("mMeanAlt", iScen, sep = "") )
    if( is.null(mMeanAltIR ) == FALSE )
    {
        repeat
        {
            #Loop to created what you want to include in the report

        }
    }

    #browser()
    iScen = iScen -1
    vScen = rep(1:(iScen),rep(2, iScen ))
    vScen[seq(2,length(vScen),2)] = " "

    #Build a dataframe to create the table, this example as some simple latex to have up arrow ect
    dfScen = data.frame( cbind(vScen,rep(c("Population 1","Populaiton s")), mScenDeltaDelta,
                               c("Null", "Null",
                                 "Trt", "Treatment",
                                 "Treatment", "Treatment",
                                 "$\\downarrow$ Treatment", "Treatment",
                                 "$\\downarrow$ Treatment", "$\\uparrow$ $\\uparrow$ Treatment",

                                 "$\\uparrow$ $\\uparrow$ Treatment", "$\\uparrow$ $\\uparrow$ Treatment",
                                 "$\\approx$ Treatment", "$\\approx$ Treatment",
                                 "$\\downarrow$ $\\downarrow$ Treatment", "$\\downarrow$ $\\downarrow$  Treatment",
                                 "$\\downarrow$ Treatment", "$\\downarrow$ Treatment",
                                 "90\\% Treatment", "90\\% Treatment"),
                               c("Null", "Null",
                                 "Treatment", "Treatment",
                                 "Treatment", "Null",
                                 "$\\downarrow$ Treatment", "Treatment",
                                 "$\\downarrow$$\\downarrow$ Treatment", "$\\downarrow$$\\downarrow$ Treatment",

                                 "$\\uparrow$ Treatment", "$\\uparrow$ Treatment",
                                 "$\\downarrow$ Treatment", "$\\downarrow$ Treatment",
                                 "$\\downarrow$ $\\downarrow$ Treatment", "$\\downarrow$ $\\downarrow$ Treatment",
                                 "$\\downarrow$ Treatment", "$\\downarrow$ Treatment",
                                 "90\\% Treatment", "90\\% Treatment")))
    colnames( dfScen ) = c( "Scen.","Population", paste( "Week ", vObsTime), "$\\delta\\delta$ Obs. Mean Outcome ", "Outcome 1","Outcome 2"  )
    dfScen <- dfScen[,-3]

    strTable = kable( dfScen, row.names=FALSE, escape=FALSE ) %>% kable_styling("basic") %>%
        add_header_above(c(" " = 1, " " = 1, "$\\delta\\delta$ Outcome 1 " = 4, " " = 1,"Description" = 2), escape=FALSE) %>%
        row_spec( c(1,2,5,6,9,10,13,14,17,18), background="#f9f9f9") %>%
        column_spec( 7, width= "3cm")

    return( strTable )

}

#####################################################################################################################.
#   Read in the data from the simulation.
#   This function will load Design.RData, from BuildMe.R and simsCombined.RData, created after
#   the simulations complete by calling BuildSimulationResultsDataSet()
#####################################################################################################################.
ProcessSimulationResults <- function( )
{
    #load( "cTrialDesign.RData")   #This should create mDesigns matrix that was saved in the BuildMe.R file
    cTrialDesign <- readRDS( "cTrialDesign1.Rds")
    # dfDesign <- data.frame( cTrialDesign  )


    #load( "simsCombined.RData")

    #simsAll     <- simsCombined
    simsAll     <- readRDS( "simsCombined.Rds")
    nQtyISA     <- length( cTrialDesign$cISADesigns )
    vQtyTrt     <- rep( 0, nQtyISA )
    for( iISA in 1:nQtyISA )
    {
        vQtyTrt[ iISA ] <- length( cTrialDesign$cISADesigns[[iISA]]$vQtyPats)
    }


    vScen       <- unique(simsAll$iScen)
    vDesign     <- sort(unique( simsAll$Design) )

    vRes        <- list()
    iDes        <- 1
    iScen       <- 1
    iCount      <- 0
    iDes <-1
    for( iDes in 1:length( vDesign ) )
    {
        for( iScen in 1:length(vScen) )
        {

            iCount <- iCount + 1
            lISAResult         <- PostProcessSimulationResults( simsAll, iDes, iScen, vQtyTrt,  dPUFinal = NA )


            vRes[[iCount]] <- cbind( data.frame(            design   = iDes,
                                                            scenario = iScen,
                                                            value    = lISAResult$vValue, # move the first two elements out of here
                                                            what     = lISAResult$vWhat,
                                                            isa      = lISAResult$vISA ),
                                     vScenLab = paste( iScen))


        }
    }


    mRes         <- do.call( rbind, vRes)
    lResults     <- list( mResults = mRes,  cTrialDesign = cTrialDesign )
    return( lResults )
}

PlotExampleTrial <- function( nQtyPatsCtrl,  nQtyPatsTrt, dRespCtrl, dDelta, nSeed )
{
    set.seed( nSeed )
    dRespTrt  <- dRespCtrl + dDelta

    nQtyRespCtrl <- rbinom( 1, nQtyPatsCtrl, dRespCtrl )
    nQtyRespTrt  <- rbinom( 1, nQtyPatsTrt, dRespTrt )

    vPiCtrl <<- rbeta( 1000000, 0.5 + nQtyRespCtrl, 0.5 + nQtyPatsCtrl - nQtyRespCtrl  )
    vPiTrt  <<- rbeta( 1000000, 0.5 + nQtyRespTrt, 0.5 + nQtyPatsTrt -  nQtyRespTrt  )
    vDelta  <- vPiTrt - vPiCtrl


    vDen <- density( vDelta )

    vDesiredCI  <- c( 0.2, 0.8)
    dC1         <- 0.20
    dfDen       <- density( vDelta )
    dProbLess   <- mean( vDelta > dC1 )
    vCI         <- round( quantile( vDelta, probs= vDesiredCI ), 3)
    dfDen       <- data.frame( Delta = dfDen$x, Density = dfDen$y )

    dfDen$Grp <- ifelse( dfDen$Delta >= dC1, "1", "2")

    gPlot1 <- ggplot( dfDen, aes( x=Delta, y = Density, group=Grp, fill = Grp) ) +
                geom_line( size = 0.75) +
                geom_ribbon( data=subset( dfDen, Grp =1), aes( x=Delta, ymax = Density), ymin= 0, alpha =0.3)+
                scale_fill_manual(name='', values=c("1" = "green4", "2" = "red"))+
                theme_bw() +
                theme( plot.title = element_text(hjust = 0.5), legend.position="none" ) +
                #ggtitle( paste0("Probability of Delta > ", dC1, " = ", round( 100*dProbLess,1), "%, Est. Delta = ", round( mean( vDelta), 3)) ) +
                ggtitle( paste0( "Est. Delta = ", round( mean( vDelta), 3),
                                 "\nProbability of Delta > ", dC1, " = ", round( 100*dProbLess,1),
                                 "%\n60% CI From (20%, 80%): ( ",vCI[1], ", ", vCI[2], " )" )) +
                xlab( paste0( "Delta " ) )  +
                scale_x_continuous(breaks=seq(-0.25,0.5,by=.1), minor_breaks=seq( -0.2,0.5,by=.1),limit=c(-0.25, 0.5) )+
                annotate( geom= "text", x=-0.25, y = 6.4, vjust=1, hjust=0,  label= "Example Data") +
                annotate( geom="text", x=-0.25, y =6, vjust=1, hjust=0, label=paste0( "Ctrl: ", nQtyRespCtrl, "/", nQtyPatsCtrl, "\nTrt: ",nQtyRespTrt, "/", nQtyPatsTrt  ))

    vPi <- seq( 0.001, 0.999, 0.001 )
    #vPiCtrlDen <- dbeta( vPi, 0.138 + nQtyRespCtrl, 0.862 + nQtyPatsCtrl - nQtyRespCtrl)
    #vPiTrtDen  <- dbeta( vPi, 0.138 + nQtyRespTrt,  0.862 + nQtyPatsTrt -  nQtyRespTrt )

    vPiCtrlDen <- dbeta( vPi, 0.5 + nQtyRespCtrl, 0.5 + nQtyPatsCtrl - nQtyRespCtrl)
    vPiTrtDen  <- dbeta( vPi, 0.5 + nQtyRespTrt,  0.5 + nQtyPatsTrt -  nQtyRespTrt )

    vCICtrl    <- qbeta( vDesiredCI, 0.5 + nQtyRespCtrl, 0.5 + nQtyPatsCtrl - nQtyRespCtrl )
    vCITrt     <- qbeta( vDesiredCI, 0.5 + nQtyRespTrt,  0.5 + nQtyPatsTrt -  nQtyRespTrt  )

    dfCtrlPost <- data.frame( Pi = vPi, Den = vPiCtrlDen, Trt = 'Ctrl')
    dfTrtPost  <- data.frame( Pi = vPi, Den = vPiTrtDen, Trt = 'Trt')

    dfPostDen    <- dfCtrlPost %>% dplyr::add_row( dfTrtPost )

    gPlot2 <- ggplot( dfPostDen, aes( Pi, Den, group = Trt, fill = Trt )) +
                geom_line( size = 0.75) +
                geom_ribbon( data=subset( dfPostDen, (Trt =='Ctrl' & Pi > vCICtrl[ 1 ] & Pi < vCICtrl[2] ) |
                                                     (Trt =='Trt'  & Pi > vCITrt[ 1 ] & Pi < vCITrt[ 2 ]) ) ,
                             aes( x=Pi, ymax = Den), ymin= 0, alpha =0.3) +
                scale_fill_manual(name='', values=c("Ctrl" = "#E69F00", "Trt" = "#56B4E9")) +
                theme_bw() +
                theme( plot.title = element_text(hjust = 0.5), legend.position="none" ) +
                ggtitle( paste0( "Posterior Disttribution with 60% CI (20%, 80%)\n Brown (Control), Blue (Treatment)"  )) +
                xlab( paste0( "Response Rate" ) )  +
                scale_x_continuous(breaks=seq(0.15,1,by=.1), minor_breaks=seq( 0.2,1,by=.1),limit=c(0.15, 1) )+
                annotate( geom= "text", x=-0.25, y = 6.7, vjust=1, hjust=0,  label= "Example Data") +
                annotate( geom="text", x=-0.25, y =6, vjust=1, hjust=0, label=paste0( "Ctrl: ", nQtyRespCtrl, "/", nQtyPatsCtrl, "\nTrt: ",nQtyRespTrt, "/", nQtyPatsTrt  ))

    return( list( DeltaPlot = gPlot1, TreatmentPlot = gPlot2 ) )
}
PlotResults <- function( dfResults, file="",
                         colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                          "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                         vScenarioLabel = NULL, nStartFigureNumber = 1   )
{


    dfResultsAll       <- dfResults
    #dfResultsAll$SSLab <- ifelse( dfResultsAll$design==3, "100:100 NB", "50:100")
    vScen              <- sort( unique( dfResults$scenario) )
    vDesigns           <- unique( dfResults$design )

    for( nScen in vScen )
    {

        dfResults       <- dfResultsAll[ dfResultsAll$scenario == nScen,]
        dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" | what =="Pr(Pause)")

        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)","Pr(Pause)", "Pr(Go)" ) )

        #dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" )

        #dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)", "Pr(Go)" ) )



        if( length(vScenarioLabel ) == length( vScen ) )
        {
            ggTitle <- ggtitle( paste( "Figure", nStartFigureNumber + nScen - 1, " - Scenario ", nScen) , subtitle = vScenarioLabel[ nScen ])
        }
        else
        {
            ggTitle <- ggtitle( paste( "Platform Simulations - Scenario ", nScen) )
        }

        if( length( vDesigns ) == 1)
        {
            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(isa, level=unique(isa)), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                xlab("ISA") +
                ylab("Probability") +
                ggTitle +
                scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                geom_text( aes(y = GetY( value ), x = factor(isa, level=unique(isa)), label= GetLabel(value)), size=2.5, col=rep(c("black","black","white"),nrow(dfProbs)/3))
        }
        else
        {



            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(design , level=unique(design )), label=GetLabel(value), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                facet_grid( .~isa) +
                xlab("Design") +
                ylab("Probability") +
                ggTitle +
                scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +

                #scale_fill_manual(values=c(colorPalette[7], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                #geom_text(size = 5, position = position_stack(vjust = 0.5), col=rep(c("black","white"),nrow(dfProbs)/3))
                geom_text( aes(y = GetY( value ), x = factor(design, level=unique(design)), label= GetLabel(value)), size=2.5, col=c("black"))


        }
        if( file != "" )
            png( paste( file ), width=1000,height=600)
        print( ocPlot )
        if( file != "" )
            dev.off()

    }

}


GetPlotResults <- function( dfResults, file="",
                         colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                          "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                         vScenarioLabel = NULL, nStartFigureNumber = 1   )
{


    dfResultsAll       <- dfResults
    #dfResultsAll$SSLab <- ifelse( dfResultsAll$design==3, "100:100 NB", "50:100")
    vScen              <- sort( unique( dfResults$scenario) )
    vDesigns           <- unique( dfResults$design )

    lPlots <- list()
    for( nScen in vScen )
    {

        dfResults       <- dfResultsAll[ dfResultsAll$scenario == nScen,]
        dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" | what =="Pr(Pause)")

        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)","Pr(Pause)", "Pr(Go)" ) )

        dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" )

        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)", "Pr(Go)" ) )



        if( length(vScenarioLabel ) == length( vScen ) )
        {
            ggTitle <- ggtitle( paste( "Figure", nStartFigureNumber + nScen - 1, " - Scenario ", nScen) , subtitle = vScenarioLabel[ nScen ])
        }
        else
        {
            ggTitle <- ggtitle( paste( "Platform Simulations - Scenario ", nScen) )
        }

        if( length( vDesigns ) == 1)
        {
            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(isa, level=unique(isa)), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                xlab("ISA") +
                ylab("Probability") +
                ggTitle +
                scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                geom_text( aes(y = GetY( value ), x = factor(isa, level=unique(isa)), label= GetLabel(value)), size=2.5, col=rep(c("black","black","white"),nrow(dfProbs)/3))
        }
        else
        {



            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(design , level=unique(design )), label=GetLabel(value), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                facet_grid( .~isa) +
                xlab("Design") +
                ylab("Probability") +
                ggTitle +
                #scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +

                scale_fill_manual(values=c(colorPalette[7], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                #geom_text(size = 5, position = position_stack(vjust = 0.5), col=rep(c("black","white"),nrow(dfProbs)/3))
                geom_text( aes(y = GetY( value ), x = factor(design, level=unique(design)), label= GetLabel(value)), size=2.5, col=rep(c("black","white"),nrow(dfProbs)/2))


        }
        if( file != "" )
            png( paste( file ), width=1000,height=600)
        #print( ocPlot )
        lPlots[[paste0("scenario", nScen)]] <- ocPlot
        if( file != "" )
            dev.off()

    }
    return( lPlots )

}

PlotCIResults <- function( dfResults, file="",
                         colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                          "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                         vScenarioLabel = NULL, nStartFigureNumber = 1   )
{


    dfResultsAll       <- dfResults
    #dfResultsAll$SSLab <- ifelse( dfResultsAll$design==3, "100:100 NB", "50:100")
    vScen              <- sort( unique( dfResults$scenario) )
    vDesigns           <- unique( dfResults$design )

    for( nScen in vScen )
    {

        dfResults       <- dfResultsAll[ dfResultsAll$scenario == nScen,]
        dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" | what =="Pr(Pause)")

        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)","Pr(Pause)", "Pr(Go)" ) )

        dfProbs         <- subset( dfResults, what=="Pr(Go)" | what == "Pr(No Go)" )

        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go)", "Pr(Go)" ) )



        if( length(vScenarioLabel ) == length( vScen ) )
        {
            ggTitle <- ggtitle( paste( "Figure", nStartFigureNumber + nScen - 1, " - Scenario ", nScen) , subtitle = vScenarioLabel[ nScen ])
        }
        else
        {
            ggTitle <- ggtitle( paste( "Platform Simulations - Scenario ", nScen) )
        }

        if( length( vDesigns ) == 1)
        {
            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(isa, level=unique(isa)), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                xlab("ISA") +
                ylab("Probability") +
                ggTitle +
                scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                geom_text( aes(y = GetY( value ), x = factor(isa, level=unique(isa)), label= GetLabel(value)), size=2.5, col=rep(c("black","black","white"),nrow(dfProbs)/3))
        }
        else
        {



            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(design , level=unique(design )), label=GetLabel(value), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                facet_grid( .~isa) +
                xlab("Design") +
                ylab("Probability") +
                ggTitle +
                #scale_fill_manual(values=c(colorPalette[7],colorPalette[5], colorPalette[4])) +

                scale_fill_manual(values=c(colorPalette[7], colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                #geom_text(size = 5, position = position_stack(vjust = 0.5), col=rep(c("black","white"),nrow(dfProbs)/3))
                geom_text( aes(y = GetY( value ), x = factor(design, level=unique(design)), label= GetLabel(value)), size=2.5, col=rep(c("black","white"),nrow(dfProbs)/2))


        }
        if( file != "" )
            png( paste( file ), width=1000,height=600)
        print( ocPlot )
        if( file != "" )
            dev.off()

    }

}


PlotResultsLineGraph <- function( dfResults,vTrueRates, file="",
                                  colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),

                                  vScenarioLabel = NULL, nStartFigureNumber = 1)
{


    #vTrueRates <-  0.138 + c( 0, 0.1, 0.125, 0.15, 0.175, 0.2, 0.25, 0.3,0.35,0.4 )

    dfScen <- data.frame( vScenLab = as.character( 1:length( vTrueRates ) ), TrueRates = vTrueRates)


    dfResultsAll    <- dfResults
    vScen           <- sort( unique( dfResults$scenario) )
    vDesigns        <- unique( dfResults$design )
    vISA            <- unique( dfResultsAll$isa )
    nScen <- vScen[1]

    nISA <- vISA[ 1 ]

    vWhat           <- c( "Pr(Go)", "Pr(No Go)" )
    vLabel          <- c( "Probability of Go", "Probability of No Go")
    lPlotsByISA     <- list()
    lPlotsByDesign  <- list()
    ######################################################################################################################## .
    # Plot 1 - Pr(Go) line for each design, facet for each ISA ####
    ######################################################################################################################## .

    i <- 1
    for( strWhat in vWhat )
    {
        dfProbs         <- dfResultsAll #[ dfResultsAll$isa == nISA,]

        dfProbs         <- subset( dfProbs, what==strWhat ) #| what == "Pr(No Go)" | what =="Pr(Pause)")

        dfProbs         <- dplyr::left_join( dfProbs, dfScen, by="vScenLab")

        dfProbs$design    <- factor( dfProbs$design  )

        if( length( vDesigns ) == 1)
        {
            lPlotsByISA[[ i ]] <-  ggplot( data = dfProbs, aes( y=value, x = TrueRates)) +
                geom_line( size = 1.5 )+
                facet_grid( .~isa) +
                xlab("True Rate on E") +
                ylab( vLabel[ i ] ) +
                ggtitle( paste0( "Platform Simulation Results - ", vLabel[ i ])) +
                scale_y_continuous(breaks=seq(0,1,by=.1), minor_breaks=seq( 0.05,1, by=0.1),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal" )  #, legend.title  =element_blank() )


        }
        else
        {

            lPlotsByISA[[ i ]] <-  ggplot( data = dfProbs, aes( y=value, x = TrueRates, colour = design, group=design)) +
                geom_line( size = 1.5 )+
                facet_grid( .~isa) +
                xlab("True Rate on E") +
                ylab( vLabel[ i ] ) +
                ggtitle( paste0( "Platform Simulation Results - ", vLabel[ i ])) +
                scale_y_continuous(breaks=seq(0,1,by=.1), minor_breaks=seq( 0.05,1, by=0.1),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal" ) +

                scale_color_manual(values=colorPalette, name= "Design") #, legend.title  =element_blank() )

            lPlotsByDesign[[ i ]] <-  ggplot( data = dfProbs, aes( y=value, x = TrueRates, colour = isa, group=isa)) +
                geom_line( size = 1.5 )+
                facet_grid( .~design) +
                xlab("True Rate on E") +
                ylab( vLabel[ i ] ) +
                ggtitle( paste0( "Platform Simulation Results - ", vLabel[ i ])) +
                scale_y_continuous(breaks=seq(0,1,by=.1), minor_breaks=seq( 0.05,1, by=0.1),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5),
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal" ) +

                scale_color_manual(values=colorPalette, name= "ISA")
        }

        i <- i + 1
    }
    # if( file != "" )
    #     png( paste( file ), width=1000,height=600)
    # print( ocPlot )
    # if( file != "" )
    #     dev.off()


    return( list( lPlotsByISA = lPlotsByISA, lPlotsByDesign = lPlotsByDesign ) )
}


PlotResultsWithIAInfo <- function( dfResults, file="",
                                   colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7") ,
                                   vScenarioLabel = NULL  )
{

    dfResultsAll       <- dfResults
    #dfResultsAll$SSLab <- ifelse( dfResultsAll$design==3, "100:100 NB", "50:100")
    vScen              <- sort( unique( dfResults$scenario) )
    vDesigns           <- unique( dfResults$design )


    for( nScen in vScen )
    {

        if( length(vScenarioLabel ) == length( vScen ) )
        {
            ggTitle <- ggtitle( paste( "Platform Simulations - Scenario ", nScen) , subtitle = vScenarioLabel[ nScen ])
        }
        else
        {
            ggTitle <- ggtitle( paste( "Platform Simulations - Scenario ", nScen) )
        }


        dfResults       <- dfResultsAll[ dfResultsAll$scenario == nScen,]
        dfProbs         <- subset( dfResults, what=="Pr(No Go @ FA)" | what == "Pr(No Go @ IA)" | what =="Pr(Pause)" | what == "Pr(Go @ IA)" | what == "Pr(Go @ FA)" )




        dfProbs$what    <- factor( dfProbs$what, levels = c("Pr(No Go @ FA)" ,"Pr(No Go @ IA)", "Pr(Pause)", "Pr(Go @ IA)", "Pr(Go @ FA)") )

        if( length( vDesigns ) == 1)
        {
            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(isa, level=unique(isa)), label=GetLabel(value),fill = what)) +
                geom_bar( stat="identity", position='stack') +
                xlab("ISA") +
                ylab("Probability") +
                ggTitle+
                scale_fill_manual(values=c( colorPalette[2],colorPalette[7], colorPalette[5],colorPalette[3],colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5), panel.grid.major.y = element_line(size=1.5), plot.subtitle = element_text( hjust=0.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                geom_text(size = 3,position = position_stack(vjust = 0.5), col=rep(c("black","black","black","white","white"),nrow(dfProbs)/5))
        }
        else
        {



            ocPlot <- ggplot(data=dfProbs, aes(y=value, x=factor(design , level=unique(design )), label=GetLabel(value), fill = what)) +
                geom_bar( stat="identity", position='stack') +
                facet_grid( .~isa) +
                xlab("Design") +
                ylab("Probability") +
                ggTitle +
                scale_fill_manual(values=c( colorPalette[2],colorPalette[7], colorPalette[5],colorPalette[3],colorPalette[4])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5), panel.grid.major.y = element_line(size=1.5), plot.subtitle = element_text( hjust=0.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )+
                geom_text(size = 3, position = position_stack(vjust = 0.5), col=rep(c("black","black","black","white","white"),nrow(dfProbs)/5))
            # geom_text( aes(y = GetY( value ), x = factor(design, level=unique(design)), label= GetLabel(value)), size=5, col=rep(c("black","black","white"),nrow(dfProbs)/3))


        }
        if( file != "" )
            png( paste( file ), width=1000,height=600)
        print( ocPlot )
        if( file != "" )
            dev.off()

    }

}


PlotSubgroupResults <- function( dfResults, file="",
                                 colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  )
{

    vScen <- unique( dfResults$scenario )
    vISA  <- unique( dfResults$isa )
    for( iScen in vScen )
    {
        for( nISA in vISA )
        {
            dfResultsScen <- subset( dfResults, scenario == iScen & isa== nISA )
            ocPlot <- ggplot( data = dfResultsScen, aes( y =value, x=factor(vGroup, level=unique(vGroup)), fill=what)) +
                geom_bar( stat="identity", position="stack") +
                xlab("Group") +
                ylab("Probability") +
                ggtitle( paste( "Subroup Decisions - Scenario ", iScen, " ISA ", nISA) )+
                scale_fill_manual(values=c( colorPalette[4],colorPalette[3], colorPalette[5],colorPalette[7],colorPalette[2])) +
                scale_y_continuous(breaks=seq(0,1,by=.2), minor_breaks=seq( 0.05,1, by=0.05),limit=c(0,1) )+
                theme_bw() +
                theme(plot.title =element_text(hjust=0.5), panel.grid.major.y = element_line(size=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal", legend.title  =element_blank() )

            print( ocPlot )
        }


    }
    #if( file != "" )
    #   png( paste( file ), width=1000,height=600)
    #print( ocPlot )
    if( file != "" )
        dev.off()

    #}

}



#####################################################################################################################.
# For a given design and scenario process the results.
# This function loops over each ISA and processes the results
#####################################################################################################################.
PostProcessSimulationResults  <- function( mSims, nDes, nScen, vQtyTrt, dPUFinal = NA, bPrintStatus = FALSE  )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen & mSims$Design == nDes, ]

    nQtyISA   <- length( vQtyTrt )

    if( bPrintStatus )
        print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp), " # ISA = ", nQtyISA ))

    nTreatmentStartNumber <- 2

    lISAResults <- list( vWhat = c(), vISA = c(), vValue = c() )

    for( nISA in 1:nQtyISA )
    {
        lISAResults           <- ProcessISAResults( mSimsTmp, nISA, nTreatmentStartNumber, vQtyTrt[ nISA ], lISAResults, dPUFinal = dPUFinal  )
        nTreatmentStartNumber <- lISAResults$nNextTreatmentStartNumber

    }

    lISAResults$vWhat   <- c( lISAResults$vWhat, c("AveTrialLength", "LowerLimitTrialLength", "UpperLimitTrialLength" ) )
    lISAResults$vISA    <- c( lISAResults$vISA, c( "Trial", "Trial", "Trial"  ) )
    lISAResults$vValue  <- c( lISAResults$vValue, c( mean( mSimsTmp[, "CurrentTime" ] ), quantile( mSimsTmp[, "CurrentTime" ], probs=c(0.025, 0.975) ) ) )

    return( lISAResults )

}

PostProcessSimulationSubgroupResults  <- function( mSims, nDes, nScen, vQtyTrt, dPUFinal = NA, bPrintStatus = FALSE  )
{

    mSimsTmp <- mSims[ mSims$iScen == nScen & mSims$Design == nDes, ]

    nQtyISA   <- length( vQtyTrt )

    if( bPrintStatus )
        print(paste( "Scen ", nScen, " Qty Reps ", nrow(mSimsTmp), " # ISA = ", nQtyISA ))

    nTreatmentStartNumber <- 2

    lISASubsetResults <- list( vWhat = c(), vISA = c(), vGroup=c(), vValue = c() )

    for( nISA in 1:nQtyISA )
    {
        lISASubsetResults     <- ProcessISASubgroupResults( mSimsTmp, nISA, lISASubsetResults  )
        #nTreatmentStartNumber <- lISASubsetResults$nNextTreatmentStartNumber

    }


    return( lISASubsetResults )

}


#####################################################################################################################.
#   For a given ISA, process the results and add to the lISAReults object.
#   By calling this function for each ISA the lISAReults will contain a dataframe structured for ggplot.
#   If dPUFinal = NA, the default, this function uses whatever was simulated.  If another value of dPUFinal
#   is supplied then it will compute new values for the OCs.
#####################################################################################################################.
ProcessISAResults <- function( mSims, nISANumber, nTreatmentStartNumber, nQtyTrtInISA, lISAResults, dPUFinal = NA )
{
    #browser()
    dAvePlacISA   <- mean( mSims[,paste("ISA", nISANumber, "Trt1", sep="") ] )

    vISATrt       <- seq( from = nTreatmentStartNumber, length = nQtyTrtInISA-1 )
    strTrt        <- paste( "ISA", nISANumber, "Trt", vISATrt, sep ="")
    vPats         <- mSims[,strTrt]
    #The next line is needed for multiple doses - Not needed for the first ISA
    #vPats         <- apply( vPats, 1, sum)
    dAveTrtISA    <- mean( vPats  )


    mTimeISA           <- mSims[,paste0(c( "ISAStart", "FinalPatientEnrolledTimeISA", "StartIAISA", "FinalAnalysisTimeISA") , nISANumber)]
    #Add the average time to decision in an ISA
    mTimeISA           <- cbind( mTimeISA[,1:4], mTimeISA[,paste0( "FinalAnalysisTimeISA" , nISANumber)] - mTimeISA[,paste0( "ISAStart" , nISANumber)]  )
    colnames( mTimeISA )[5] <- paste0( "TimeToDecision", nISANumber )

    vMeanTime          <- apply( mTimeISA, 2, mean  )
    vLowerLimit        <- apply( mTimeISA, 2, quantile, probs=c(0.025) )
    vUpperLimit        <- apply( mTimeISA, 2, quantile, probs=c(0.975) )
    lISAResults$vWhat  <- c( lISAResults$vWhat, c( "Ave N Placebo", "Ave N Treatment", "Ave N ISA",
                                                    paste0("Ave",c( "ISAStart", "FinalPatientEnrolledTimeISA", "StartIAISA", "FinalAnalysisTimeISA", "TimeToDecision") ),
                                                    paste0("LowerLimit",c( "ISAStart", "FinalPatientEnrolledTimeISA", "StartIAISA", "FinalAnalysisTimeISA", "TimeToDecision") ),
                                                    paste0("UpperLimit",c( "ISAStart", "FinalPatientEnrolledTimeISA", "StartIAISA", "FinalAnalysisTimeISA", "TimeToDecision") ) ) )
    lISAResults$vISA   <- c( lISAResults$vISA,  rep( paste( "ISA", nISANumber),18))
    lISAResults$vValue <- c( lISAResults$vValue, c( dAvePlacISA, dAveTrtISA, dAvePlacISA + dAveTrtISA,
                                                    vMeanTime, vLowerLimit, vUpperLimit ) )

    # Compute the decision probabilities.
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrollment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #ISA 1 status
    if( is.na( dPUFinal ) )
    {
        vISAStatus        <- c(mSims[ ,paste( "ISA", nISANumber, "Status", sep="") ], 3:7)
        vTableStatus      <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
        vTableStatus      <- vTableStatus/nrow( mSims )
    }
    else
    {
        #For the simulated trials that were decided at the end (eg vISAStatus = 5,6,7)
        # We can find the final PU to make it such that the Pr( Go ) == dTargetGoProb
        #nQtyTrtInISA-1 because we don't need placebo
        vTableStatus <- ComputeNewStatusTableProbs( mSims, nISANumber, nQtyTrtInISA-1, dPUFinal )
    }

    dProbGoAtIA       <- vTableStatus[1]
    dProbClosedIAISA  <- sum(vTableStatus[1:2])
    dProbFAISA        <- sum( vTableStatus[3:5])
    dProbGoAtIA       <- vTableStatus[1]
    dProbGoAtFA       <- vTableStatus[3]
    dProbGoISA        <- dProbGoAtIA + dProbGoAtFA
    dProbNoGoAtIA     <- vTableStatus[2]
    dProbNoGoAtFA     <- vTableStatus[4]
    dProbNoGoISA      <- dProbNoGoAtIA + dProbNoGoAtFA
    dProbPauseISA     <- 1.0 - dProbGoISA - dProbNoGoISA

    lISAResults$vWhat  <- c( lISAResults$vWhat, c(  "Pr(Go)", "Pr(Pause)","Pr(No Go)",
                                                    "Pr(Closed IA)", "Pr(FA)",
                                                    "Pr(Go @ IA)",     "Pr(Go @ FA)",
                                                    "Pr(No Go @ IA)", "Pr(No Go @ FA)"  ))
    lISAResults$vISA   <- c( lISAResults$vISA,  rep( paste( "ISA", nISANumber), 9 ) )
    lISAResults$vValue <- c( lISAResults$vValue, c( dProbGoISA, dProbPauseISA, dProbNoGoISA,
                                                    dProbClosedIAISA, dProbFAISA,
                                                    dProbGoAtIA,    dProbGoAtFA,
                                                    dProbNoGoAtIA, dProbNoGoAtFA  ) )


    lISAResults$nNextTreatmentStartNumber <- vISATrt[ length( vISATrt ) ] + 1

    return( lISAResults )

}



ProcessISASubgroupResults <- function( mSims, nISANumber,  lISASubsetResults )
{

    #' Decision= 3 The ISA was closed with a Go BEFORE Final Analysis (FA).  This can be a Final status for an ISA because it was closed.}
    #' Decision= 4 The ISA was closed with a No Go BEFORE FA.  This can be a Final status for an ISA because it was closed.}
    #' Decision= 5 The ISA was closed with a Go at FA.  This can be a Final status for an ISA because it was closed.}
    #' Decision= 6 The ISA was closed with a No Go at FA.  This can be a Final status for an ISA because it was closed.}
    #' Decision= 0 The ISA was closed with a Pause at FA.  This can be a Final status for an ISA because it was closed.}
    strGroupDec <- paste( "ISA", nISANumber, "lAnalysis1.dfPostCalcs.Decision", 1:4,sep="")

    mGroupDecisions <- mSims[ , strGroupDec ]
    nQtyGroups      <- 4
    mGroupDecisions <- rbind( mGroupDecisions,rep(3,nQtyGroups), rep(4,nQtyGroups), rep(5,nQtyGroups), rep(6,nQtyGroups), rep(0,nQtyGroups))

    mGroupDecisions          <- (apply( mGroupDecisions, 2, table)-1)/nrow(mGroupDecisions)
    lISASubsetResults$vWhat  <- c(lISASubsetResults$vWhat, rep(c("Indeterminate", "Go at IA", "No Go at IA", "Go at FA", "No Go at FA" ),4))
    lISASubsetResults$vISA   <- c(lISASubsetResults$vISA, rep( nISANumber, 5*nQtyGroups))
    lISASubsetResults$vGroup <- c(lISASubsetResults$vGroup, rep( c(1:nQtyGroups), rep(5,nQtyGroups)))
    lISASubsetResults$vValue <- c(lISASubsetResults$vValue, as.vector( mGroupDecisions ))
    return( lISASubsetResults )
    }

#####################################################################################################################.
#   Compute the new table of OCs given a dPUFinal this different than was used in the simulation step
#####################################################################################################################.

ComputeNewStatusTableProbs <- function( mSims, nISANumber, nQtyTrtInISA, dPUFinal )
{

    strISAStatus   <- paste( "ISA", nISANumber, "Status", sep="")


    vstrPrGrtMAV   <- paste( "ISA", nISANumber, "lAnalysis1.dPrGrtMAV", sep="")
    vstrPrGrtMAV   <- c( vstrPrGrtMAV, paste( "ISA", nISANumber, "lAnalysis1.dPrGrtMAV.", 1:(nQtyTrtInISA-1), sep=""))
    mPrGrt         <- mSims[ , c( strISAStatus,vstrPrGrtMAV)  ]

    vISAStatus     <- CalculateUpdatedStatus( dPUFinal, mPrGrt )
    vISAStatus     <- c( vISAStatus, 3:7)
    vTableStatus   <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus   <- vTableStatus/nrow( mSims )

    return( vTableStatus)
}

CalculateUpdatedStatus <- function(dPUFinal, mPrGrt  )
{
    vISAStatus       <- mPrGrt[ , 1]
    nCol             <- ncol( mPrGrt )
    vMaxPrGrt        <- apply( mPrGrt[,-1], 1, max)
    vISAStatus       <- ifelse( vISAStatus ==5, 7, vISAStatus )  #Need to make the default a pause so if PUFinal > what was run in design
    #It wont leave it as a Go
    vISAStatusUpdate <- ifelse( vISAStatus >=5 & vMaxPrGrt > dPUFinal, 5, vISAStatus )

    return( vISAStatusUpdate )

}


ComputeNewPUFinal <- function( mSims, nScen, nDes, nISANumber, nQtyTrtInISA, dTargetGoProb, dPUFinalCurrent )
{

    mSims             <- mSims[ mSims$iScen == nScen & mSims$Design == nDes, ]

    strISAStatus      <- paste( "ISA", nISANumber, "Status", sep="")
    vISAStatus        <- c(mSims[ , strISAStatus], 3:7)

    vstrPrGrtMAV      <- paste( "ISA", nISANumber, "lAnalysis1.dPrGrtMAV", sep="")
    vstrPrGrtMAV      <- c( vstrPrGrtMAV, paste( "ISA", nISANumber, "lAnalysis1.dPrGrtMAV.", 1:(nQtyTrtInISA-1), sep=""))
    mPrGrt            <- mSims[ , c( strISAStatus,vstrPrGrtMAV)  ]

    vTableStatus      <- table( vISAStatus ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus      <- vTableStatus/nrow( mSims )

    dProbGoAtIA       <- vTableStatus[1]
    dProbGoAtFA       <- vTableStatus[3]
    dProbGoISA        <- dProbGoAtIA + dProbGoAtFA
    dPUFinal          <- dPUFinalCurrent
    if( dProbGoAtIA < dTargetGoProb )  #Can adjust the final PU to get the target Pr( Go )
    {
        dPUFinal <- uniroot( CalculatePUFinal, c(0.01, 1.0), mPrGrt=mPrGrt, dTargetGoProb = dTargetGoProb  )
        dPUFinal <- dPUFinal$root

    }
    return( dPUFinal )
}


CalculatePUFinal <- function( dPUFinal, mPrGrt, dTargetGoProb )
{
    dProbGo <- CalculateProbGo( dPUFinal, mPrGrt )
    dVal    <- dProbGo- dTargetGoProb
    return( dVal )
}


CalculateProbGo <- function(dPUFinal, mPrGrt )
{

    vISAStatus        <- mPrGrt[ , 1]
    nCol              <- ncol( mPrGrt )
    # Compute the decision probabilities.
    #   0 = ISA not open;
    #   1 = ISA open,
    #   2 = met max enrollment,
    #   3 = Closed with a Go before FA,
    #   4 = Closed - No Go before FA
    #   5 = closed - Go at the FA
    #   6 = Closed - No Go at the FA
    #   7 = Closed - Pause at the FA
    #browser()
    vMaxPrGrt        <- apply( mPrGrt[,-1], 1, max)
    vISAStatus       <- ifelse( vISAStatus ==5, 7, vISAStatus )  #Need to make the default a pause so if PUFinal > what was run in design
    #It wont leave it as a Go
    vISAStatusUpdate <- ifelse( vISAStatus >=5 & vMaxPrGrt > dPUFinal, 5, vISAStatus )
    vISAStatusUpdate <- c( vISAStatusUpdate, 3:7)

    vTableStatus      <- table( vISAStatusUpdate ) - 1  #Subtracting 1 because the 3:7 added all possible status options
    vTableStatus      <- vTableStatus/nrow( mPrGrt )

    dProbGoAtIA       <- vTableStatus[1]
    dProbGoAtFA       <- vTableStatus[3]
    dProbGo <- dProbGoAtIA + dProbGoAtFA
    return( dProbGo )

}




GetLabel <- function( vValue )
{

    vValue <- round( vValue, 2)
    vRet <- ifelse(  vValue < 0.01, "", vValue)

    return( vRet )

}
GetY <- function( vY )
{
    # Original code with Pr(Pause)
    mY <- matrix(vY, nrow=3)
    mY[3, ] <- mY[1,] + mY[2, ] + mY[3,]/2
    mY[2, ] <- mY[1,] + mY[2, ]/2
    mY[1, ] <- mY[1,]/2
    vY <- as.vector( mY)


    # mY <- matrix(vY, nrow=2)
    # #mY[3, ] <- mY[1,] + mY[2, ] + mY[3,]/2
    # mY[2, ] <- mY[1,] + mY[2, ]/2
    # mY[1, ] <- mY[1,]/2
    # vY <- as.vector( mY)
    return( vY )

}



WriteMatrix <- function(strMatrixName, mMatrix) {
    begin <- paste( "$$", strMatrixName, " = \\begin{bmatrix}", sep="")
    end <- "\\end{bmatrix}$$"
    X <-
        apply(mMatrix, 1, function(x) {
            paste(
                paste(x, collapse = "&"),
                "\\\\"
            )
        })
    paste(c(begin, X, end), collapse = "")
}



