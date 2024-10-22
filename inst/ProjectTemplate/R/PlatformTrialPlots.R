# library( tidyverse)
# dStartDate <- ymd( "2024-01-01")
# dLastDate <- ymd( "2027-06-01")
# strTitle <- "Trial"
#' @export
TimeLinePlotTrial <- function( dfTrial, dStartDate, dLastDate,  strTitle = ""   )
{
    # Need the long version
    
    
    vWhat    <- c( "ISAStart1", "ISAStart2", "ISAStart3", 
                   "FinalPatientEnrolledTimeISA1", "FinalPatientEnrolledTimeISA2", "FinalPatientEnrolledTimeISA3",
                   "FinalAnalysisTimeISA1", "FinalAnalysisTimeISA2", "FinalAnalysisTimeISA3",
                   "StartIAISA1","StartIAISA2","StartIAISA3")
    
    dfTrialTmp <- select ( dfTrial, ISAStart1, ISAStart2, ISAStart3, FinalPatientEnrolledTimeISA1, FinalPatientEnrolledTimeISA2, FinalPatientEnrolledTimeISA3,
                           FinalAnalysisTimeISA1, FinalAnalysisTimeISA2, FinalAnalysisTimeISA3,
                           StartIAISA1,StartIAISA2,StartIAISA3)
    # Create long format
    dfSimResults <- dfTrialTmp %>% 
        pivot_longer( cols = all_of(vWhat), names_to = "what", values_to = "value")
    
    
    dfSimResults$isa <- as.factor( rep( c(1,2,3),4) )
    
    dfSimResults <- dplyr::arrange( dfSimResults, isa)
    
    dfSimResults$what <- rep( c( "ISAStart", "FinalPatientEnrolledTimeISA", "FinalAnalysisTimeISA","StartIAISA" ),3)
    dfSub <- dfSimResults   %>% mutate( valueInDays = value *365/12, date=dStartDate + valueInDays,
                                                                                                                    # position = c(0.9,0.9,0.9,0.9,-1,-1,-1,-1,0.5,0.5,0.5,0.5),
                                                                                                                    position = c( 1,1,1,1,-1,-1,-1,-1,0.5,0.5,0.5,0.5),
                                                                                                                    direction = c( 1, 1,1,1, -1, -1, -1,-1 ,1, 1,1,1 ),
                                                                                                                    label = case_when(
                                                                                                                        what == "ISAStart" ~ "Start",
                                                                                                                        what == "FinalPatientEnrolledTimeISA" ~ "Enrollement\nEnd",
                                                                                                                        what == "FinalAnalysisTimeISA" ~ "Final\nAnalysis",
                                                                                                                        what == "StartIAISA" ~ "Interim\nAnalysis"  ))
    
    
    #if( all( vDesigns <= 12 ) )
    #    dfSub <- dplyr::filter( dfSub, what != "AveStartIAISA")
    
    month_buffer <- 2
    
    month_date_range <- seq(min(dfSub$date) - months(month_buffer), dLastDate, by ='2 month' ) # max(dfSub$date) + months(month_buffer), by='2 month')
    month_format <- format(month_date_range, '%b')
    month_df <- data.frame(month_date_range, month_format)
    
    year_date_range <- seq(min(dfSub$date) - months(month_buffer), dLastDate, by='year')
    year_date_range <- as.Date(
        intersect(
            ceiling_date(year_date_range, unit="year"),
            floor_date(year_date_range, unit="year")
        ),  origin = "1970-01-01"
    )
    year_format <- format(year_date_range, '%Y')
    year_df <- data.frame(year_date_range, year_format)
    
    text_offset <- 0.02
    
    dfSub$month_count <- ave(dfSub$date==dfSub$date, dfSub$date, FUN=cumsum)
    dfSub$text_position <- (dfSub$month_count * text_offset * dfSub$direction) + dfSub$position
    
    if(is.na( strTitle) )
    {
        strTitle <- "" 
    }else if(  strTitle == "" ){
        
        strTitle <- paste0( "Design ",vDesigns, " " )
    }else{
        strTitle <- paste0( "Design: ", strTitle )
    }
    
    vHjust <- 0.5 + sign(dfSub$text_position)*0.5
    
    gPlot <- ggplot( dfSub, aes( x = date, y = 0, col=isa, label = what) ) +
        theme_classic( ) +
        #scale_color_manual( values = unlist(get_graf_colours( paste0( "kelly_", c(1,2,4) ) ) ) ) +
        ggtitle( strTitle ) +
        geom_hline( yintercept = 0, color = "black", linewidth = 0.3) +
        geom_segment( aes( y = position, yend = 0, xend =date) ) +
        geom_point( aes( y = 0), size=3) +
        theme(axis.line.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x =element_blank(),
              axis.ticks.x =element_blank(),
              axis.line.x =element_blank(),
              legend.position = "bottom",
              plot.title = element_text(hjust = 0.5)) +
        geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=4.5,vjust=0.5, color='black', angle=90) +
        geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=4.5, color='black') +
        geom_text(aes(y=text_position,label=label),size=4.5, angle=90, hjust = vHjust ) +
        guides( color=guide_legend( title =""))
    # gPlot <- ggplotly( gPlot )
    #browser()
    # gPlot <- gPlot %>% 
    #     layout(
    #         annotations = list(
    #             list(
    #                 x = month_df$month_date_range,
    #                 y = -0.1,
    #                 text = month_df$month_format,
    #                 showarrow = FALSE,
    #                 xanchor = "center",
    #                 yanchor = "middle",
    #                 textangle = 90,
    #                 font = list(size = 12)
    #             ),
    #             list(
    #                 x = month_df$year_date_range,
    #                 y = -0.2,
    #                 text = month_df$year_format,
    #                 showarrow = FALSE,
    #                 xanchor = "center",
    #                 yanchor = "middle",
    #                 textangle = 90,
    #                 font = list(size = 12)
    #             )
    #         )
    #     )
    return( gPlot )
    
}


#TimeLinePlotTrial( dfTrial, dStartDate, dLastDate, vDesigns = c(25), scenario = 3)

#' @export
TimeLinePlot <- function( dfSimResults, dStartDate, dLastDate = NA, vDesigns, strTitle = "" , nScenario = 1  )
{
    vWhat    <- c( "AveISAStart", "AveFinalPatientEnrolledTimeISA","AveFinalAnalysisTimeISA", "AveStartIAISA")
    dfSub    <- dplyr::filter( dfSimResults,  design %in% vDesigns, scenario ==nScenario, what %in% vWhat )   
    
    dfAveTimeToDecision <- filter( dfSimResults, what =="AveTimeToDecision", design %in% vDesigns, scenario ==nScenario ) %>% 
                            dplyr::mutate( value = round( value, 1))
    #vPosition <- c( 1,1,1,1,-1,-1,-1,-1,0.5,0.5,0.5,0.5, rep( -0.5, 4))
    #vDirection <- c( 1, 1,1,1, -1, -1, -1,-1 ,1, 1,1,1, rep(-1,4) )
    
    
    nQtyISAs   <- length( unique( dfSub$isa ))
    vPosition  <- rep( c( 1, -1, 0.5, -0.5 ), nQtyISAs) # Each element is a position for labeling for an ISA, this will be longer than needed but subset next
    vPosition  <- vPosition[ 1:nQtyISAs ]
    
    nQtyItems  <- length( vWhat )
    vPosition  <- rep( vPosition, rep( nQtyItems, nQtyISAs))  # Need the position of each of the items selected above
    vDirection <- sign( vPosition )
    
    
    dfSub <- dplyr::mutate( dfSub, valueInDays = value *365/12, date=dStartDate + valueInDays,
                               # position = c(0.9,0.9,0.9,0.9,-1,-1,-1,-1,0.5,0.5,0.5,0.5),
                            position = vPosition,
                            direction = vDirection,
                            label = case_when(
                                what == "AveISAStart" ~ "Start",
                                what == "AveFinalPatientEnrolledTimeISA" ~ "Enrollement\nEnd",
                                what == "AveFinalAnalysisTimeISA" ~ "Final\nAnalysis",
                                what == "AveStartIAISA" ~ "Interim\nAnalysis"  ))
    
    dfSubIA   <- dplyr::filter( dfSub, what == "AveStartIAISA")
    dfSubFA   <- dplyr::filter( dfSub, what == "AveFinalAnalysisTimeISA")
    if( all( dfSubIA$value == dfSubFA$value )) 
    {
        # The times of IA and FA are the same, so don't want to show the IA
        dfSub <- dplyr::filter( dfSub, what !="AveStartIAISA")
    }
    
    #if( all( vDesigns <= 12 ) )
    #    dfSub <- dplyr::filter( dfSub, what != "AveStartIAISA")
    
    month_buffer <- 0
    
    if( is.na( dLastDate ) )
        dLastDate <- max(dfSub$date) 
    
    month_date_range <- seq(min(dfSub$date) - months(month_buffer), dLastDate, by ='4 month' ) # max(dfSub$date) + months(month_buffer), by='2 month')
    month_format <- format(month_date_range, '%b')
    month_df <- data.frame(month_date_range, month_format)
    
    year_date_range <- seq(min(dfSub$date) - months(month_buffer), dLastDate, by='year')
    year_date_range <- as.Date(
        intersect(
            ceiling_date(year_date_range, unit="year"),
            floor_date(year_date_range, unit="year")
        ),  origin = "1970-01-01"
    )
    year_format <- format(year_date_range, '%Y')
    year_df <- data.frame(year_date_range, year_format)
    
    text_offset <- 0.02
    
    dfSub$month_count <- ave(dfSub$date==dfSub$date, dfSub$date, FUN=cumsum)
    dfSub$text_position <- (dfSub$month_count * text_offset * dfSub$direction) + dfSub$position
    
    if(is.na( strTitle) )
    {
        strTitle <- "" 
    }else if(  strTitle == "" ){
        
        strTitle <- paste0( "Design ",vDesigns, " " )
    }else{
        #strTitle <- paste0( "Design: ", strTitle )
    }
    
    vHjust <- 0.5 + sign(dfSub$text_position)*0.5
    
    gPlot <- ggplot( dfSub, aes( x = date, y = 0, col=isa, label = what) ) +
                theme_classic( ) +
                #scale_color_manual( values = unlist(get_graf_colours( paste0( "kelly_", c(1,2,4) ) ) ) ) +
                ggtitle( strTitle ) +
                geom_hline( yintercept = 0, color = "black", linewidth = 0.3) +
                geom_segment( aes( y = position, yend = 0, xend =date) ) +
                geom_point( aes( y = 0), size=3) +
                theme(axis.line.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.text.x =element_blank(),
                      axis.ticks.x =element_blank(),
                      axis.line.x =element_blank(),
                      legend.position = "bottom",
                      plot.title = element_text(hjust = 0.5)) +
                geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=4.5,vjust=0.5, color='black', angle=90) +
                geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=4.5, color='black') +
                geom_text(aes(y=text_position,label=label),size=2.5, angle=90, hjust = vHjust ) +
                guides( color=guide_legend( title ="")) 
    
    
    # Add text to the plot
    gPlot <- gPlot + annotate("text", x = dStartDate, y = -0.8, 
                      label = paste0("Ave. Time to\nDecision\n",paste0(dfAveTimeToDecision$isa, ": ", dfAveTimeToDecision$value, collapse ="\n") ), 
                      size = 3, color = "red", hjust = 0)
   
   # gPlot <- ggplotly( gPlot )
    #browser()
    # gPlot <- gPlot %>% 
    #     layout(
    #         annotations = list(
    #             list(
    #                 x = month_df$month_date_range,
    #                 y = -0.1,
    #                 text = month_df$month_format,
    #                 showarrow = FALSE,
    #                 xanchor = "center",
    #                 yanchor = "middle",
    #                 textangle = 90,
    #                 font = list(size = 12)
    #             ),
    #             list(
    #                 x = month_df$year_date_range,
    #                 y = -0.2,
    #                 text = month_df$year_format,
    #                 showarrow = FALSE,
    #                 xanchor = "center",
    #                 yanchor = "middle",
    #                 textangle = 90,
    #                 font = list(size = 12)
    #             )
    #         )
    #     )
    return( gPlot )
    
}


#' @export
PlotOCsForDesignsGivenISA <- function( dfSimResults, vDesigns, strISA, strWhat = "Pr(Go)", ylab = "Probability of Go" ,  strGroupVariable = "designLabel")
{
    
    # Plot line graphs for a set of designs and a specific ISA ####
    #vDesigns <- c(1,2,3,4)
    #strISA   <- "ISA 3"
    
    dfSub <- dplyr::filter( dfSimResults, isa== strISA, design %in% vDesigns , what==strWhat)
    
    # if( all( is.na( vDesignLabels ) ) )
    # {
    #     vDesignLabels <- unique( dfSub$designLabel )
    #     
    # }
    
    colorPalette<- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                     "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
                     "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499")
    
    gPlot <- ggplot( dfSub, aes_string( x ="TreatmentEffect", y ="value", group = strGroupVariable, colour=strGroupVariable) ) +
            geom_line( linewidth=1.5) + 
            theme_bw() +
            #scale_color_manual(values = colorPalette[1:length(vDesigns)] ) +#,labels = vDesignLabels  ) +
            scale_color_grafify( palette = "kelly" )  + #(values = colorPalette[1:length(vDesigns)] ) +#,labels = vDesignLabels  ) +
            #scale_fill_manual( values = colorPalette) +
            theme(plot.title =element_text(hjust=0.5), 
                  plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(linewidth=1.5)) +
            theme( legend.position  ="top", legend.direction="horizontal" ) +
            #xlab("Treatment Effect") + 
            #ylab( ylab ) +
            labs( title = paste0( strISA), x = "Treatment Effect", y = ylab, color= "Design ") +
            #ggtitle( paste0( strISA))  +
            scale_y_continuous( minor_breaks= seq( 0, 1, 0.05), limits = c( 0, 1.0), breaks= seq( 0.0, 1, 0.1))
    
    #return( ggplotly( gPlot ) )
    return( gPlot )
}


#' @export
PlotOCsForAllISAsInDesign <- function( dfSimResults, nDesign,  strWhat = "Pr(Go)", ylab = "Probability of Go" )
{

    
    # For a given design plot by ISA across scenarios ####
    dfISA <- dplyr::filter( dfSimResults, design==nDesign, what== strWhat) # %>% left_join( dfScen, by = "scenario") %>% mutate(design=factor(design))
    
    
    colorPalette<- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                     "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
                     "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499")
    
    gPlot <- ggplot( dfISA, aes( x =TreatmentEffect, y =value, group = isa, colour=isa)) +
                geom_line( linewidth=1.5) + 
                theme_bw() +
                #scale_color_discrete("ISA ",labels = dfISA$isa ) +
                #scale_color_manual(values = colorPalette[1:3],labels = unique( dfISA$isa )  ) +
                scale_color_grafify( palette = "kelly" )  +
                theme(plot.title =element_text(hjust=0.5), 
                      plot.subtitle = element_text(hjust = 0.5), panel.grid.major.y = element_line(linewidth=1.5)) +
                theme( legend.position  ="top", legend.direction="horizontal" ) +
                labs( title = paste0( strISA), x = "Treatment Effect", y = ylab, color= "Design ") +
                # 
                # xlab("Treatment Effect") + 
                # ylab(  ylab) +
                # ggtitle( paste0( "Design ", nDesign )) +
                scale_y_continuous( minor_breaks= seq( 0, 1, 0.05), limits = c( 0, 1.0), breaks= seq( 0.0, 1, 0.1))
    
    return( gPlot )
}

#' @export
PlotOCs <- function( dfSimResults, strWhat = "Pr(Go)", strTitle = "Probability of Go", ylab = "Probability of Go", xlab = "Ratio", colorPalette = NA)
{
    if( is.na( colorPalette ))
    {
        
        
        colorPalette<- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                         "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
                         "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499")
        
    }
    
    dfSimResults$design <- as.factor( dfSimResults$design)
    dfSimResults <- dplyr::filter( dfSimResults, what == strWhat)
   # browser()
    
    if( length( unique( dfSimResults$vProbResponse2)) > 1 )
    {
        xlab <- "Pr( Treatement Fail Exp )"
        ggplot( dfSimResults, aes( x = vProbResponse2, y = value, group = design, color = design  )) +
            geom_line( linewidth = 1.5) +
            theme_bw() + 
            scale_color_manual(values = colorPalette) +
            theme_minimal() +
            theme( plot.title = element_text(hjust = 0.5, size = 14) )   +
            #theme( plot.title =element_text(hjust=0.5))S
            facet_wrap(~isa, scales = "free_y", ncol = 1) +
            labs(title = strTitle, x = xlab, y = ylab, color="Design")
        
    }
    else if(  length( unique( dfSimResults$SafetyRatio) ) > 1) 
    {
        xlab <- "True Safety Ratio"
        ggplot( dfSimResults, aes( x = SafetyRatio, y = value, group = design, color = design  )) +
            geom_line( linewidth = 1.5) +
            theme_bw() + 
            scale_color_manual(values = colorPalette) +
            theme_minimal() +
            theme( plot.title = element_text(hjust = 0.5, size = 14) )   +
            #theme( plot.title =element_text(hjust=0.5))
            facet_wrap(~isa, scales = "free_y", ncol = 1) +
            labs(title = strTitle, x = xlab, y = ylab, color="Design")
        
    }
       
    
    
}

#' @export
PlotExampleTrialInfo <- function( dfResSub, dfSimResAll, nDesign, nScen, nTrialIndex )
{
    # Timeline example of 1 trial
    
    dStartDate <- ymd( "2024-01-01")
    dLastDate <- ymd( "2027-06-01")
    
    dfResSubTmp <<- dplyr::filter( dfResSub, design == nDesign, scenario == nScen )
    
    #dPL <- dfResSubTmp[ dfResSubTmp$design == nDesign, ]$PL[1]
    dPL <- dfResSubTmp$PL[1]
    
    print(paste( "In plots - PL ", dPL, " Design ", nDesign, " Scen ", nScen  ))
    gTimeLinePlot <- TimeLinePlot( dfResSubTmp, dStartDate, dLastDate, as.numeric( nDesign ), paste0( dfResSubTmp$designLabel[1], " PL = ", dPL ), nScenario = nScen ) 
    #gTimeLinePlot
    
    
    # Selected all simulated trials for this design, scenario
    dfSimResAllSub <- dplyr::filter( dfSimResAll, Design == nDesign, iScen == nScen )
    
    dfTrial <- dfSimResAllSub[ nTrialIndex, ]
    dfTrial <<- dfTrial
    
    gTrialTimeLine <- TimeLinePlotTrial( dfTrial, dStartDate, dLastDate, "Trial")
    lReturn <- list( TimelinePlot      = gTimeLinePlot )
    
    nISA <- 1
    for( nISA in 1:3 )
    {
        lISARet <- PlotExampleTrialISAPost( dfTrial, nISA )
        lReturn[[paste0("ISA", nISA ) ]] <- lISARet 
    }
    lReturn$TrialTimelinePlot <- gTrialTimeLine
    return( lReturn )
    
}



PlotExampleTrialISAPost <- function( dfTrial, nISA )
{
    
    vRespRate <- seq( 0.005, 0.995, by = 0.005)
    vDesiredCI  <- c( 0.025, 0.975 )
    
    dAlphaCtrl <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.dPostACtrl") ]
    dBetaCtrl  <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.dPostBCtrl") ]
    dAlphaExp  <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.dPostATrt" ) ]
    dBetaExp   <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.dPostBTrt" ) ]
    dPostProbGr <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.dPrGrtMAV" ) ]
    
    nQtyRespCtrl <- dAlphaCtrl -0.141
    nQtyPatsCtrl <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.nNCtrl" ) ]
    nQtyExtCtrl  <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.nNExtCtrls" ) ]
    
    nQtyRespExp  <- dAlphaExp - 0.141
    nQtyPatsExp  <- dfTrial[ ,paste0( "ISA", nISA, "lAnalysis1.nNTrt" ) ]
    
    
    vPiCtrlDen <- dbeta( vRespRate, dAlphaCtrl, dBetaCtrl)
    vPiTrtDen  <- dbeta( vRespRate, dAlphaExp, dBetaExp)
    
    vCICtrl    <- qbeta( vDesiredCI, dAlphaCtrl, dBetaCtrl )
    vCITrt     <- qbeta( vDesiredCI, dAlphaExp,  dBetaExp  )
    
    dfCtrlPost <- data.frame( Pi = vRespRate, Den = vPiCtrlDen, Trt = 'Ctrl')
    dfTrtPost  <- data.frame( Pi = vRespRate, Den = vPiTrtDen, Trt = 'Exp')
    
    dfPostDen    <- dfCtrlPost %>% dplyr::add_row( dfTrtPost )
    
    gPlot1 <- ggplot( dfPostDen, aes( Pi, Den, group = Trt, fill = Trt )) +
        geom_line( linewidth = 0.75) +
        geom_ribbon( data=subset( dfPostDen, (Trt =='Ctrl' & Pi > vCICtrl[ 1 ] & Pi < vCICtrl[2] ) | 
                                      (Trt =='Exp'  & Pi > vCITrt[ 1 ] & Pi < vCITrt[ 2 ]) ) , 
                     aes( x=Pi, ymax = Den), ymin= 0, alpha =0.3) +
        scale_fill_manual(name='', values=c("Ctrl" = "#E69F00", "Exp" = "#56B4E9")) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5), legend.position="none" ) +
        ggtitle( paste0( "ISA ", nISA, "\nPosterior Disttribution with ", (vDesiredCI[2] - vDesiredCI[ 1 ])*100,"% CI ( ", 100* vDesiredCI[ 1 ], "%, ", 100* vDesiredCI[ 2 ], "% ) \n Brown (Control), Blue (Experimental)"  )) +
        xlab( paste0( "Response Rate" ) )  +
        ylab( "Density") +
        scale_x_continuous(breaks=seq(0,1,by=.1), minor_breaks=seq( 0,1,by=.1),limit=c(0,1) )+
        annotate( geom= "text", x=0.5, y = max(dfPostDen$Den), size = 7, vjust=1, hjust=0,  label= "Example Data") +
        annotate( geom="text", x=0.5, y =0.94*max(dfPostDen$Den), size = 7, vjust=1, hjust=0, label=paste0( "Ctrl: ", nQtyRespCtrl, "/", nQtyPatsCtrl, "=",round( 100*nQtyRespCtrl/nQtyPatsCtrl,1), "%", 
                                                                                                            " (Ext. Ctrl. = ",nQtyExtCtrl,  ") \n",
                                                                                                            "Exp: ",nQtyRespExp, "/", nQtyPatsExp, "=", round( 100*nQtyRespExp/nQtyPatsExp,1), "%"  )) 
    
    
    
    
    vPiCtrl    <- rbeta( 100000, dAlphaCtrl, dBetaCtrl )
    vPiExp     <- rbeta( 100000,  dAlphaExp, dBetaExp )
    vDelta     <- vPiExp - vPiCtrl 
    vRatio     <- vPiExp/vPiCtrl
    
    # Plot the distribution of delta pi_E-pi_C
   # vDen       <- density( vDelta )
    
    # For this trial we are intersted in pi_E - pi_C < dC1 because event is a bad thing
    dC1         <- 0.1
    dfDen       <- density( vDelta )
    dProbLess   <- mean( vDelta < dC1 )
    vCI         <- round( quantile( vDelta, probs= vDesiredCI ), 3)
    dfDen       <- data.frame( Delta = dfDen$x, Density = dfDen$y )
    
    dfDen$Grp <- ifelse( dfDen$Delta <= dC1, "1", "2")
    
    gPlot2 <- ggplot( dfDen, aes( x=Delta, y = Density, group=Grp, fill = Grp) ) +
        geom_line( linewidth = 0.75) +
        geom_ribbon( data=subset( dfDen, Grp =1), aes( x=Delta, ymax = Density), ymin= 0, alpha =0.3)+
        scale_fill_manual(name='', values=c("1" = "green4", "2" = "red"))+
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5), legend.position="none" ) +
        ggtitle( paste0( "ISA ", nISA, "\nEst. Delta = ", round( mean( vDelta), 3), 
                         "\nProbability of Delta < ", dC1, " = ", round( 100* dProbLess, 1 ), 
                         "%\n", (vDesiredCI[2] - vDesiredCI[ 1 ])*100,"% CI (", 100* vDesiredCI[ 1 ], "%, ", 100* vDesiredCI[ 2 ], 
                         "%) : ( ",vCI[1], ", ", vCI[2], " )" )) +
        xlab( paste0( "Delta (Event Rate E - Event Rate Control)" ) ) +
        annotate( geom= "text", x=min(dfDen$Delta), y = max(dfDen$Density), size = 7, vjust=1, hjust=0,  label= "Example Data") +
        annotate( geom="text", x=min(dfDen$Delta), y =max(dfDen$Density)*0.94, size = 7, vjust=1, hjust=0, 
                  label=paste0( "Ctrl: ", nQtyRespCtrl, "/", nQtyPatsCtrl, "=",round( 100*nQtyRespCtrl/nQtyPatsCtrl,1), "%", 
                                " (Ext. Ctrl. = ",nQtyExtCtrl,  ") \n",
                                "Exp: ",nQtyRespExp, "/", nQtyPatsExp, "=", round( 100*nQtyRespExp/nQtyPatsExp,1), "%"  )) 
    
    
    
    # Plot the distribution of ratio pi_E/pi_C
 
    
    
    dC1         <- 1.54
    dfDen       <- density( vRatio )
    dProbLess   <- mean( vRatio < dC1 )
    vCI         <- round( quantile( vRatio, probs= vDesiredCI ), 3)
    dfDen       <- data.frame( Delta = dfDen$x, Density = dfDen$y )
    
    dfDen$Grp <- ifelse( dfDen$Delta <= dC1, "1", "2")
    
    gPlot3 <- ggplot( dfDen, aes( x=Delta, y = Density, group=Grp, fill = Grp) ) +
        geom_line( linewidth = 0.75) +
        geom_ribbon( data=subset( dfDen, Grp =1), aes( x=Delta, ymax = Density), ymin= 0, alpha =0.3)+
        scale_fill_manual(name='', values=c("1" = "green4", "2" = "red"))+
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5), legend.position="none" ) +
        ggtitle( paste0( "ISA ", nISA, "\nEst. Ratio = ", round( mean( vRatio), 3), 
                         "\nProbability of Ratio < ", dC1, " = ", round( 100* dProbLess, 1 ), 
                         "%\n", (vDesiredCI[2] - vDesiredCI[ 1 ])*100,"% CI (", 100* vDesiredCI[ 1 ], "%, ", 100* vDesiredCI[ 2 ], 
                         "%) : ( ",vCI[1], ", ", vCI[2], " )" )) +
        xlab( paste0( "Ratio (Event Rate E/Event Rate Control)" ) ) +
        annotate( geom= "text", x=min(dfDen$Delta), y = max(dfDen$Density), size = 7, vjust=1, hjust=0,  label= "Example Data") +
        annotate( geom="text", x=min(dfDen$Delta), y =max(dfDen$Density)*0.94, size = 7, vjust=1, hjust=0, 
                  label=paste0( "Ctrl: ", nQtyRespCtrl, "/", nQtyPatsCtrl, "=",round( 100*nQtyRespCtrl/nQtyPatsCtrl,1), "%", 
                                " (Ext. Ctrl. = ",nQtyExtCtrl,  ") \n",
                                "Exp: ",nQtyRespExp, "/", nQtyPatsExp, "=", round( 100*nQtyRespExp/nQtyPatsExp,1), "%"  )) 
    
    
    
    lReturn <- list( PlotTreatmentPost = gPlot1,
                     PlotDeltaPost     = gPlot2,
                     PlotRatioPost     = gPlot3 )
    return( lReturn )
}