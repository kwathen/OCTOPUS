##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    plotServer <- eventReactive(input$goButton, {
        vPatsPerMonthPerSite1    <- as.numeric(unlist(strsplit(input$vPatsPerMonthPerSite1,",")))
        vQtySitesPOC             <- as.numeric(unlist(strsplit(input$vQtySitesPOC,",")))
        vQtySitesPlat1           <- as.numeric(unlist(strsplit(input$vQtySitesPlat1,",")))
        vQtySitesPlat2           <- as.numeric(unlist(strsplit(input$vQtySitesPlat2,",")))
        PlotPlatformRecruitment( input$nQtyReps, input$nMaxQtyPats, vPatsPerMonthPerSite1,
                                 input$dDelayToStartPOC,  input$dDelayBetweenTrialsPOC,  vQtySitesPOC,
                                 input$dDelayToStartPlat, input$dDelayBetweenTrialsPlat, vQtySitesPlat1, vQtySitesPlat2 )
    })

    output$normPlot <- renderPlot( {
        plotServer()
    })

})

