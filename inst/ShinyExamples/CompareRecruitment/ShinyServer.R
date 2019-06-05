
library(shiny)

server <- function( input, output, session ){
    plotServer <- function()
    {
        vPatsPerMonthPerSite1    <- as.numeric(unlist(strsplit(input$vPatsPerMonthPerSite1,",")))
        vQtySitesPOC             <- as.numeric(unlist(strsplit(input$vQtySitesPOC,",")))
        vQtySitesPlat1           <- as.numeric(unlist(strsplit(input$vQtySitesPlat1,",")))
        vQtySitesPlat2           <- as.numeric(unlist(strsplit(input$vQtySitesPlat2,",")))
        plot1 <- ggPlotPlatformRecruitment( input$nQtyReps, input$nMaxQtyPats, vPatsPerMonthPerSite1,
                                 input$dDelayToStartPOC,  input$dDelayBetweenTrialsPOC,  vQtySitesPOC,
                                 input$dDelayToStartPlat, input$dDelayBetweenTrialsPlat, vQtySitesPlat1, vQtySitesPlat2 )
        output$ctrlPlotRecruitment <- renderPlot( print( plot1 ))

    }
    observeEvent( input$btnGeneratePlot, {
        updateButton( session, "btnGeneratePlot", label="Running Simulations", style="danger", block=F, disable=TRUE, size="large")
        glVirtualTrial <<- plotServer()
        updateButton( session, "btnGeneratePlot", label="Generate Plot", style="success", block=F, size="large", disable=FALSE)
        updateTabsetPanel(session, "inTabset", selected="plotPanel")
    })

}

