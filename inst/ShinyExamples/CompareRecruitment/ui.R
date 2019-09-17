##### COPYRIGHT #############################################################################################################
#
# Copyright (C) 2018 JANSSEN RESEARCH & DEVELOPMENT, LLC
# This package is governed by the JRD OCTOPUS License, which is the
# GNU General Public License V3 with additional terms. The precise license terms are located in the files
# LICENSE and GPL.
#
#############################################################################################################################.

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Platform Trial Recruitment"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput( "nQtyReps", "Number of Virtual Trials: ", 100, step = 100),

            numericInput( "nMaxQtyPats", "Number of patients in POC/ISA: ", 100, step = 10),
            textInput( "vPatsPerMonthPerSite1", "Enter the number of patients enrolled per month per site ( , separated for a ramp-up)", value="0.1,0.3,0.45, 0.5" ),

            p( "Two POC Trials "),
            numericInput( "dDelayToStartPOC", "Delay (in months) to start POC: ", 6,1 ),
            numericInput( "dDelayBetweenTrialsPOC", "Delay (in months) between POC Trials: ", 3,1 ),
            textInput( "vQtySitesPOC", "Enter the number of sites open in POC each month ( , separated)", value="5,10,15,20,25,35" ),

            br(),
            p( "Platform - Two ISAs"),
            numericInput( "dDelayToStartPlat", "Delay (in months) to start platform: ", 6,1 ),
            numericInput( "dDelayBetweenTrialsPlat", "Delay (in months) between ISAs: ", 1,1 ),
            textInput("vQtySitesPlat1", "Enter the number of sites open in platform for ISA1, each month ( , separated)", value = "3,8,15,35,50,70"),
            textInput("vQtySitesPlat2", "Enter the number of sites open in platform for ISA2, each month ( , separated)", value = "7.5, 15, 23,30, 37.5, 70"),
            br(),
            actionButton("goButton", "Generate Plot"),
            p("Click the button to generate new plots."),
            width = 4
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("normPlot")
        )
    )
))
