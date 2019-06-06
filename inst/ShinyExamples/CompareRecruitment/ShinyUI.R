
body <- dashboardBody(
    tabsetPanel(id="inTabset",
        tabPanel("Design Input",
                 fluidPage(
                     fluidRow( column(width=12,box( width="400px", title = "Common Input",
                                           numericInput( "nQtyReps", "Number of Virtual Trials: ", 100, step = 100),
                                           numericInput( "nMaxQtyPats", "Number of patients in POC/ISA: ", 90, step = 10),
                                           textInput( "vPatsPerMonthPerSite1", "Enter the number of patients enrolled per month per site ( , separated for a ramp-up)", value="0.05, 0.1, 0.15, 0.15" )
                     ) )),
                     fluidRow( column( width=6, box( width="400px", title = "2 POCs",
                                             numericInput( "dDelayToStartPOC", "Delay (in months) to start POC: ", 3,1 ),
                                             numericInput( "dDelayBetweenTrialsPOC", "Delay (in months) between POC Trials: ", 6,1 ),
                                             textInput( "vQtySitesPOC", "Enter the number of sites open in POC each month ( , separated)", value="2, 4, 8, 10, 20, 40, 60" )
                     ))
                     , column(width=6,
                     box( width="400px", title = "Platform Trial - 2 ISAs",
                          numericInput( "dDelayToStartPlat", "Delay (in months) to start platform: ", 6,1 ),
                          numericInput( "dDelayBetweenTrialsPlat", "Delay (in months) between ISAs: ", 1,1 ),
                          textInput("vQtySitesPlat1", "Enter the number of sites open in platform for ISA1, each month ( , separated)", value = "1.6,  3.2,  6.4,  8.0, 16.0, 32.0,  48.0, 54, 72, 90"),
                          textInput("vQtySitesPlat2", "Enter the number of sites open in platform for ISA2, each month ( , separated)", value = "2.6,  5.2, 10.4, 13.0, 26.0, 52.0,  78.0, 90")

                     ))
                 ), bsButton( "btnGeneratePlot", label="Generate Plot", style="success", block=F, size="large")
        ), value="inputPanel"),
        tabPanel("Recruitment Plots", value="plotPanel",
                 plotOutput("ctrlPlotRecruitment", height="400px")
        )
    )
)

