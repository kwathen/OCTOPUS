library( shiny)
library( shinydashboard)
library( shinyBS )
library( ggplot2 )

source( "ShinyUI.R" )
source( "ShinyServer.R" )

glVirtualTrial <<- NA
gBaseSize      <<- 18
#set.seed(123)

shinyApp(
    ui = dashboardPage(
        dashboardHeader( title="Compare Recruitment", titleWidth="250"),
        dashboardSidebar(disable=TRUE),
        body
    ),
    server = server
)
