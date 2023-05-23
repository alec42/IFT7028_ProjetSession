

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "ERP/MES"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Planification", tabName = "planning", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName ="dashboard",
                box(plotOutput("plot1", height = 250)),
                box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50))
            ),

            tabItem(tabName ="planning",

            )
        )
    )
)