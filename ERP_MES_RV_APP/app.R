library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

source("scripts/googlesheets_access.R") # get link to gs

db_inventory <- read_sheet(link_gs, sheet = 'inventaire')
db_orders <- read_sheet(link_gs, sheet = 'commandes')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Gestion des commandes de RV"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Inventaire", tabName = "planning", icon = icon("th")),
            menuItem("Commandes", tabName = "orders", icon = icon("th")),
            menuItem("Suivi des commandes", tabName = "monitoring", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName ="dashboard",
                    plotlyOutput('plot'),
                    selectInput('xcol','X Variable', names(db_inventory))
            ),

            tabItem(tabName ="planning",
                    DT::dataTableOutput("inventaire")
            ),

            tabItem(tabName ="orders",
                    DT::dataTableOutput("orders_db")
            ),

            tabItem(tabName ="monitoring",

            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$inventaire <- DT::renderDataTable(db_inventory)
    output$orders_db <- DT::renderDataTable(db_orders)

    x <- reactive({input$xcol})

    output$plot <- plotly::renderPlotly({
        db_inventory %>% plotly::plot_ly(y=x(),
            type = 'histogram')
    })


}

shinyApp(ui, server)