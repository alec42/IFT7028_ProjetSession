library(tidyverse)
library(plotly)
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

# db_orders <- read_sheet(link_gs, sheet = 'commandes_clients') |>
#     mutate(Statut=as.factor(Statut))

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
        shinydashboard::dropdownMenuOutput("ordersToApprove")
    ),
    sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
            shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("th")),
            shinydashboard::menuItem("Commandes client", tabName = "clientOrders", icon = icon("th")),
            shinydashboard::menuItem("Commandes fournisseurs", tabName = "purchaseOrders", icon = icon("th")),
            shinydashboard::menuItem("Production journalière", tabName = "dailyProduction", icon = icon("th")),
            shinydashboard::menuItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("th"))
        )
    ),
    body = shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(tags$style(HTML("
            .box {overflow: scroll;}"))
        ),
        shinydashboard::tabItems(
            shinydashboard::tabItem(tabName ="dashboard"
            ),

            shinydashboard::tabItem(tabName ="inventory",
                    shiny::actionButton("refreshBtn", "Update"),
                    shiny::actionButton("saveBtn", "Save"),
                    DT::dataTableOutput('CustomerOrders_DT')
            ),

            shinydashboard::tabItem(tabName ="clientOrders",
                    shiny::fluidRow(
                        shinydashboardPlus::box(title = "Commandes en cours", width = 6
                        ),
                        shinydashboardPlus::box(title = "Commandes en attente de matériaux", width = 6
                        )
                    ),
                    shiny::fluidRow(
                        shinydashboardPlus::box(title = "Commandes complétées", width = 6
                        ),
                        shinydashboardPlus::box(title = "Commandes en attente", width = 6
                        )
                    )
            ),

            shinydashboard::tabItem(tabName ="purchaseOrders",

            ),

            shinydashboard::tabItem(tabName ="dailyProduction",

            ),

            shinydashboard::tabItem(tabName ="weeklyProduction",

            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    source("scripts/googlesheets_access.R", local = TRUE) # get link to gs
    source("scripts/interactiveDT.R", local = TRUE)

    ### Reactive DF for Customer Orders
    ### - Code tiré https://stackoverflow.com/questions/75948475/value-not-updated-in-shiny-using-dt-and-drop-down-selection/75950123#75950123
    # Import Data from Google Sheets
    InteractiveDT(link_gs, "commandes_clients", "Statut", input, output, session)
    shiny::observeEvent(input$refreshBtn, {
        InteractiveDT(link_gs, "commandes_clients", "Statut", input, output, session)
    })

    # output$orders_db <- DT::renderDT({
    #     datatable(data = db_orders, rownames = FALSE, selection = 'none',
    #           editable = list(target="row", disable = list(columns =seq_along(db_orders)[names(db_orders)!= "Statut"]-1))
    #     )
    # })


}

shinyApp(ui, server)