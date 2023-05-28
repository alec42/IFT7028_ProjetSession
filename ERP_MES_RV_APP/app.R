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
        shinydashboard::sidebarMenu(id="sidebarMenu",
            shinydashboard::menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
            shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("th")),
            shinydashboard::menuItem("Commandes client", tabName = "clientOrders", icon = icon("th"),
                 shinydashboard::menuSubItem("Commandes", tabName = "clientOrders_all"),
                 shinydashboard::menuSubItem("Commandes complétées", tabName = "clientOrders_completed"),
                 shinydashboard::menuSubItem("Commandes en attente", tabName = "clientOrders_wait")
            ),
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

            shinydashboard::tabItem(tabName ="inventory"
            ),

            shinydashboard::tabItem(tabName ="clientOrders_all",
                    shiny::actionButton("refreshBtn", "Update"),
                    shiny::actionButton("saveBtn", "Save"),
                    DT::dataTableOutput('CustomerOrders_DT')
            ),
            shinydashboard::tabItem(tabName = "clientOrders_completed",
                shiny::fluidRow(
                    shinydashboardPlus::box(title = "Commandes en cours", width = 6
                    ),
                    shinydashboardPlus::box(title = "Commandes en attente de matériaux", width = 6
                    )
                )
            ),
            shinydashboard::tabItem(tabName = "clientOrders_wait",
                shiny::fluidRow(
                    shinydashboardPlus::box(title = "Commandes complétées", width = 6
                    ),
                    shinydashboardPlus::box(title = "Commandes en attente", width = 6
                    )
                )
            ),

            shinydashboard::tabItem(tabName ="purchaseOrders",
                shiny::actionButton("refreshBtnPO", "Update"),
                shiny::actionButton("saveBtnPO", "Save"),
                DT::dataTableOutput('PurchaseOrders_DT')

            ),

            shinydashboard::tabItem(tabName ="dailyProduction",

            ),

            shinydashboard::tabItem(tabName ="weeklyProduction",

            )
        )
    )
)

source("scripts/googlesheets_access.R", local = TRUE) # get link to gs
source("scripts/interactiveDT.R", local = TRUE)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ### Reactive DF for Customer Orders
    interactive_dt <- InteractiveDT(link_gs = link_gs, sheet = "commandes_clients", mapping_sheet = "mapping_commandes_clients", id = "CustomerOrders", dropdownCol = "Statut", input = input, output = output, session = session) # Import Data from Google Sheets
    shiny::observeEvent(input$refreshBtn, { # Update Data if Refresh Button
        interactive_dt <- InteractiveDT(link_gs = link_gs, sheet = "commandes_clients", mapping_sheet = "mapping_commandes_clients", id = "CustomerOrders", dropdownCol = "Statut", input = input, output = output, session = session) # Import Data from Google Sheets
        print(interactive_dt$reactiveResultDF())
    })
    shiny::observeEvent(input$saveBtn, {
        googlesheets4::sheet_write(data = interactive_dt$reactiveResultDF(), ss = link_gs, sheet = "commandes_clients")
    })

    interactive_dt_fournisseur <- InteractiveDT(link_gs = link_gs, sheet = "commandes_fournisseurs", mapping_sheet = "mapping_commandes_fournisseurs", id = "PurchaseOrders", dropdownCol = "Statut", input = input, output = output, session = session) # Import Data from Google Sheets
    shiny::observeEvent(input$refreshBtnPO, { # Update Data if Refresh Button
        interactive_dt_fournisseur <- InteractiveDT(link_gs = link_gs, sheet = "commandes_fournisseurs", mapping_sheet = "mapping_commandes_fournisseurs", id = "PurchaseOrders", dropdownCol = "Statut", input = input, output = output, session = session) # Import Data from Google Sheets
    })
    shiny::observeEvent(input$saveBtnPO, {
        googlesheets4::sheet_write(data = interactive_dt_fournisseur$reactiveResultDF(), ss = link_gs, sheet = "commandes_fournisseurs")
    })

}

shinyApp(ui, server)