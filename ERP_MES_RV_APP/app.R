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
                    shinydashboardPlus::box(title = "Commandes en cours", width = 6#, DT::DTOutput("completed_table")
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

    # createFilteredTable <- function(data, filterValue) {
    #     DT::renderDT({
    #         DT::datatable(
    #             data[data$Statut == filterValue, ], escape = FALSE, selection = 'none', rownames = FALSE,
    #             options = list(paging = FALSE, ordering = FALSE, scrollX = TRUE, dom = "t",
    #                            preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
    #                            drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    #             )
    #         )
    #     })
    # }
    ### Reactive DF for Customer Orders
    # Import Data from Google Sheets
    interactive_dt <- InteractiveDT(link_gs, "commandes_clients", "Statut", input, output, session)
    # Update Data if Refresh Button
    shiny::observeEvent(input$refreshBtn, {
        interactive_dt <- InteractiveDT(link_gs, "commandes_clients", "Statut", input, output, session)
        # replaceData(proxy = interactive_dt$CustomerOrders_proxy, data = interactive_dt$reactiveHTMLDF(), rownames = FALSE)
    })
    # Update Data if select tab
    # shiny::observeEvent(input$sidebarMenu, {
    #     # interactive_dt <- InteractiveDT(link_gs, "commandes_clients", "Statut", input, output, session)
    #     replaceData(proxy = interactive_dt$CustomerOrders_proxy, data = interactive_dt$reactiveHTMLDF(), rownames = FALSE)
    #     selectedTab <- input$sidebarMenu
    #     if (selectedTab == "clientOrders_wait" || selectedTab == "clientOrders_completed") {
    #         output$completed_table <- createFilteredTable(interactive_dt$reactiveResultDF(), "Complétée")
    #     }
    # })

    shiny::observeEvent(input$saveBtn, {
        googlesheets4::sheet_write(data = interactive_dt$reactiveResultDF(), ss = link_gs, sheet = "commandes_clients")
    })

    # output$completed_table <- createFilteredTable(interactive_dt$reactiveResultDF(), "Complétée")

    # output$orders_db <- DT::renderDT({
    #     datatable(data = db_orders, rownames = FALSE, selection = 'none',
    #           editable = list(target="row", disable = list(columns =seq_along(db_orders)[names(db_orders)!= "Statut"]-1))
    #     )
    # })


}

shinyApp(ui, server)