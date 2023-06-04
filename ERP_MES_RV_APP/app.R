library(tidyverse)
# library(plotly)
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

##########################
# Interface Utilisateur #
########################

ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(),

  ##########
  ## Menu ##
  ##########
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id="sidebarMenu",
      shinydashboard::menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("th")),
      shinydashboard::menuItem("Commandes client", tabName = "clientOrders", icon = icon("th")),
      shinydashboard::menuItem("Commandes fournisseurs", tabName = "purchaseOrders", icon = icon("th")),
      shinydashboard::menuItem("Production journalière", tabName = "dailyProduction", icon = icon("th")),
      shinydashboard::menuItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("th"))
    )
  ),

  #############
  ## Contenu ##
  #############
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(".box {overflow: scroll;}"))),

    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName ="dashboard"
      ),

      shinydashboard::tabItem(tabName ="inventory",
        DT::dataTableOutput('Inventory_DT')
      ),

      shinydashboard::tabItem(tabName ="clientOrders",
        shiny::textInput("orderID", "Numéro de commande"),
        shiny::selectInput("statusChoice", "Choix Statut", c(
          "Modifiable", "En production", "Planifiée",
          "Commandée", "En attente de matériaux", "Complétée",
          "Emballée", "En livraison", "Livrée")
        ),
        shiny::actionButton("updateStatus", "Mettre à jour le statut"),
        shiny::actionButton("refreshBtn", "Annuler"),
        shiny::actionButton("saveBtn", "Enregistrer"),
        DT::dataTableOutput('CustomerOrders_DT'),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes en cours", width = 6, DT::dataTableOutput('CustomerOrders_progress_DT')),
          shinydashboardPlus::box(title = "Commandes en attente de matériaux", width = 6, DT::dataTableOutput('CustomerOrders_waiting_materials_DT'))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes complétées", width = 6, DT::dataTableOutput('CustomerOrders_completed_DT')),
          shinydashboardPlus::box(title = "Commandes en attente", width = 6, DT::dataTableOutput('CustomerOrders_pending_DT'))
        )
      ),

      shinydashboard::tabItem(tabName ="purchaseOrders",
        shiny::textInput("orderID_PO", "Numéro de commande"),
        shiny::selectInput("statusChoice_PO", "Choix Statut", c("Commandée", "Reçue", "En attente d'approbation")),
        shiny::actionButton("updateStatus_PO", "Mettre à jour le statut"),
        shiny::actionButton("refreshBtn_PO", "Annuler"),
        shiny::actionButton("saveBtn_PO", "Enregistrer"),
        DT::dataTableOutput('PurchaseOrders_DT'),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "À commander", width = 12, DT::dataTableOutput('PO_To_Order_DT'))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "En attente de réception", width = 12, DT::dataTableOutput('PO_Ordered_DT'))
        ),
      ),

      shinydashboard::tabItem(tabName ="dailyProduction",

      ),

      shinydashboard::tabItem(tabName ="weeklyProduction",

      )
    )
  )
)

source("scripts/googlesheets_access.R") # get link to gs
source("scripts/interactiveDT.R", local = TRUE)

###########################
## Onglets Google Sheets ##
###########################

customerOrdersSheetName <- "Commandes"
purchaseOrdersSheetName <- "CommandesFournisseurs"
itemsSheetName <- "Items"
InventorySheetName <- "inventaire"
dt_options <- list(dom = 't')

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #######################
  ## Données réactives ##
  #######################
  values <- reactiveValues()
  values$customerOrders <- read_sheet(link_gs_erp, sheet = customerOrdersSheetName)
  values$purchaseOrders <- read_sheet(link_gs_erp, sheet = purchaseOrdersSheetName)
  values$inventory <- read_sheet(link_gs, sheet = InventorySheetName)
  values$items <- read_sheet(link_gs_erp, sheet = itemsSheetName)

  ##########################
  ## Affichage par défaut ##
  ##########################
  output$CustomerOrders_DT <- renderDT(values$customerOrders, options = dt_options, rownames = FALSE, selection = "none")
  output$PurchaseOrders_DT <- renderDT(
    values$purchaseOrders |> left_join(values$items, by = join_by(Items == ItemID)) |>
      rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Livraison = as.Date(DateCommandeFReception)) |>
      select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Livraison),
    options = dt_options, rownames = FALSE, selection = "none")
  output$Inventory_DT <- renderDT(values$inventory, options = dt_options, rownames = FALSE, selection = "none")

  ######################
  ## Tables Commandes ##
  ######################
  output$CustomerOrders_progress_DT <- renderDT(values$customerOrders |> filter(Statut == "En production") |> select(CommandeID, Items, Statut), options = dt_options, rownames = FALSE, selection = "none")
  output$CustomerOrders_waiting_materials_DT <- renderDT(values$customerOrders |> filter(Statut == "En attente de matériaux") |> select(CommandeID, Items, Statut), options = dt_options, rownames = FALSE, selection = "none")
  output$CustomerOrders_completed_DT <- renderDT(values$customerOrders |> filter(Statut %in% c("Complétée", "Emballée", "En livraison")) |> select(CommandeID, Items, Statut), options = dt_options, rownames = FALSE, selection = "none")
  output$CustomerOrders_pending_DT <- renderDT(values$customerOrders |> filter(Statut == "Modifiable") |> select(CommandeID, Statut), options = dt_options, rownames = FALSE, selection = "none")

  ###############
  ## Tables PO ##
  ###############
  output$PO_To_Order_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "En attente d'approbation") |> rename(ID = CommandeFournisseurID) |>
      left_join(items, by = join_by(Items == ItemID)) |> select(ID, Fournisseur, Nom, Prix, Quantité, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PO_Ordered_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Commandée") |> left_join(values$items, by = join_by(Items == ItemID)) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation)) |> select(CommandeFournisseurID, Nom, Quantité, Date_Commandee, Statut),
    options = dt_options, rownames = FALSE, selection = "none")

  ##############################
  ## Menu : commandes clients ##
  ##############################
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus, {
    values$customerOrders[as.character(values$customerOrders$CommandeID) == input$orderID,]$Statut <- input$statusChoice
    output$CustomerOrders_DT <- outputDT(values$customerOrders)
  })
  # Button : Annuler
  observeEvent(input$refreshBtn, {
    values$customerOrders<- read_sheet(link_gs_erp, sheet = customerOrdersSheetName)
    output$CustomerOrders_DT <- outputDT(values$customerOrders)
  })
  # Button : Enregistrer
  observeEvent(input$saveBtn, {
    googlesheets4::sheet_write(data = values$customerOrders, ss = link_gs_erp, sheet = customerOrdersSheetName)
  })


  ###################################
  ## Menu : commandes fournisseurs ##
  ###################################
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus_PO, {
    values$purchaseOrders[as.character(values$purchaseOrders$CommandID) == input$orderID_PO, ]$Statut <- input$statusChoice_PO
    output$PurchaseOrders_DT <- outputDT(values$purchaseOrders)
  })
  # Button : Annuler
  observeEvent(input$refreshBtn_PO, {
    values$purchaseOrders<- read_sheet(link_gs, sheet = purchaseOrdersSheetName)
    output$PurchaseOrders_DT <- outputDT(values$purchaseOrders)
  })
  # Button : Enregistrer
  observeEvent(input$saveBtn_PO, {
    googlesheets4::sheet_write(data = values$purchaseOrders, ss = link_gs, sheet = purchaseOrdersSheetName)
  })
}

shinyApp(ui, server)