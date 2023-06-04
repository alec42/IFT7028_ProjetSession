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
      shinydashboard::menuItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("th")),
      shinydashboard::menuItem("Expédition", tabName = "expedition", icon = icon("road"))
      
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
        splitLayout(cellWidths = c("0", "25%", "25%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          shiny::textInput("orderID", "Numéro de commande"),
          shiny::selectInput("statusChoice", "Choix Statut", c(
            "Modifiable", "En production", "Planifiée",
            "Commandée", "En attente de matériaux", "Complétée",
            "Emballée", "En livraison", "Livrée")
          )
        ),
        shiny::actionButton("updateStatus", "Mettre à jour les tables"),
        shiny::actionButton("refreshBtn", "Annuler"),
        shiny::actionButton("saveBtn", "Enregistrer"),
        br(),br(),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Toutes les commandes", width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary", DT::dataTableOutput('CustomerOrders_DT'))
        ),
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
        splitLayout(cellWidths = c("0", "25%", "25%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          shiny::textInput("orderID_PO", "Numéro de commande"),
          shiny::selectInput("statusChoice_PO", "Choix Statut", c("Commandée", "Reçue", "En attente d'approbation"))
        ),
        shiny::actionButton("updateStatus_PO", "Mettre à jour les tables"),
        shiny::actionButton("refreshBtn_PO", "Annuler"),
        shiny::actionButton("saveBtn_PO", "Enregistrer"),
        br(),br(),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Toutes les commandes", width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary", DT::dataTableOutput('PurchaseOrders_DT'))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "À commander", width = 12, solidHeader = TRUE, status = "success", DT::dataTableOutput('PO_To_Order_DT'))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "En attente de réception", width = 12, solidHeader = TRUE, status = "warning", DT::dataTableOutput('PO_Ordered_DT'))
        ),
      ),

      shinydashboard::tabItem(tabName ="dailyProduction",

      ),

      shinydashboard::tabItem(tabName ="weeklyProduction",

      ),
      
      shinydashboard::tabItem(tabName = "expedition",
                              shiny::textInput("orderID_exp", "Numéro de commande"),
                              shiny::selectInput("statusChoice_exp", "Choisir un statut", c("Complétée","Emballée","En livraison", "Livrée")),
                              shiny::actionButton("details_exp", "Détails de la commande"),
                              shiny::actionButton("updateStatus_exp", "Mettre à jour le statut"),
                              shiny::actionButton("refreshBtn_exp", "Annuler"),
                              shiny::actionButton("saveBtn_exp", "Enregistrer"),
                              DT::dataTableOutput('Expedition_DT'),
                              DT::dataTableOutput('ExpeditionDetails_DT')
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
clientsSheetName <- "Clients"
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
  values$clients <- read_sheet(link_gs_erp, sheet = clientsSheetName)

  ##########################
  ## Affichage par défaut ##
  ##########################
  # Commandes Clients - Default
  output$CustomerOrders_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = join_by(ClientID == ClientID)) |> 
      mutate(Client = paste(Prenom, ' ', Nom), 
             Date_Commande = as.Date(DateCommandeCreation), 
             Date_Livraison = as.Date(DateCommandeLivraison)) |>
      select(Client, CommandeID, Prix, Statut, Date_Commande, Date_Livraison),
    options = dt_options, rownames = FALSE, selection = "none")
  
  # Commandes Fournisseurs - Default
  output$PurchaseOrders_DT <- renderDT(
    values$purchaseOrders |> left_join(values$items, by = join_by(Items == ItemID)) |>
      rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Livraison = as.Date(DateCommandeFReception)) |>
      select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Livraison),
    options = dt_options, rownames = FALSE, selection = "none")
  
  # Inventaire - Default
  output$Inventory_DT <- renderDT(values$inventory, options = dt_options, rownames = FALSE, selection = "none")
  
  # Expédition - Default
  output$Expedition_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = join_by(ClientID == ClientID)) |>
      mutate(Client = paste(Prenom, ' ', Nom), 
             Date_Commande = as.Date(DateCommandeCreation), 
             Date_Livraison = as.Date(DateCommandeLivraison)) |>
      filter(Statut %in% c("Complétée", "Emballée", "En livraison")) |> 
      select(Client, Adresse, CommandeID, Prix, Items, Statut), 
    options = dt_options, rownames = FALSE, selection = "none")

  ######################
  ## Tables Commandes ##
  ######################
  #
  output$CustomerOrders_progress_DT <- renderDT(
    values$customerOrders |> filter(Statut == "En production") |> select(CommandeID, Items, Statut), 
    options = dt_options, rownames = FALSE, selection = "none")
  
  #
  output$CustomerOrders_waiting_materials_DT <- renderDT(
    values$customerOrders |> filter(Statut == "En attente de matériaux") |> select(CommandeID, Items, Statut), 
    options = dt_options, rownames = FALSE, selection = "none")
  
  #
  output$CustomerOrders_completed_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = join_by(ClientID == ClientID)) |>
      mutate(Client = paste(Prenom, ' ', Nom), 
             Date_Commande = as.Date(DateCommandeCreation), 
             Date_Livraison = as.Date(DateCommandeLivraison)) |>
      filter(Statut %in% c("Complétée", "Emballée", "En livraison")) |> 
      select(Client, Adresse, CommandeID, Prix, Items, Statut), 
    options = dt_options, rownames = FALSE, selection = "none")
  
  #
  output$CustomerOrders_pending_DT <- renderDT(
    values$customerOrders |> filter(Statut == "Modifiable") |> select(CommandeID, Statut), 
    options = dt_options, rownames = FALSE, selection = "none")

  ###############
  ## Tables PO ##
  ###############
  output$PO_To_Order_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "En attente d'approbation") |> rename(ID = CommandeFournisseurID) |>
      left_join(values$items, by = join_by(Items == ItemID)) |> select(ID, Fournisseur, Nom, Prix, Quantité, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PO_Ordered_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Commandée") |> left_join(values$items, by = join_by(Items == ItemID)) |>
      rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation)) |>
      select(ID, Nom, Quantité, Date_Commandee, Statut),
    options = dt_options, rownames = FALSE, selection = "none")

  ##############################
  ## Menu : commandes clients ##
  ##############################
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus, {
    print(values$items)
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
    values$purchaseOrders[as.character(values$purchaseOrders$CommandeFournisseurID) == input$orderID_PO, ]$Statut <- input$statusChoice_PO
    output$PurchaseOrders_DT <- renderDT(
      values$purchaseOrders |> left_join(values$items, by = join_by(Items == ItemID)) |>
        rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Livraison = as.Date(DateCommandeFReception)) |>
        select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Livraison),
      options = dt_options, rownames = FALSE, selection = "none")
  })
  # Button : Annuler
  observeEvent(input$refreshBtn_PO, {
    values$purchaseOrders<- read_sheet(link_gs_erp, sheet = purchaseOrdersSheetName)
    output$PurchaseOrders_DT <- renderDT(
      values$purchaseOrders |> left_join(values$items, by = join_by(Items == ItemID)) |>
        rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Livraison = as.Date(DateCommandeFReception)) |>
        select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Livraison),
      options = dt_options, rownames = FALSE, selection = "none")
  })
  # Button : Enregistrer
  observeEvent(input$saveBtn_PO, {
    googlesheets4::sheet_write(data = values$purchaseOrders, ss = link_gs, sheet = purchaseOrdersSheetName)
  })
  
  
  #######################
  ## Menu : expédition ##
  ####################### 
  
  # Button : Détails de la commande
  observeEvent(input$details_exp, {
    output$ExpeditionDetails_DT <- renderDT(
      values$customerOrders |> 
        mutate(Items = str_extract_all(Items, "\\d+:\\d+")) |>
        unnest(Items) |>
        separate(Items, into = c("ItemID", "Quantity"), sep = ":") |>
        mutate(across(c(ItemID, Quantity), as.integer)) |> 
        left_join(values$items, by = 'ItemID') |> 
        select(CommandeID, Type, Nom, Dimensions, Quantity) |>
        filter(as.character(CommandeID) == input$orderID_exp),
      options = dt_options, rownames = FALSE, selection = "none")
  })
}

shinyApp(ui, server)