library(tidyverse)
# library(plotly)
library(DT)
library(shiny)
library(scales)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
# library(shinyWidgets)

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
      shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("list-alt")),
      shinydashboard::menuItem("Tableau de bord", tabName = "clientOrders", icon = icon("dashboard")), # was commandes client
      shinydashboard::menuItem("Réception (fournisseurs)", tabName = "purchaseOrders", icon = icon("envelope")),
      shinydashboard::menuItem("Expédition (client)", tabName = "expedition", icon = icon("road")),
      shinydashboard::menuItem("Production journalière", tabName = "dailyProduction", icon = icon("calendar")),
      shinydashboard::menuItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("calendar"))
    )
  ),

  #############
  ## Contenu ##
  #############
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(".box {overflow: scroll;}"))),

    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName ="inventory",
        shinydashboardPlus::box(title="Ajouter un nouvel item", width = 12,
          splitLayout(cellWidths = c("0", "24%", "24%", "24%", "24%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                      shiny::numericInput("inventory_itemIDSelect", "ItemID", value = 8), # hardcode max + 1
                      shiny::textInput("inventory_FournisseurSelect", "Fournisseur"),
                      shiny::numericInput("inventory_PrixSelect", "Prix", value = 0),
                      shiny::textInput("inventory_NameSelect", "Nom d'item")
          ),
          splitLayout(cellWidths = c("0", "24%", "24%", "24%", "24%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                      shiny::textInput("inventory_DescriptionSelect", "Description de l'item"),
                      shiny::textInput("inventory_DescriptionSelect", "Type d'item"),
                      shiny::textInput("inventory_DimensionsSelect", "Dimensions (entrer en crochets si planche)", placeholder = "[H;W;L]"), # conditionnel??
                      shiny::numericInput("inventory_MinStockSelect", "Stock minimum à conserver", value = 1)
          ),
          shiny::actionButton("AddInventoryBtn", "Confirmer et ajouter")
        ),
        DT::dataTableOutput('Inventory_DT')
      ),

      shinydashboard::tabItem(tabName ="clientOrders",
        splitLayout(cellWidths = c("0", "25%", "25%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          shiny::textInput("orderID", "Numéro de commande"),
          shiny::selectInput("statusChoice", "Choix Statut", c(
            "Modifiable", "En production", "Planifiée",
            "Commandée", "En attente de matériaux", "Complétée",
            "Emballée", "En livraison", "Livrée"))),
        shiny::actionButton("updateStatus", "Mettre à jour les tables"),
        shiny::actionButton("refreshBtn", "Annuler"),
        shiny::actionButton("saveBtn", "Enregistrer"),
        br(),br(),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Toutes les commandes", width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary",
            DT::dataTableOutput('CustomerOrders_DT'))),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes en production", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_progress_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("ProgessOrdersTotal"))),
          shinydashboardPlus::box(title = "Commandes en conception", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_pending_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("PendingOrdersTotal"))),
          shinydashboardPlus::box(title = "Commandes complétées", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_completed_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("CompletedOrdersTotal")))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Matériaux en attentes pour commandes", solidHeader = TRUE, status = "success", width = 12,
            DT::dataTableOutput('CustomerOrders_waiting_materials_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "commandes en attente de matériaux", header = textOutput("WaitingOrdersTotal"))))
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
        splitLayout(cellWidths = c("0", "25%", "25%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          shiny::textInput("orderID_exp", "Numéro de commande"),
          shiny::selectInput("statusChoice_exp", "Choisir un statut", c("Complétée","Emballée","En livraison", "Livrée"))
        ),
        shiny::actionButton("updateStatus_exp", "Mettre à jour le statut"),
        shiny::actionButton("refreshBtn_exp", "Annuler"),
        shiny::actionButton("saveBtn_exp", "Enregistrer"),
        br(),br(),
        shinydashboardPlus::box(title = "Liste d'items de la commande", solidHeader = TRUE, width = 12,
          DT::dataTableOutput('ExpeditionDetails_DT')
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes prêtes à emballer", solidHeader = TRUE, status = "danger", width = 6,
            DT::dataTableOutput('CustomerOrders_ready_wrap_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "commandes prêtes à emballer", header = textOutput("ReadyWrapOrdersTotal"))
          ),
          shinydashboardPlus::box(title = "Commandes prêtes à expédier", solidHeader = TRUE, status = "warning", width = 6,
            DT::dataTableOutput('CustomerOrders_ready_ship_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "commandes prêtes à expédier", header = textOutput("ReadyShipOrdersTotal"))
          ),
          shinydashboardPlus::box(title = "Commandes expédiées", solidHeader = TRUE, status = "success", width = 12,
            DT::dataTableOutput('CustomerOrders_shipped_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "commandes expédiées", header = textOutput("ShippedOrdersTotal"))
          )
        )
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
InventorySheetName <- "Inventaire"
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
  values$inventory <- read_sheet(link_gs_erp, sheet = InventorySheetName)
  values$items <- read_sheet(link_gs_erp, sheet = itemsSheetName)
  values$clients <- read_sheet(link_gs_erp, sheet = clientsSheetName)

  ##########################
  ## Affichage par défaut ##
  ##########################
  # Commandes Clients - Default
  output$CustomerOrders_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = "ClientID") |>
      mutate(Client = paste(Prenom, ' ', Nom), Prix = scales::dollar(Prix), Date_Commandee = as.Date(DateCommandeCreation), Date_Livraison = as.Date(DateCommandeLivraison)) |>
      rename(ID = CommandeID) |> select(ID, Client, Prix, Statut, Date_Commandee, Date_Livraison),
    options = dt_options, rownames = FALSE, selection = "none")

  # Commandes Fournisseurs - Default
  output$PurchaseOrders_DT <- renderDT(
    values$purchaseOrders |> left_join(values$items, by = "ItemID") |> mutate(Prix = scales::dollar(Prix), Date_Commandee = as.Date(DateCommandeFCreation), Date_Reception = as.Date(DateCommandeFReception)) |>
      rename(ID = CommandeFournisseurID) |> select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee, Date_Reception),
    options = dt_options, rownames = FALSE, selection = "none")


  #######################
  ## Menu : inventaire ##
  #######################
  output$Inventory_DT <- renderDT(
    values$inventory |>
    left_join(values$customerOrders |>
                filter(Statut %in% c("Commandée", "En attente de matériaux")) %>%
                mutate(Items = str_extract_all(Items, "\\d+:\\d+")) %>%
                unnest(Items) %>%
                separate(Items, into = c("ItemID", "Quantity"), sep = ":") %>%
                mutate(across(c(ItemID, Quantity), as.integer)) |>
                select(ItemID, Quantity) |> group_by(ItemID) |> summarise(Quantité_Requise = sum(Quantity)),
              by = "ItemID") |>
    left_join(values$purchaseOrders |>
                filter(Statut %in% c("En attente d'approbation", "Commandée")) |>
                select(ItemID, Quantité) |> group_by(ItemID) |> summarise(Quantité_Commandée = sum(Quantité)),
              by = "ItemID") |>
      mutate(Date_Mise_A_Jour = as.Date(DateMiseAJour)) |>
      select(ItemID, Date_Mise_A_Jour, QuantiteDisponible, Quantité_Requise, Quantité_Commandée),
    options = dt_options, rownames = FALSE, selection = "none")


  #######################
  ## Tables Expedition ##
  #######################
  output$CustomerOrders_shipped_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut %in% c("Livrée", "En livraison")) |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ShippedOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut %in% c("Livrée", "En livraison")))
  })
  #
  output$CustomerOrders_ready_ship_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut == "Emballée") |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ReadyShipOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Emballée"))
  })
  #
  output$CustomerOrders_ready_wrap_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut == "Complétée") |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ReadyWrapOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Complétée"))
  })
  ######################
  ## Tables Commandes ##
  ######################
  #
  output$CustomerOrders_progress_DT <- renderDT(
    values$customerOrders |> filter(Statut == "En production") |> rename(ID = CommandeID) |> select(ID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ProgessOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "En production"))
  })
  output$CustomerOrders_waiting_materials_DT <- renderDT(
    values$customerOrders |> filter(Statut == "En attente de matériaux") |>
      mutate(Date_Commandee = as.Date(DateCommandeCreation), Items = str_extract_all(Items, "\\d+:\\d+")) |> unnest(Items) |> separate(Items, into = c("ItemID", "Quantity"), sep = ":") |> mutate(across(c(ItemID, Quantity), as.integer)) |>
      left_join(values$items, by = 'ItemID') |> select(CommandeID, Date_Commandee, Nom, Type, Quantity),
    options = dt_options, rownames = FALSE, selection = "none")
  output$WaitingOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "En attente de matériaux"))
  })
  #
  output$CustomerOrders_completed_DT <- renderDT(
    values$customerOrders |>
      filter(Statut %in% c("Complétée", "Emballée", "En livraison")) |> rename(ID = CommandeID) |> select(ID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$CompletedOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut %in% c("Complétée", "Emballée", "En livraison")))
  })
  #
  output$CustomerOrders_pending_DT <- renderDT(
    values$customerOrders |> filter(Statut == "Modifiable") |> rename(ID = CommandeID) |> select(ID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PendingOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Modifiable"))
  })

  ###############
  ## Tables PO ##
  ###############
  output$PO_To_Order_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "En attente d'approbation") |> rename(ID = CommandeFournisseurID) |>
      left_join(values$items, by = "ItemID") |> mutate(Prix = scales::dollar(Prix)) |>
      select(ID, Fournisseur, Nom, Prix, Quantité, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PO_Ordered_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Commandée") |> left_join(values$items, by = "ItemID") |>
      rename(ID = CommandeFournisseurID) |> mutate(Date_Commandee = as.Date(DateCommandeFCreation)) |>
      select(ID, Nom, Quantité, Date_Commandee, Statut),
    options = dt_options, rownames = FALSE, selection = "none")

  ##############################
  ## Menu : commandes clients ##
  ##############################
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus, {
    values$customerOrders[as.character(values$customerOrders$CommandeID) == input$orderID,]$Statut <- input$statusChoice
    output$CustomerOrders_DT <- renderDT(
      values$customerOrders |> left_join(values$clients, by = "ClientID") |>
        mutate(Client = paste(Prenom, ' ', Nom), Date_Commandee = as.Date(DateCommandeCreation), Date_Livraison = as.Date(DateCommandeLivraison), Prix = scales::dollar(Prix)) |> rename(ID = CommandeID) |>
        select(ID, Client, Prix, Statut, Date_Commandee, Date_Livraison),
      options = dt_options, rownames = FALSE, selection = "none")
  })
  # Button : Annuler
  observeEvent(input$refreshBtn, {
    values$customerOrders <- read_sheet(link_gs_erp, sheet = customerOrdersSheetName)
    output$CustomerOrders_DT <- renderDT(
      values$customerOrders |> left_join(values$clients, by = "ClientID") |>
        mutate(Client = paste(Prenom, ' ', Nom), Date_Commandee = as.Date(DateCommandeCreation), Date_Livraison = as.Date(DateCommandeLivraison), Prix = scales::dollar(Prix)) |> rename(ID = CommandeID) |>
        select(ID, Client, Prix, Statut, Date_Commandee, Date_Livraison),
      options = dt_options, rownames = FALSE, selection = "none")
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
      values$purchaseOrders |> left_join(values$items, by = "ItemID") |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Reception = as.Date(DateCommandeFReception), Prix = scales::dollar(Prix)) |>
        rename(ID = CommandeFournisseurID) |> select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Reception),
      options = dt_options, rownames = FALSE, selection = "none")
  })
  # Button : Annuler
  observeEvent(input$refreshBtn_PO, {
    values$purchaseOrders<- read_sheet(link_gs_erp, sheet = purchaseOrdersSheetName)
    output$PurchaseOrders_DT <- renderDT(
      values$purchaseOrders |> left_join(values$items, by = "ItemID") |> mutate(Date_Commandee = as.Date(DateCommandeFCreation), Date_Reception = as.Date(DateCommandeFReception), Prix = scales::dollar(Prix)) |>
        rename(ID = CommandeFournisseurID) |> select(ID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee,	Date_Reception),
      options = dt_options, rownames = FALSE, selection = "none")
  })
  # Button : Enregistrer
  observeEvent(input$saveBtn_PO, {
    googlesheets4::sheet_write(data = values$purchaseOrders, ss = link_gs_erp, sheet = purchaseOrdersSheetName)
  })


  #######################
  ## Menu : expédition ##
  #######################
  # Button : Détails de la commande
  observeEvent(input$orderID_exp, {
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
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus_exp, {
    values$customerOrders[as.character(values$customerOrders$CommandeID) == input$orderID_exp,]$Statut <- input$statusChoice_exp
  })
  # Button : Annuler
  observeEvent(input$refreshBtn_exp, {
    values$customerOrders <- read_sheet(link_gs_erp, sheet = customerOrdersSheetName)
  })
  # Button : Enregistrer
  observeEvent(input$saveBtn_exp, {
    googlesheets4::sheet_write(data = values$customerOrders, ss = link_gs_erp, sheet = customerOrdersSheetName)
  })
}

shinyApp(ui, server)