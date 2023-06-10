library(tidyverse)
library(DT)
library(shiny)
library(scales)
library(rjson)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)


# day_planif <- Sys.Date()
day_planif <- "2023-05-31"

source("scripts/googlesheets_access.R") # get link to gs
source("scripts/planning_algo.R", local = TRUE)
source("scripts/google_drive_json_update.R")

# GDriveJSONUpdate(
#   dossier_racine = "Industrie_VR_IFT7028/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
#   customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes'))

###################
## Google Sheets ##
###################

customerOrdersSheetName <- "Commandes"
purchaseOrdersSheetName <- "CommandesFournisseurs"
itemsSheetName <- "Items"
InventorySheetName <- "Inventaire"
clientsSheetName <- "Clients"
panneauxSheetName <- "PanneauxDetail"
piecesSheetName <- "PiecesDetail"
employeesDispoSheetName <- "DisposEmployés"
factoryHoursSheetName <- "DisposUsine"
dt_options <- list(dom = 't')


###########################
## Interface Utilisateur ##
###########################

ui <- shinydashboardPlus::dashboardPage(title="S.T.E.V.E.", skin = "blue-light",
  header = shinydashboardPlus::dashboardHeader(#controlbarIcon = icon('gears'),
    # title = "Stock Tracking for Experimental Vehicle Enterprise (S.T.E.V.E.)",
    title = span("S.T.E.V.E.", style = "display: flex; justify-content: center; align-items: center;"),
    tags$li(class = "dropdown",
      actionButton(inputId = "HeaderButton", label = "Update", icon = icon("arrows-rotate"))
    ),
    tags$li(class = "dropdown",
      shiny::actionButton(inputId = "HeaderButtonHelp", label = "Help", icon = icon("circle-info"))
    )
  ),

  ##########
  ## Menu ##
  ##########
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id="sidebarMenu",
      shinydashboard::menuItem("Tableau de bord", tabName = "clientOrders", icon = icon("dashboard")),
      shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("list-alt")),
      shinydashboard::menuItem("Réception (fournisseurs)", tabName = "purchaseOrders", icon = icon("envelope")),
      shinydashboard::menuItem("Expédition (client)", tabName = "expedition", icon = icon("truck-fast")),
      shinydashboard::menuItem("Production", icon = icon("calendar-alt"),startExpanded = T,
        shinydashboard::menuSubItem("Horaires", tabName = "schedules", icon = icon("clock")),
        shinydashboard::menuSubItem("Production journalière", tabName = "dailyProduction", icon = icon("calendar-day"), selected = T),
        shinydashboard::menuSubItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("calendar-week"))
      )
    )
  ),

  ##########
  ## Body ##
  ##########
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    tags$head(tags$style(HTML(".box {overflow: auto;}"))),

    shinydashboard::tabItems(

      #### Inventaire ####
      shinydashboard::tabItem(tabName ="inventory",
        shiny::fluidRow(
          shinydashboardPlus::box(title="Ajouter/Retirer un nouvel item", width = 12,
            splitLayout(cellWidths = c("0", "24%", "24%", "24%", "24%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                        shiny::textInput("inventory_FournisseurSelect", "Fournisseur"),
                        shiny::numericInput("inventory_PrixSelect", "Prix", value = 0),
                        shiny::textInput("inventory_NameSelect", "Nom d'item")
            ),
            splitLayout(cellWidths = c("0", "24%", "24%", "24%", "24%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                        shiny::textInput("inventory_DescriptionSelect", "Description de l'item"),
                        shiny::textInput("inventory_TypeSelect", "Type d'item"),
                        shiny::textInput("inventory_DimensionsSelect", "Dimensions (entrer en crochets si planche)", placeholder = "[H;W;L]"), # conditionnel??
                        shiny::numericInput("inventory_MinStockSelect", "Stock minimum à conserver", value = 1)
            ),
            shiny::actionButton("AddInventoryBtn", "Ajouter"),

            shiny::textInput("itemID", "ItemID"),
            shiny::actionButton("removeInventoryBtn", "Retirer"),
            shiny::actionButton("refreshItemBtn", "Annuler"),
            shiny::actionButton("saveInventoryBtn", "Enregistrer")
          ),
          shinydashboardPlus::box(title = "Liste d'items disponibles", width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary", DT::dataTableOutput('Items_DT')),
          shinydashboardPlus::box(title = "Inventaire actuel", width = 12, collapsible = F, solidHeader = TRUE, status = "success", DT::dataTableOutput('Inventory_DT'))
        ),
      ),

      #### Dashboard ####
      shinydashboard::tabItem(tabName ="clientOrders",
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Toutes les commandes", width = 12, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary",
            DT::dataTableOutput('CustomerOrders_DT'))),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Revenus", width = 6, solidHeader = TRUE, background = "olive",
                                  shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("RevenueTotal"))),
          shinydashboardPlus::box(title = "Dépenses", width = 6, solidHeader = TRUE, background = "maroon",
                                  shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("CostsTotal")))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes en conception", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_pending_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("PendingOrdersTotal"))),
          shinydashboardPlus::box(title = "Commandes en production", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_progress_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("ProgessOrdersTotal"))),
          shinydashboardPlus::box(title = "Commandes complétées", width = 4, solidHeader = TRUE, status = "warning",
            DT::dataTableOutput('CustomerOrders_completed_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("CompletedOrdersTotal")))
        ),
        shiny::fluidRow(
          shinydashboardPlus::box(title = "Commandes en attente d'approbation", width = 4, solidHeader = TRUE, status = "success",
            DT::dataTableOutput('POs_pending_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("PendingPOsTotal"))),
          shinydashboardPlus::box(title = "Commandes approuvées", width = 4, solidHeader = TRUE, status = "success",
            DT::dataTableOutput('POs_sent_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("SentPOsTotal"))),
          shinydashboardPlus::box(title = "Commandes Reçues", width = 4, solidHeader = TRUE, status = "success",
            DT::dataTableOutput('POs_received_DT'),
            footer = shinydashboardPlus::descriptionBlock(text = "Total", header = textOutput("ReceivedPOsTotal")))
        )
      ),

      #### Réceptions ####
      shinydashboard::tabItem(tabName ="purchaseOrders",
        splitLayout(cellWidths = c("0", "25%", "25%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          shiny::textInput("orderID_PO", "Numéro de commande"),
          shiny::selectInput("statusChoice_PO", "Choix Statut", c("Commandée", "Reçue"))
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
        )
      ),

      #### Horaires ####
      shinydashboard::tabItem(tabName ="schedules",
        shinydashboardPlus::box(
          title = "Horaire de l'usine pour la semaine", solidHeader = TRUE, width = 12, status = "warning", collapsible = T,
          splitLayout(cellWidths = c("0%", "13%", "10%", "10%", "2%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            shiny::selectInput("factoryDay", "Jour", choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), selectize = F, size = 5),
            shiny::selectInput("factoryOpen", "Ouverture", choices = sprintf("%02d:00:00", seq(8, 18)), selectize = F, size = 5),
            shiny::selectInput("factoryClose", "Fermeture", choices = sprintf("%02d:00:00", seq(8, 18)), selected = "18:00:00",selectize = F, size = 5),
            br(),
            tableOutput('factoryHoursTable')
          ),
          shiny::actionButton("updateFactoryHours", "Mettre à jour heures"),
          shiny::actionButton("refreshFactoryHours", "Annuler"),
          shiny::actionButton("saveFactoryHours", "Sauvegarder")
        ),
        shinydashboardPlus::box(
          title = "Horaire de l'employé pour la semaine", solidHeader = TRUE, width = 12, status = "warning", collapsible = T,
          splitLayout(cellWidths = c("0", "15%", "70%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            shiny::numericInput(inputId = "EmployeeDispoID", label = "Employé", value=0),
            shinyWidgets::checkboxGroupButtons(
              inputId = "employeeSchedule", label = "Disponibilités",
              choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
            )
          ),
          shiny::actionButton("refreshEmployeeDispo", "Annuler"),
          shiny::actionButton("saveEmployeeDispo", "Sauvegarder")
        )
      ),

      #### Planification journalière ####
      shinydashboard::tabItem(tabName ="dailyProduction",
        fluidPage(
          shinydashboardPlus::box(title="Panneau", width = 12,
            splitLayout(cellWidths = c("0", "24%", "24%"), tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
              shiny::textInput("panneauID", "ID du panneau"),
              shiny::selectInput("panneauStatus", "Choix statut", c("TODO", "DONE"))
            ),
            shiny::actionButton("updatePanneauBtn", "Mettre à jour le statut"),
            shiny::actionButton("cancelPanneauBtn", "Annuler"),
            shiny::actionButton("savePanneauBtn", "Enregistrer"),
            br(),
            DT::dataTableOutput('Panneaux_DT')
          ),
          shinydashboardPlus::box(title = "Planification pour la journée", solidHeader = TRUE, width = 12,
            fluidRow(
              column(width=2, shiny::dateInput("dayPlanif", label = "Date", value = Sys.Date())),
              column(width=3, shiny::textInput("maxDaysPlanif", label = "Nombre maximal jours à planifier", value = 5)),
              column(width=3, shiny::textInput("bufferDaysPlanif", label = "Période tampon pour planification", value = 3)),
              column(width=2, shiny::textInput("nbMachinesPlanif",label = "Machines Disponibles", value = 1)),
              column(width=1, actionButton(inputId = "HeaderButton2", label = "Update", icon = icon("arrows-rotate")))
            ),
            timevisOutput("timelineDaily"),
            tableOutput('tableDaily_1'),
            tableOutput('tableDaily_2')
          )
        )
      ),

      #### Planification hebdomadaire ####
      shinydashboard::tabItem(tabName ="weeklyProduction",
        shinydashboardPlus::box(title = "Planification pour la semaine", solidHeader = TRUE, width = 12,
          timevisOutput("timelineWeekly"),
          tableOutput('tableWeekly_1')
        )
      ),

      #### Expéditions ####
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
############
## END UI ##
############


#############
## Server ##
#############

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
  values$panneaux <- read_sheet(link_gs_erp, sheet = panneauxSheetName)
  values$pieces <- read_sheet(link_gs_erp, sheet = piecesSheetName)
  values$employees <- read_sheet(link_gs_erp, sheet = employeesDispoSheetName)
  values$factory <- read_sheet(link_gs_erp, sheet = factoryHoursSheetName) %>% mutate(across(c(Monday, Tuesday, Wednesday, Thursday, Friday), ~format(., format="%H:%M:%S")))

  ############################
  ## Menu : Tableau de bord ##
  ############################

  # Affichage : Toutes les commandes
  output$CustomerOrders_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = "ClientID") |>
      mutate(Client = paste(Prenom, ' ', Nom), Prix = scales::dollar(Prix), Date_Commandee = as.Date(DateCommandeCreation), Date_Livraison = as.Date(DateCommandeLivraison)) |>
      select(CommandeID, Client, Prix, Statut, Date_Commandee, Date_Livraison),
    options = dt_options, rownames = FALSE, selection = "none")

  # Affichage : Revenus
  output$RevenueTotal <- renderText({
    scales::dollar(sum((values$purchaseOrders |> left_join(values$items, 'ItemID') |> mutate(Cout = Prix * Quantité))$Cout))
  })

  # Affichage : Dépenses
  output$CostsTotal <- renderText({
    scales::dollar(sum(values$customerOrders$Prix))
  })

  # Affichage : Commandes en production
  output$CustomerOrders_progress_DT <- renderDT(
    values$customerOrders |> filter(Statut == "En production") |> select(CommandeID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ProgessOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "En production"))
  })

  # Affichage : Commandes compétées
  output$CustomerOrders_completed_DT <- renderDT(
    values$customerOrders |>
      filter(Statut %in% c("Complétée", "Emballée", "En livraison")) |> select(CommandeID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$CompletedOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut %in% c("Complétée", "Emballée", "En livraison")))
  })

  # Affichage : Commandes en conception
  output$CustomerOrders_pending_DT <- renderDT(
    values$customerOrders |> filter(Statut == "Modifiable") |> select(CommandeID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PendingOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Modifiable"))
  })

  # Affichage : Commandes en attente d'approbation
  output$POs_pending_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "En attente d'approbation") |> select(CommandeFournisseurID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$PendingPOsTotal <- renderText({
    nrow(values$purchaseOrders |> filter(Statut == "En attente d'approbation"))
  })

  # Affichage : Commandes reçues
  output$POs_received_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Reçue") |> select(CommandeFournisseurID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ReceivedPOsTotal <- renderText({
    nrow(values$purchaseOrders |> filter(Statut == "Reçue"))
  })

  # Affichage : Commande approuvées
  output$POs_sent_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Commandée") |> select(CommandeFournisseurID, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$SentPOsTotal <- renderText({
    nrow(values$purchaseOrders |> filter(Statut == "Commandée"))
  })


  #######################
  ## Menu : Inventaire ##
  #######################

  # Affichage : Item
  output$Items_DT <- renderDT(
    values$items,
    rownames = FALSE, selection = "none")

  # Affichage : Inventaire
  output$Inventory_DT <- renderDT(
    values$inventory |>
      left_join(values$customerOrders |>
                  filter(Statut %in% c("Commandée", "En attente de matériaux")) |>
                  mutate(Items = str_extract_all(Items, "\\d+:\\d+")) |>
                  unnest(Items) |>
                  separate(Items, into = c("ItemID", "Quantity"), sep = ":") |>
                  mutate(across(c(ItemID, Quantity), as.integer)) |>
                  select(ItemID, Quantity) |> group_by(ItemID) |> summarise(Quantité_Requise = sum(Quantity)),
                by = "ItemID") |>
      left_join(values$purchaseOrders |>
                  filter(Statut %in% c("En attente d'approbation", "Commandée")) |>
                  select(ItemID, Quantité) |> group_by(ItemID) |> summarise(Quantité_Commandée = sum(Quantité)),
                by = "ItemID") |>
      left_join(values$items, by = join_by(ItemID == ItemID)) |>
      mutate(Date_Mise_A_Jour = as.Date(DateMiseAJour)) |>
      select(ItemID, Fournisseur, Prix, Nom, Description, Type, Dimensions, MinStock, QuantiteDisponible, Quantité_Requise, Quantité_Commandée, Date_Mise_A_Jour),
    options = dt_options, rownames = FALSE, selection = "none")

  # Button : Ajouter un item
  observeEvent(input$AddInventoryBtn, {
    values$items <- values$items |> add_row(ItemID = nrow(values$items) + 1,
                                            Fournisseur = input$inventory_FournisseurSelect,
                                            Prix = input$inventory_PrixSelect,
                                            Nom = input$inventory_NameSelect,
                                            Description = input$inventory_DescriptionSelect,
                                            Type = input$inventory_TypeSelect,
                                            Dimensions = input$inventory_DimensionsSelect,
                                            MinStock = input$inventory_MinStockSelect)
    values$inventory <- values$inventory |> add_row(
      ItemID=nrow(values$items) + 1, QuantiteDisponible=0, DateMiseAJour=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })

  # Button : Annuler
  observeEvent(input$refreshItemBtn, {
    values$items<- read_sheet(link_gs_erp, sheet = itemsSheetName)
  })


  # Button : Enregistrer
  observeEvent(input$saveInventoryBtn, {
    googlesheets4::sheet_write(data = values$items, ss = link_gs_erp, sheet = itemsSheetName)
  })

  # Button : Retirer
  observeEvent(input$removeInventoryBtn, {
    values$items <- values$items |> filter(ItemID != input$itemID)
  })


  ######################
  ## Menu : Réception ##
  ######################

  # Affichage : Toutes les commandes
  output$PurchaseOrders_DT <- renderDT(
    values$purchaseOrders |> left_join(values$items, by = "ItemID") |> mutate(Prix = scales::dollar(Prix), Date_Commandee = as.Date(DateCommandeFCreation), Date_Reception = as.Date(DateCommandeFReception)) |>
      select(CommandeFournisseurID, Fournisseur, Nom, Prix, Quantité, Statut, Date_Commandee, Date_Reception),
    options = dt_options, rownames = FALSE, selection = "none")

  # Affichage : Commandes en attente d'approbation
  output$PO_To_Order_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "En attente d'approbation") |>
      left_join(values$items, by = "ItemID") |> mutate(Prix = scales::dollar(Prix)) |>
      select(CommandeFournisseurID, Fournisseur, Nom, Prix, Quantité, Statut),
    options = dt_options, rownames = FALSE, selection = "none")

  # Affichage : Commandes approuvées
  output$PO_Ordered_DT <- renderDT(
    values$purchaseOrders |> filter(Statut == "Commandée") |> left_join(values$items, by = "ItemID") |>
      mutate(Date_Commandee = as.Date(DateCommandeFCreation)) |>
      select(CommandeFournisseurID, Nom, Quantité, Date_Commandee, Statut),
    options = dt_options, rownames = FALSE, selection = "none")

  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus_PO, {
    if ((input$statusChoice_PO == "Reçue") &&
        (values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$Statut != "Reçue")) {
      values$inventory[values$inventory$ItemID == values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$ItemID, ]$QuantiteDisponible <-
        values$inventory[values$inventory$ItemID == values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$ItemID, ]$QuantiteDisponible +
        values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$Quantité
    }

    if ((input$statusChoice_PO == "Commandée") &&
        (values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$Statut == "Reçue")) {
      values$inventory[values$inventory$ItemID == values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$ItemID, ]$QuantiteDisponible <-
        values$inventory[values$inventory$ItemID == values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$ItemID, ]$QuantiteDisponible -
        values$purchaseOrders[values$purchaseOrders$CommandeFournisseurID == input$orderID_PO, ]$Quantité
    }

    values$purchaseOrders[as.character(values$purchaseOrders$CommandeFournisseurID) == input$orderID_PO, ]$Statut <- input$statusChoice_PO
  })

  # Button : Annuler
  observeEvent(input$refreshBtn_PO, {
    values$purchaseOrders<- read_sheet(link_gs_erp, sheet = purchaseOrdersSheetName)
    values$inventory <- read_sheet(link_gs_erp, sheet = InventorySheetName)
  })

  # Button : Enregistrer
  observeEvent(input$saveBtn_PO, {
    googlesheets4::sheet_write(data = values$purchaseOrders, ss = link_gs_erp, sheet = purchaseOrdersSheetName)
    googlesheets4::sheet_write(data = values$inventory, ss = link_gs_erp, sheet = InventorySheetName)
  })


  #######################
  ## Menu : Expédition ##
  #######################

  # Affichage : Commandes expédiées
  output$CustomerOrders_shipped_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut %in% c("Livrée", "En livraison")) |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ShippedOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut %in% c("Livrée", "En livraison")))
  })

  # Affichage : Commandes prêtes à expédier
  output$CustomerOrders_ready_ship_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut == "Emballée") |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ReadyShipOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Emballée"))
  })

  # Affichage : Commandes prêtes à emballer
  output$CustomerOrders_ready_wrap_DT <- renderDT(
    values$customerOrders |> left_join(values$clients, by = 'ClientID') |>
      mutate(Client = paste(Prenom, ' ', Nom)) |> filter(Statut == "Complétée") |>
      select(CommandeID, Client, Adresse, Statut),
    options = dt_options, rownames = FALSE, selection = "none")
  output$ReadyWrapOrdersTotal <- renderText({
    nrow(values$customerOrders |> filter(Statut == "Complétée"))
  })

  # Affichage : Liste d'items de la commande
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

  ###################################
  ## Menu : Production journalière ##
  ###################################

  # Affichage : État du panneau
  output$Panneaux_DT <- renderDT(
    values$panneaux |> filter(PanneauID == input$panneauID),
    options = dt_options, rownames = FALSE, selection = "none")

  # Button : Mettre à jour le statut
  observeEvent(input$updatePanneauBtn, {
    values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$Statut <- input$panneauStatus
    if (input$panneauStatus == "DONE"){
      values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$DateFabrication <- input$dayPlanif
    }

    if (prod(sapply(
          values$panneaux |> filter(CommandeID == values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$CommandeID) |> select("Statut"),
          function(x) x == "DONE")
        ) == 1) {
      values$customerOrders[values$customerOrders$CommandeID == values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$CommandeID, ]$Statut <- "Complétée"
    }
  })

  # Button : Annuler
  observeEvent(input$cancelPanneauBtn, {
    values$panneaux <- read_sheet(link_gs_erp, sheet = panneauxSheetName)
    values$customerOrders <- read_sheet(link_gs_erp, sheet = customerOrdersSheetName)
  })

  # Button : Enregistrer
  observeEvent(input$savePanneauBtn, {
   googlesheets4::sheet_write(data = values$panneaux, ss = link_gs_erp, sheet = panneauxSheetName)
    googlesheets4::sheet_write(data = values$customerOrders, ss = link_gs_erp, sheet = customerOrdersSheetName)
  })

  ####################################
  ## Menu : Production hebdomadaire ##
  ####################################

  # Button : Mettre à jour les heures d'usine
  output$factoryHoursTable <- renderTable(values$factory)
  observeEvent(input$updateFactoryHours, {
    values$factory[input$factoryDay] <- ifelse(values$factory["MomentID"]=="Ouverture", input$factoryOpen, input$factoryClose)
  })
  # Button : Annuler heures d'usine
  observeEvent(input$refreshFactoryHours, {
    values$factory <- read_sheet(link_gs_erp, sheet = factoryHoursSheetName) |>
      mutate(across(c(Monday, Tuesday, Wednesday, Thursday, Friday), ~format(., format="%H:%M:%S")))
  })
  # Button : Enregistrer heures d'usine
  observeEvent(input$saveFactoryHours, {
    googlesheets4::sheet_write(data = values$factory, ss = link_gs_erp, sheet = factoryHoursSheetName)
  })
  # Button : Mettre à jour les dispos d'employés
  observeEvent(c(input$refreshEmployeeDispo, input$saveEmployeeDispo, input$EmployeeDispoID), {
    shinyWidgets::updateCheckboxGroupButtons(
      session, inputId = "employeeSchedule",
      selected = colnames(values$employees)[as.logical(filter(values$employees, EmployerID == input$EmployeeDispoID))][-1])
  })
  # Button : Annuler changements horaire d'employé
  observeEvent(input$refreshEmployeeDispo, {
    values$employees <- read_sheet(link_gs_erp, sheet = employeesDispoSheetName)
  })
  # Button : Enregistrer changements horaire d'employés
  observeEvent(input$saveEmployeeDispo, {
    values$employees <- values$employees |>
      rows_upsert(by = "EmployerID", tibble(!!!set_names(c(input$EmployeeDispoID, as.numeric(colnames(values$employees)[-1] %in% input$employeeSchedule)), colnames(values$employees))))
    googlesheets4::sheet_write(data = values$employees, ss = link_gs_erp, sheet = employeesDispoSheetName)
  })

  #### Header Buttons ####
  observeEvent(c(input$HeaderButton, input$HeaderButton2), {

    shinyWidgets::sendSweetAlert(session, closeOnClickOutside = T, html = T, title = "Mise à jour effectuée", width = "45%", text = "Les données ont été mises à jour.", type = "success")
    ##################
    #### Planning ####
    ##################
    # add a button for max_range (hirozn planif), buffer (horizon_gelé), and nb_machines
    MES_output <- MES_planif(
      values$customerOrders, values$inventory, values$purchaseOrders, values$panneaux, values$items, values$factory, values$employees,
      input$dayPlanif, max_range = as.numeric(input$maxDaysPlanif), buffer = as.numeric(input$bufferDaysPlanif), nb_machines = as.numeric(input$nbMachinesPlanif))
      # input$dayPlanif, max_range = 5, buffer = 3, nb_machines = 1)

    ## Update the real data tables in BD
    values$customerOrders <- MES_output[[1]]
    values$purchaseOrders <- MES_output[[2]]
    values$panneaux <- MES_output[[3]]

    ## Local data for interface
    data <- MES_output[[4]]

    ## Today Data
    data_today <- data %>%
      filter(str_split_i(start, " ", 1) == input$dayPlanif)
    data_today_to_timevis <- data_today %>%
      mutate(content = ifelse(type=="range", paste("PanneauID", content, sep=" "), content))
    # Today's unique groups (timeline)
    data_today_groups <- data_today %>%
      select(group) %>%
      mutate(group2 = group) %>%
      distinct() %>%
      rename(id = group, content = group2)
    #Get today planif (table output)
    data_today_small <- data_today %>% select(-FichierDecoupe)
    panneau_df <- merge(values$panneaux, data_today_small, by.x='PanneauID', by.y = "content") %>% select(-type,-group)
    #Get today fournisseurs (table output)
    fournisseurs_today <- values$purchaseOrders %>% filter(as.Date(DateCommandeFReception) == input$dayPlanif)

    ## Week Data
    data_to_timevis <- data %>% mutate(content = ifelse(type == "range",paste("PanneauID", content, sep=" "),content))
    data_groups <- MES_output[[5]]
    #Get fournisseurs in planif (table output)
    fournisseurs_planif <- values$purchaseOrders %>% filter(as.Date(DateCommandeFReception) >= input$dayPlanif)

    ## Timeline
    values$panelDF <- panneau_df |>
      mutate(DateFabrication=format(as.Date(DateFabrication), "%Y-%m-%d"), StartTask = str_split_i(start, " ", 2), EndTask = str_split_i(end, " ", 2), .keep = "unused")
    values$manufacturerDF <- fournisseurs_today |>
      mutate(DateCommande=format(as.Date(DateCommandeFCreation), "%Y-%m-%d"), DateReception=format(DateCommandeFReception, "%Y-%m-%d"), .keep = "unused")
    values$todayDF <- data_today_to_timevis
    values$todayGroupsDF <- data_today_groups
    values$weekDF <- data_to_timevis
    values$weekGroupsDF <- data_groups
    values$weekFournisseurs <- fournisseurs_planif |>
      mutate(DateCommande=format(as.Date(DateCommandeFCreation), "%Y-%m-%d"), DateReception=format(DateCommandeFReception, "%Y-%m-%d"), .keep = "unused")

    # Affichage par défaut
    output$tableWeekly_1 <- renderTable(values$weekFournisseurs)

    # Affichage par défaut
    output$timelineDaily <- renderTimevis({
      timevis(data=values$todayDF |> filter(!(hour(time) >= 0 & hour(time) < 8)), groups=values$todayGroupsDF)
      #options=list(
      #  hiddenDates = htmlwidgets::JS("{start: '2023-06-03 00:00:00', end: '2023-06-05 00:00:00', [repeat:'weekly']}")))
    })
    output$tableDaily_1 <- renderTable(values$panelDF)  #Current day panneaux prod
    output$tableDaily_2 <- renderTable(values$manufacturerDF) #Current day fournisseurs recus
    output$timelineWeekly <- renderTimevis({
      timevis(data=values$weekDF, groups=values$weekGroupsDF)
    })
    #TODO : Save planif data to some BD
    # sheet_append  somehwere
  })
  observeEvent(input$HeaderButtonHelp, {

    shinyWidgets::sendSweetAlert(
      session, title = "Comment utiliser cette application?",
      closeOnClickOutside = T, type = "info", html = T, width = "65%", text =
    # showModal(modalDialog(title = "Comment utiliser cette application?", easyClose = TRUE,
      HTML("<div class='text-left'>
        L'application comporte plusieurs onglets selon vos besoins:
        <br>-   Le tableau de bord contient des informations sur l'usine ...
        <br>-   L'inventaire contient l'inventaire actuel de l'usine et permet l'ajout de nouveaux items
        <br>-   L'onglet de réception permet de tenir compte des commandes auprès des fournisseurs pour l'équipe d'achat et les employés qui font la réception
        <br>-   L'onglet d'expédition permet de tenir compte de l'état des commandes (y compris la MAJ de ceux-ci lors de emballage et l'expédition)
        <br>-   L'onglet de production comporte 3 sous-onglets
        <br>    -   L'horaire de l'usine et des employés
        <br>    -   L'horaire de production pour la journée
        <br>    -   L'horaire de production pour la semaine
        </div>
      ")
    )#)
  })
}
################
## END SERVER ##
################

shinyApp(ui, server)