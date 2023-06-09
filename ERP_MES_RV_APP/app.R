library(tidyverse)
library(DT)
library(shiny)
library(scales)
library(rjson)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(timevis)

###########################
## Interface Utilisateur ##
###########################

ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(),

  ##########
  ## Menu ##
  ##########
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id="sidebarMenu",
      shinydashboard::menuItem("Tableau de bord", tabName = "clientOrders", icon = icon("dashboard")),
      shinydashboard::menuItem("Inventaire", tabName = "inventory", icon = icon("list-alt")),
      shinydashboard::menuItem("Réception (fournisseurs)", tabName = "purchaseOrders", icon = icon("envelope")),
      shinydashboard::menuItem("Expédition (client)", tabName = "expedition", icon = icon("road")),
      shinydashboard::menuItem("Production journalière", tabName = "dailyProduction", icon = icon("calendar")),
      shinydashboard::menuItem("Production hebdomadaire", tabName = "weeklyProduction", icon = icon("calendar"),selected=TRUE)
    )
  ),

  ##########
  ## Body ##
  ##########
  body = shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(".box {overflow: scroll;}"))),

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

      #### Planification journalière ####
      shinydashboard::tabItem(tabName ="dailyProduction",
        shiny::textInput("panneauID", "ID du panneau"),
        shiny::selectInput("panneauStatus", "Choix statut", c("TODO", "DONE")),
        shiny::actionButton("updatePanneauBtn", "Mettre à jour le statut"),
        shiny::actionButton("cancelPanneauBtn", "Annuler"),
        shiny::actionButton("savePanneauBtn", "Enregistrer"),
        DT::dataTableOutput('Panneaux_DT'),
        shinydashboardPlus::box(title = "Planification pour la journée", solidHeader = TRUE, width = 12,
        timevisOutput("timelineDaily"),
        tableOutput('tableDaily_1'),
        tableOutput('tableDaily_2')
        )
      ),

      #### Planification hebdomadaire ####
      shinydashboard::tabItem(tabName ="weeklyProduction",
        shinydashboardPlus::box(title = "Horaire de l'usine pour la semaine", solidHeader = TRUE, width = 12,
          splitLayout(cellWidths = c("0", "8%", "6%", "6%", "5%"),
              tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            shiny::selectInput("factoryDay", "Jour de la semaine", choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),
            shiny::selectInput("factoryClose", "Fermeture", choices = sprintf("%02d:00:00", seq(8, 18))),
            shiny::selectInput("factoryOpen", "Ouverture", choices = sprintf("%02d:00:00", seq(8, 18))),
            br(),
            tableOutput('factoryHoursTable')
          ),
          shiny::actionButton("updateFactoryHours", "Mettre à jour les heures"),
          shiny::actionButton("refreshFactoryHours", "Annuler"),
          shiny::actionButton("saveFactoryHours", "Sauvegarder")
        ),
        shinydashboardPlus::box(title = "Horaire de l'employé pour la semaine", solidHeader = TRUE, width = 12,
          shinyWidgets::checkboxGroupButtons(
            inputId = "employeeSchedule", label = "Disponibilités",
            choices = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o", style = "color: steelblue"))
          )
        ),
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

source("scripts/googlesheets_access.R") # get link to gs
source("scripts/planning_algo.R")
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

############################
# Getting Planning ########
###########################
# add a button for max_range (hirozn planif), buffer (horizon_gelé), and nb_machines
MES_output <- MES_planif(values$customerOrders, values$inventory, values$purchaseOrders, values$panneaux, values$items, values$factory, values$employees, Sys.Date(), max_range = 5, buffer = 3, nb_machines = 1)

#Update the real data tables in BD
Commande <- MES_output[[1]]
CommandesFournisseurs <- MES_output[[2]]
PanneauDetail <- MES_output[[3]]

#Local data for interface
data <- MES_output[[4]]
data_to_timevis <- data %>% mutate(content = ifelse(type == "range",paste("PanneauID", content, sep=" "),content))
data_groups <- MES_output[[5]]

#TODO : Save planif data to some BD
# sheet_append  somehwere

#Get today prod -- for timeline
data_today <- data %>%
  filter(str_split_i(start, " ", 1) == today)
data_today_to_timevis <- data_today %>% mutate(content = ifelse(type=="range", paste("PanneauID", content, sep=" "), content))

#Get today unique groups -- for timeline
data_today_groups <- data_today %>%
  select(group) %>%
  mutate(group2 = group) %>%
  distinct() %>%
  rename(id = group, content = group2)
## Getting Planning - Code de Laurence à bouger et intégrer dans l'app

#Get today planif -- for table
data_today_small <- data_today %>% select(-FichierDecoupe)
panneau_df <- merge(PanneauDetail, data_today_small, by.x='PanneauID', by.y = "content") %>% select(-type,-group)

#Get today fournisseurs -- for table
fournisseurs_today <- CommandesFournisseurs %>% filter(DateCommandeFReception == today)

#Get fournisseurs in planif -- for table
fournisseurs_planif <- CommandesFournisseurs %>% filter(DateCommandeFReception >= today)
#DONE planif

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
  values$factory <- read_sheet(link_gs_erp, sheet = factoryHoursSheetName) |> mutate(across(c(Monday, Tuesday, Wednesday, Thursday, Friday), ~format(.x, "%H:%M:%S")))

  ## Timeline
  values$panelDF <- panneau_df
  values$manufacturerDF <- fournisseurs_today
  values$todayDF <- data_today_to_timevis
  values$todayGroupsDF <- data_today_groups
  values$weekDF <- data_to_timevis
  values$weekGroupsDF <- data_groups
  values$weekFournisseurs <- fournisseurs_planif


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
                                            MinStock = input$inventory_MinStockSelect
    values$inventory <- values$inventory |> add_row(
      ItemID=nrow(values$items) + 1, QuantiteDisponible=0, DateMiseAJour=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
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

  # # Affichage par défaut
  # output$timelineDaily <- renderTimevis({
  #   timevis(data=values$todayDF, groups=values$todayGroupsDF)
  #   #options=list(
  #   #  hiddenDates = htmlwidgets::JS("{start: '2023-06-03 00:00:00', end: '2023-06-05 00:00:00', [repeat:'weekly']}")))
  # })
  # output$tableDaily_1 <- renderTable(values$panelDF)  #Current day panneaux prod
  # output$tableDaily_2 <- renderTable(values$manufacturerDF) #Current day fournisseurs recus
  # output$timelineWeekly <- renderTimevis({
  #   timevis(data=values$weekDF, groups=values$weekGroupsDF)
  # })

  # Affichage : État du panneau
  output$Panneaux_DT <- renderDT(
    values$panneaux |> filter(PanneauID == input$panneauID),
    options = dt_options, rownames = FALSE, selection = "none")

  # Button : Mettre à jour le statut
  observeEvent(input$updatePanneauBtn, {
    values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$Statut <- input$panneauStatus
    if (input$panneauStatus == "DONE"){
      values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$DateFabrication <- Sys.Date()
    }

    if (prod(sapply(values$panneaux |>
          filter(CommandeID == values$panneaux[as.character(values$panneaux$PanneauID) == input$panneauID, ]$CommandeID)
          |> select("Statut"), function(x) x == "DONE")) == 1){
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

  # # Affichage par défaut
  # output$tableWeekly_1 <- renderTable(values$weekFournisseurs)
  # Button : Mettre à jour les heures
  output$factoryHoursTable <- renderTable(values$factory)
  observeEvent(input$updateFactoryHours, {
    values$factory[input$factoryDay] <- ifelse(values$factory["MomentID"]=="Ouverture", input$factoryOpen, input$factoryClose)
    print(values$factory)
  })

  # Button : Annuler
  observeEvent(input$refreshFactoryHours, {
    values$factory <- read_sheet(link_gs_erp, sheet = factoryHoursSheetName) |>
      mutate(across(c(Monday, Tuesday, Wednesday, Thursday, Friday), ~format(.x, "%H:%M:%S")))
  })

  # Button : Enregistrer
  observeEvent(input$saveFactoryHours, {
    googlesheets4::sheet_write(data = values$factory, ss = link_gs_erp, sheet = factoryHoursSheetName)
  })
}
################
## END SERVER ##
################

shinyApp(ui, server)