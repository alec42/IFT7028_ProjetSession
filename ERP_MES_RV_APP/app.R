library(tidyverse)
library(plotly)
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
    header = shinydashboard::dashboardHeader(
    ),
    
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

            shinydashboard::tabItem(tabName ="inventory"
            ),

            shinydashboard::tabItem(tabName ="clientOrders",
                    shiny::textInput("orderID", "Numéro de commande"),
                    shiny::selectInput("statusChoice", "Choix Statut", c("Modifiable", "Complétée", 
                                                                         "En production", "Planifiée", 
                                                                         "Commandée", "Expédiée",
                                                                         "En attente de matériaux")),
                    shiny::actionButton("updateStatus", "Mettre à jour le statut"),
                    shiny::actionButton("refreshBtn", "Annuler"),
                    shiny::actionButton("saveBtn", "Enregistrer"),
                    DT::dataTableOutput('CustomerOrders_DT')
            ),

            shinydashboard::tabItem(tabName ="purchaseOrders",
                shiny::textInput("orderID_PO", "Numéro de commande"),
                shiny::selectInput("statusChoice_PO", "Choix Statut", c("Commandée", "Reçue")),
                shiny::actionButton("updateStatus_PO", "Mettre à jour le statut"),
                shiny::actionButton("refreshBtn_PO", "Annuler"),
                shiny::actionButton("saveBtn_PO", "Enregistrer"),
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
  
  #######################
  ## Données réactives ##
  #######################
  values <- reactiveValues()
  values$customerOrders <- read_sheet(link_gs, sheet = "commandes_clients")
  values$purchaseOrders <- read_sheet(link_gs, sheet = "commandes_fournisseurs")
  
  ##########################
  ## Affichage par défaut ##
  ##########################
  output$CustomerOrders_DT <- outputDT(values$customerOrders)
  output$PurchaseOrders_DT <- outputDT(values$purchaseOrders)
  
  ##############################
  ## Menu : commandes clients ##
  ##############################
  
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus,
               {
                 values$customerOrders[as.character(values$customerOrders$CommandID) == input$orderID,]$Statut <- input$statusChoice
                 output$CustomerOrders_DT <- outputDT(values$customerOrders)
               }
               )
  
  # Button : Annuler
  observeEvent(input$refreshBtn,
               {
                 values$customerOrders<- read_sheet(link_gs, sheet = "commandes_clients")
                 output$CustomerOrders_DT <- outputDT(values$customerOrders)
               }
               )
  
  # Button : Enregistrer
  observeEvent(input$saveBtn,
               {
                 googlesheets4::sheet_write(data = values$customerOrders, ss = link_gs, sheet = "commandes_clients")
               }
               )
  

  ###################################
  ## Menu : commandes fournisseurs ##
  ###################################
  
  # Button : Mettre à jour le statut
  observeEvent(input$updateStatus_PO,
               {
                 values$purchaseOrders[as.character(values$purchaseOrders$CommandID) == input$orderID_PO,]$Statut <- input$statusChoice_PO
                 output$PurchaseOrders_DT <- outputDT(values$purchaseOrders)
               }
  )
  
  # Button : Annuler
  observeEvent(input$refreshBtn_PO,
               {
                 values$purchaseOrders<- read_sheet(link_gs, sheet = "commandes_fournisseurs")
                 output$PurchaseOrders_DT <- outputDT(values$purchaseOrders)
               }
  )
  
  # Button : Enregistrer
  observeEvent(input$saveBtn_PO,
               {
                 googlesheets4::sheet_write(data = values$purchaseOrders, ss = link_gs, sheet = "commandes_fournisseurs")
               }
  )
  
}

shinyApp(ui, server)