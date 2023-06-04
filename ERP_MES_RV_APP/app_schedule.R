#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(timevis)
library(DT)
library(tidyverse)
library(htmlwidgets)

#source("scripts/googlesheets_access.R") # get link to gs
source("planning_algo.R")

#Data from database (temporary) : 
Commande <- data.frame(
  CommandeID = c("1","2","3","4"),
  ClientID = c("11","12","13","14"),
  FichierFabrication = c("googleDrive\\complet\\1\\infos.json","googleDrive\\complet\\2\\infos.json","googleDrive\\complet\\3\\infos.json","googleDrive\\complet\\3\\infos.json"),
  Statut = c("Commandée","Commandée","En prod","Commandée"),
  CommandeDetailID = c("1","2","3","4"),
  DateCreation = c("2023-05-25","2023-05-27","2023-05-30","2023-05-29"),
  DateLivraison = c("2023-06-05", "2023-06-08", "2023-06-04","2023-06-10"),
  P1 = c(2,2,1,1),
  P2 = c(1,1,1,2),
  P3 = c(1,2,1,2)
)

CommandeDetail <- data.frame(
  CommandeID = c(rep("1",4),rep("2",5),rep("3",3), rep("4",5)),
  PanneauID = c("1-P1-1","1-P1-2","1-P2-1","1-P3-1",
                "2-P1-1","2-P1-2","2-P2-1","2-P3-1","2-P3-2", 
                "3-P1-1","3-P2-1","3-P3-1",
                "4-P1-1","4-P1-2","4-P2-1","4-P3-1","4-P3-1"),
  PanneauType = c("P1","P2","P2","P3",
                  "P1","P1","P2","P3","P3",
                  "P1","P2","P3",
                  "P1", "P2","P2", "P3","P3"),
  Statut = c(rep("TODO",17)),
  Fichier = c("googleDrive\\complet\\1\\C1\\P1-1.CAD","googleDrive\\complet\\1\\C1\\P1-2.CAD","googleDrive\\complet\\1\\C1\\P2-1.CAD","googleDrive\\complet\\1\\C1\\P3-1.CAD",
              "googleDrive\\complet\\1\\C2\\P1-1.CAD","googleDrive\\complet\\1\\C2\\P1-2.CAD","googleDrive\\complet\\1\\C2\\P2-1.CAD","googleDrive\\complet\\1\\C2\\P3-1.CAD","googleDrive\\complet\\1\\C2\\P3-2.CAD",
              "googleDrive\\complet\\1\\C3\\P1-1.CAD","googleDrive\\complet\\1\\C3\\P2-1.CAD","googleDrive\\complet\\1\\C3\\P3-1.CAD",
              "googleDrive\\complet\\1\\C4\\P1-1.CAD","googleDrive\\complet\\1\\C4\\P1-2.CAD","googleDrive\\complet\\1\\C4\\P2-1.CAD","googleDrive\\complet\\1\\C4\\P3-1.CAD","googleDrive\\complet\\1\\C4\\P3-2.CAD")
)

Inventaire <- data.frame(
  ItemID = c("P1","P2","P3","Colle","Vernis","Scie japonaise"),
  Qtte = c(2,4,5,2,4,5),
  MinStock = c(3,3,3,3,3,3)
)

CommandesFournisseurs <- data.frame(
  Fournisseur = c("XX"),
  DateReception = c("2023-06-02"),
  Item = c("P1"),
  Qtte = c(4),
  Statut = c("En attente")
)


#--------- Data from planning algo -------------------
#TODO : Date function to keep track of where we are
today = "2023-06-01"
MES_output <- MES_planif(Commande, Inventaire, CommandesFournisseurs, CommandeDetail, today, max_range = 6, buffer = 2, nb_machines = 1)

#Updat the real data tables
Commande <- MES_output[[1]]
Inventaire <- MES_output[[2]][[1]]
CommandesFournisseurs <- MES_output[[3]]

#Local data for interface
data <- MES_output[[4]]
data_groups <- MES_output[[5]]

# ------------- Generating tables from planning -------------------------

#Get today prod -- for timeline
data_today <- data %>%
  filter(str_split_i(start, " ", 1) == today)
data_today

#Get today unique groups -- for timeline
data_today_groups <- data_today %>%
  select(group) %>%
  mutate(group2 = group) %>%
  distinct() %>%
  rename(id = group, content = group2)


#Get today planif -- for table
panneau_df <- merge(CommandeDetail, data_today, by.x='PanneauID', by.y = "content") %>% select(-type,-group)
panneau_df

#Get today fournisseurs -- for table
fournisseurs_today <- CommandesFournisseurs %>% filter(DateReception == today)
fournisseurs_today

#Get fournisseurs in planif -- for table
fournisseurs_planif <- CommandesFournisseurs %>% filter(DateReception >= today)
fournisseurs_planif

#TODO : Insert button to update the status of each panneau
  #Add loop to update the status of the commande if all panneaux are done
#----------------------------------------------------


# Define UI for the timeline
ui <- fluidPage(
  timevisOutput("timeline"),
  fluidPage(
    fluidRow(
      column(12,
             tableOutput('table1')
      )
    )
  ),
  fluidPage(
    fluidRow(
      column(12,
             tableOutput('table2')
      )
    )
  )
)


# Define server logic to make the timeline appear
server <- function(input, output) {
  values <- reactiveValues()
  # values$customerOrders <- read_sheet(link_gs, sheet = customerOrdersSheetName)
  values$panelDF <- panneau_df
  values$manufacturerDF <- fournisseurs_today
  values$todayDF <- data_today
  values$todayGroupsDF <- data_today_groups
  values$weekDF <- data
  values$weekGroupsDF <- data_groups
  values$weekFournisseurs <- fournisseurs_planif

  output$timeline <- renderTimevis({
    #Whole planif :
    #timevis(data=values$weekDF, groups=values$weekGroupsDF)

    #Today planif :
    timevis(data=values$todayDF, groups=values$todayGroupsDF)#, 
            #options=list(
            #  hiddenDates = htmlwidgets::JS("{start: '2023-06-03 00:00:00', end: '2023-06-05 00:00:00', [repeat:'weekly']}")))
  })

  #Today tables
  output$table1 <- renderTable(values$panelDF)  #Current day panneaux prod
  output$table2 <- renderTable(values$manufacturerDF) #Current day fournisseurs recus
  
  #Full planif table
  #output$table1 <- renderTable(values$weekFournisseurs)
}

# Run the application
shinyApp(ui = ui, server = server)
