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
source("scripts/planning_algo.R")

#Data from database (temporary) :
items = c(c("1"=2,"2":1,"3":1), c("1":2,"2":1,"3":2), c("1":1,"2":1,"3":1), c("1":1,"2":2,"3":2))
item1 = c("1"=2,"2":1,"3":1)
item2 = c("1":2,"2":1,"3":2)
item3 = c("1":1,"2":1,"3":1)
item4 = c("1":1,"2":2,"3":2)
Commande <- data.frame(
  CommandeID = c("1","2","3","4"),
  ClientID = c("11","12","13","14"),
  FichierFabrication = c("googleDrive\\complet\\1\\infos.json","googleDrive\\complet\\2\\infos.json","googleDrive\\complet\\3\\infos.json","googleDrive\\complet\\3\\infos.json"),
  Statut = c("Commandée","Commandée","En prod","Commandée"),
  CommandeDetailID = c("1","2","3","4"),
  DateCommandeCreation = c("2023-05-25","2023-05-27","2023-05-30","2023-05-29"),
  DateCommandeLivraison = c("2023-06-05", "2023-06-08", "2023-06-04","2023-06-10"),
  FichierAssemblage = c("googleDrive\\complet\\1\\infos.json","googleDrive\\complet\\2\\infos.json","googleDrive\\complet\\3\\infos.json","googleDrive\\complet\\3\\infos.json")#,
  #Items = list(list("1"=2,"2":1,"3":1),
               #c("1":2,"2":1,"3":2),
               #c("1":1,"2":1,"3":1),
               #c("1":1,"2":2,"3":2))
)


PanneauDetail <- data.frame(
  CommandeID = c(rep("1",4),rep("2",5),rep("3",3), rep("4",5)),
  PanneauID = c(1,2,3,4,
                5,6,7,8,9,
                10,11,12,
                13,14,15,16,17),
  PanneauType = c(1,2,2,3,
                  1,1,2,3,3,
                  1,2,3,
                  1, 2,2,3,3),
  Statut = c(rep("TODO",17)),
  DatePrevue = c(rep("",17)),
  FichierDecoupe = c("googleDrive\\complet\\1\\C1\\P1-1.CAD","googleDrive\\complet\\1\\C1\\P1-2.CAD","googleDrive\\complet\\1\\C1\\P2-1.CAD","googleDrive\\complet\\1\\C1\\P3-1.CAD",
              "googleDrive\\complet\\1\\C2\\P1-1.CAD","googleDrive\\complet\\1\\C2\\P1-2.CAD","googleDrive\\complet\\1\\C2\\P2-1.CAD","googleDrive\\complet\\1\\C2\\P3-1.CAD","googleDrive\\complet\\1\\C2\\P3-2.CAD",
              "googleDrive\\complet\\1\\C3\\P1-1.CAD","googleDrive\\complet\\1\\C3\\P2-1.CAD","googleDrive\\complet\\1\\C3\\P3-1.CAD",
              "googleDrive\\complet\\1\\C4\\P1-1.CAD","googleDrive\\complet\\1\\C4\\P1-2.CAD","googleDrive\\complet\\1\\C4\\P2-1.CAD","googleDrive\\complet\\1\\C4\\P3-1.CAD","googleDrive\\complet\\1\\C4\\P3-2.CAD")
)

Inventaire <- data.frame(
  ItemID = c(1,2,3,4,5,6,7),
  QuantiteDisponible = c(4,2,5,2,4,5,2)
)

CommandesFournisseurs <- data.frame(
  CommandeFournisseurID = c(1),
  Statut = c("Commandée"),
  DateCommandeFCreation = c("2023-05-30"),
  DateCommandeFReception = c("2023-06-02"),
  ItemID = c(1),
  Quantité = c(4)
)

Items <- data.frame(
  ItemID = c(1,2,3,4,5,6,7),
  Fournisseur = c("Reno Depot","Canadian Tire", "BMR","Home Depot","Reno Depot","Canadian Tire","BMR"),
  MinStock = c(3,3,3,3,3,3,3),
  DélaisLivraison = c(1,2,2,2,2,2,8),
  TempsFabrication = c(2,2,2,2,2,2,2)
)

DisposEmployers <- data.frame(
  EmployerID = c(1,2,3,4),
  Monday = c(0,0,0,0), #monday
  Tuesday = c(1,1,0,0), #tuesday
  Wednesday = c(1,1,1,1), #wednesday
  Thursday = c(1,0,1,1), #thursday
  Friday = c(1,0,0,0) #friday
)

DisposUsine <- data.frame(
  Monday = c("08:00:00","16:00:00"),
  Tuesday = c("08:00:00","16:00:00"),
  Wednesday = c("08:00:00","12:00:00"),
  Thursday = c("08:00:00","16:00:00"),
  Friday = c("08:00:00","12:00:00")
)


#--------- Data from planning algo -------------------
today = "2023-06-01"
MES_output <- MES_planif(Commande, Inventaire, CommandesFournisseurs, PanneauDetail, Items, DisposUsine, DisposEmployers, today, max_range = 5, buffer = 3, nb_machines = 1)

#Update the real data tables in BD
Commande <- MES_output[[1]]
CommandesFournisseurs <- MES_output[[2]]
PanneauDetail <- MES_output[[3]]

#Local data for interface
data <- MES_output[[4]]
data_to_timevis <- data %>% mutate(content = ifelse(type == "range",paste("PanneauID", content, sep=" "),content))

data_groups <- MES_output[[5]]

#TODO : Save planif data to some BD

# ------------- Generating tables from planning -------------------------

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


#Get today planif -- for table
data_today_small <- data_today %>% select(-FichierDecoupe)
panneau_df <- merge(PanneauDetail, data_today_small, by.x='PanneauID', by.y = "content") %>% select(-type,-group)

#Get today fournisseurs -- for table
fournisseurs_today <- CommandesFournisseurs %>% filter(DateCommandeFReception == today)

#Get fournisseurs in planif -- for table
fournisseurs_planif <- CommandesFournisseurs %>% filter(DateCommandeFReception >= today)


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
  values$todayDF <- data_today_to_timevis
  values$todayGroupsDF <- data_today_groups
  values$weekDF <- data_to_timevis
  values$weekGroupsDF <- data_groups
  values$weekFournisseurs <- fournisseurs_planif

  output$timeline <- renderTimevis({
    #Whole planif :
    timevis(data=values$weekDF, groups=values$weekGroupsDF)

    #Today planif :
    #timevis(data=values$todayDF, groups=values$todayGroupsDF)#,
            #options=list(
            #  hiddenDates = htmlwidgets::JS("{start: '2023-06-03 00:00:00', end: '2023-06-05 00:00:00', [repeat:'weekly']}")))
  })

  #Today tables
  #output$table1 <- renderTable(values$panelDF)  #Current day panneaux prod
  #output$table2 <- renderTable(values$manufacturerDF) #Current day fournisseurs recus

  #Full planif table
  output$table1 <- renderTable(values$weekFournisseurs)
}

# Run the application
shinyApp(ui = ui, server = server)
