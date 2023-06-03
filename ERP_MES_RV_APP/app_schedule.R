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

source("scripts/googlesheets_access.R") # get link to gs

#Data to put in the schedule
data <- data.frame(
  content = c("P1", "P2", "P3", "P4",
              "P1", "P2", "P3", "P4",
              "Fournisseur XX"),
  start   = c("2023-06-01 08:00:00", "2023-06-01 10:00:00","2023-06-01 12:00:00", "2023-06-01 14:00:00",
              "2023-06-02 08:00:00", "2023-06-02 10:00:00", "2023-06-03 08:00:00", "2023-06-02 16:00:00",
              "2023-06-01 08:00:00"),
  end     = c("2023-06-01 10:00:00", "2023-06-01 12:00:00","2023-06-01 14:00:00", "2023-06-01 16:00:00",
              "2023-06-02 10:00:00", "2023-06-02 12:00:00", "2023-06-03 10:00:00", "2023-06-02 18:00:00",
              NA),
  group = c(rep("Commande 1",4),rep("Commande 2",4), "Receptions fournisseurs"),
  type = c(rep("range",4), rep("range",4), "point")
)

data_groups <- data.frame(
  id = c("Commande 1", "Commande 2", "Receptions fournisseurs"),
  content = c("Commande 1", "Commande 2", "Receptions fournisseurs")
)

# ------------- TODAY schedule -------------------------

#Get the commandes for today
today = "2023-06-01" #TODO : Function that keeps track of the time
# today <- Sys.Date()
data_today <- data %>%
  filter(str_split_i(start, " ", 1) == today)

data_today_groups <- data_today %>%
  select(group) %>%
  mutate(group2 = group) %>%
  distinct() %>%
  rename(id = group, content = group2)

#Construct table for one day planif
panneau_df <- data_today %>%
  filter(type == "range") %>%
  select(-type) %>%
  mutate(
    Status = "TODO",
    Ref = paste("googleDrive\\complet\\", group, "\\", content, "\\info_file.json"), #TODO : Generate the path to file correctly
    id = seq_along(group) - 1  #TODO :Get panneau ID from database
  )

#Table of entering commandes fournisseurs
fournisseurs_df <- data_today %>%
  filter(type == "point") %>%
  select(content) %>%
  rename(Fournisseur = content)

#TODO : Insert button to update the status when done
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
  values$manufacturerDF <- fournisseurs_df
  values$todayDF <- data_today
  values$todayGroupsDF <- data_today_groups
  values$weekDF <- data
  values$weekGroupsDF <- data_groups

  output$timeline <- renderTimevis({
    #Uncomment this one for the whole week :
    timevis(data=values$weekDF, groups=values$weekGroupsDF)

    #Uncomment this one for the current day :
    # timevis(data=values$todayDF, groups=values$todayGroupsDF)
  })

  #Comment these two if looking at whole week
  output$table1 <- renderTable(values$panelDF)  #Current day panneaux prod
  output$table2 <- renderTable(values$manufacturerDF) #Current day fournisseurs recus
}

# Run the application
shinyApp(ui = ui, server = server)
