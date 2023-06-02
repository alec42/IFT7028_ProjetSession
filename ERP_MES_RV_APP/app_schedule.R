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

# data_today <- data.frame()
# data_today_groups <- data.frame()
# for (row in 1:nrow(data)){
#   date_to_compare = strsplit(data[row,"start"], " ")
#   #print(date_to_compare[[1]][1])
#   if (date_to_compare[[1]][1] == today){
#     #print(data[row,])
#     data_today <- rbind(data_today, data[row,])
#     #print(c(data[row,"groups"],data[row,"groups"]))
#     data_today_groups <- rbind(data_today_groups,c(data[row,"group"],data[row,"group"]))
#   }
# }
# colnames(data_today_groups) = c("id","content")
# data_today_groups <- data_today_groups[!duplicated(data_today_groups),]

#Construct table for one day planif
panneau_df <- data_today %>%
  filter(type == "range") %>%
  select(-type) %>%
  mutate(
    Status = "TODO",
    Ref = paste("googleDrive\\complet\\", group, "\\", content, "\\info_file.json"), #TODO : Generate the path to file correctly
    id = seq_along(group) - 1  #TODO :Get panneau ID from database
  )

# panneau_df <- data.frame()
# id_counter = 0
# for (row in 1:length(data_today)){
#   if (data_today[row,"type"] == "range"){ #Only put the panneaux in the table for panneaux
#     #print(data_today[row,-5]) #Drop the type column
#     new_row <- data_today[row,-5]
#     new_row["Status"] <- "TODO"
#     new_row["Ref"] <- paste("googleDrive\\complet\\",data_today[row,"group"],"\\",data_today[row,"content"],"\\info_file.json")
#     #TODO : Generate the path to file correctly
#     new_row["id"] <- id_counter #TODO :Get panneau ID from database
#     id_counter = id_counter +  1
#     panneau_df <- rbind(panneau_df, new_row)
#   }
# }

#Table of entering commandes fournisseurs
fournisseurs_df <- data_today %>%
  filter(type == "point") %>%
  select(content) %>%
  rename(Fournisseur = content)

# fournisseurs_df <- data.frame()
# for (row in 1:nrow(data_today)){
#   #print(data_today[row,"type"])
#   if (data_today[row,"type"] == "point"){ #Only put the panneaux in the table for panneaux
#
#     fournisseurs_df <- rbind(fournisseurs_df, data_today[row,"content"])
#   }
# }
# colnames(fournisseurs_df) <- c("Fournisseur")

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
    #timevis(data=data, groups=data_groups)
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
