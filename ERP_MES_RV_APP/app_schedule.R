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
data_today = data.frame()
data_today_groups = data.frame()

#Get the commandes for today
today = "2023-06-01" #TODO : Function that keeps track of the time
for (row in 1:nrow(data)){
  date_to_compare = strsplit(data[row,"start"], " ")
  #print(date_to_compare[[1]][1])
  if (date_to_compare[[1]][1] == today){
    #print(data[row,])
    data_today <- rbind(data_today, data[row,])
    #print(c(data[row,"groups"],data[row,"groups"]))
    data_today_groups <- rbind(data_today_groups,c(data[row,"group"],data[row,"group"]))
  }
}
colnames(data_today_groups) = c("id","content")
data_today_groups <- data_today_groups[!duplicated(data_today_groups),]

#Construct table for one day planif
panneau_df <- data.frame()
id_counter = 0
for (row in 1:length(data_today)){
  if (data_today[row,"type"] == "range"){ #Only put the panneaux in the table for panneaux
    #print(data_today[row,-5]) #Drop the type column
    new_row <- data_today[row,-5]
    new_row["Status"] <- "TODO"
    new_row["Ref"] <- paste("googleDrive\\complet\\",data_today[row,"group"],"\\",data_today[row,"content"],"\\info_file.json")
    #TODO : Generate the path to file correctly
    new_row["id"] <- id_counter #TODO :Get panneau ID from database
    id_counter = id_counter +  1
    panneau_df <- rbind(panneau_df, new_row)
  }
}

#Table of entering commandes fournisseurs
fournisseurs_df <- data.frame()
for (row in 1:length(data_today)){
  #print(data_today[row,"type"])
  if (data_today[row,"type"] == "point"){ #Only put the panneaux in the table for panneaux
    
    fournisseurs_df <- rbind(fournisseurs_df, data_today[row,"content"])
  }
}
colnames(fournisseurs_df) <- c("Fournisseur")

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

  output$timeline <- renderTimevis({
    #Uncomment this one for the whole week :
    #timevis(data=data, groups=data_groups)
    
    #Uncomment this one for the current day :
    timevis(data=data_today, groups=data_today_groups)
  })
  
  #Comment these two if looking at whole week
  output$table1 <- renderTable(panneau_df)  #Current day panneaux prod
  output$table2 <- renderTable(fournisseurs_df) #Current day fournisseurs recus
}

# Run the application 
shinyApp(ui = ui, server = server)
