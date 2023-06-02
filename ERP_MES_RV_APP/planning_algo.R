library(dplyr)
library(lubridate)

#Data from database (temporary) : 

Commande <- data.frame(
  CommandeID = c("1","2","3"),
  ClientID = c("11","12","13"),
  FichierFabrication = c("googleDrive\\complet\\1\\infos.json","googleDrive\\complet\\2\\infos.json","googleDrive\\complet\\3\\infos.json"),
  Statut = c("Modifiable","Commandée","En prod"),
  CommandeDetailID = c("1","2","3"),
  DateLivraison = c("2023-06-05", "2023-06-08", "2023-06-04"),
  P1 = c(2,2,2),
  P2 = c(1,1,2),
  P3 = c(1,2,1)
)

CommandeDetail <- data.frame(
  CommandeID = c(rep("1",4),rep("2",5),rep("3",5)),
  PanneauID = c("1-P1-1","1-P1-2","1-P2-1","1-P3-1",
                "2-P1-1","2-P1-2","2-P2-1","2-P3-1","2-P3-2", 
                "3-P1-1","3-P1-2","3-P2-1","3-P2-2","3-P3-1"),
  PanneauType = c("P1","P2","P2","P3",
                  "P1","P1","P2","P3","P3",
                  "P1","P1","P2","P2","P3"),
  Statut = c(rep("TODO",14))
)

Inventaire <- data.frame(
  ItemID = c("P1","P2","P3","Colle","Vernis"),
  Qtte = c(2,4,5,2,4),
  MinStock = c(3,3,3,3,3)
)

CommandesFournisseurs <- data.frame(
  Fournisseur = c("XX"),
  DateReception = c("2023-06-02"),
  Item = c("P1"),
  Qtte = c(4),
  Statut = c("En attente")
)


#------------------- Utility functions for planification algo ----------------------------
#Get dates
get_dates_to_plan <- function(today, max_range){
  plan_times = c()
  counter = 0
  while (length(plan_times) != max_range){
    next_day <- (as.Date(today)+counter)
    
    if (wday(next_day) != 1 & wday(next_day) != 7){ #Sunday = 1, Saturday = 7, skip them
      plan_times <- append(plan_times, next_day)
    }
    counter = counter + 1
  }
  plan_times
}

#Planning from list of commandes we can start today + update inventory
plan <- function(plan_today, inventory, planif_data, days_forward, day_idx, max_per_time){
  #Plan_today contains the row od the commande to plan
  #planif_data is the dataframe containing all already planned stuff
  list_of_times = c("08:00:00","10:00:00","12:00:00","14:00:00","16:00:00")
  counter_times = 1
  counter_days = 1
  #days_forward = plan_times
  #day_idx = 1
  if (nrow(plan_today) > 0){ #If we have something to plan for today
    for (i in 1:nrow(plan_today)){
      r = planned_today[i,]
      r["CommandeID"]
      panneaux_todo <- CommandeDetail %>% filter(CommandeID == r[["CommandeID"]], Statut == "TODO")
      panneaux_todo
  
      for (j in 1:nrow(panneaux_todo)){
        #Check if we need to change day
        
        if (counter_times > 4){
          counter_times = 1 #Restart the day
          if (counter_days + 1 > length(days_forward)){ #Do not go further than the horizon of days we want
            return(list(planif_data, inventory))
          }
          else{
            counter_days = counter_days + 1
            day_idx = day_idx + 1 #move to next day
            #print(day_idx)
            #day_idx = 2
            
            #inventory[[day_idx-1]]
            inventory[day_idx] <-  inventory[day_idx-1]
            #print("Updating next inventory day")
            #print(inventory[day_idx])
            #inventory[day_idx] <- list(days_forward[counter_days],inventory[[day_idx-1]]) #update the inventroy of next day to be same as previous day
          }
        }
        
        #Check that there is not already something planned on this day at that time when calling this again
        
        start_time = paste(days_forward[counter_days],list_of_times[counter_times],sep= " ")
        start_time
        #print(start_time)
        
        #print(planif_data)
        current_work <- planif_data %>% filter(start == start_time)
        current_work
        #print(current_work)
        
        if (!(nrow(current_work) == max_per_time)){
          new_task <- c(panneaux_todo[j,"PanneauID"],start_time, paste(days_forward[counter_days],list_of_times[counter_times+1],sep= " "), paste("Commande",r[["CommandeID"]],sep=" "), "range")
          new_task
          
          #planif_data <- rbind(planif_data, new_task)
          planif_data[nrow(planif_data)+1,] <- new_task
          #print(planif_data)
          #Remove 1 from inventaire for this day and panneau
          #day_idx
          #inventory[[day_idx]]
          #panneaux_todo[i,"PanneauType"]
          #print(day_idx)
          new_inv <- update_inventory_from_plan(inventory[[day_idx]], panneaux_todo[j,"PanneauType"])
          #new_inv
          inventory[[day_idx]]<- new_inv
          #inventory[[day_idx]]
        }
        
        counter_times = counter_times + 1
        
      } #This finishes all panneaux for one commande
      planif_data
    }#This finishes for all commandes we can start today
  }
  else{
    #day_idx = day_idx + 1 #move to next day
    
    #Copy inventory since it has not changed
    inventory[day_idx+1] <-  inventory[day_idx]
    #counter_days = counter_days + 1
  }
  return(list(planif_data, inventory))
}

update_inventory_from_plan <- function(inventory_day, panneau_ID, qtty_less = 1){
  #inv <- inventory[day_idx][[2]][1]
  #print(inventory_day)
  inv <- inventory_day %>% mutate(Qtte = ifelse(ItemID == panneau_ID, Qtte - qtty_less, Qtte))
  #print(inv)
  return(inv)
}


#Update the inventory from receiving deliveries --- OK
update_inventory_from_deliveries <- function(fournisseurs_today, inv_today, planif, day_today){
  #Adjust item values
  #print(day)
  for (i in 1:nrow(fournisseurs_today)){
    item = fournisseurs_today[i,'Item']
    #print(item)
    qtty = as.integer(fournisseurs_today[i,'Qtte'])
    #print(qtty)
    #print(inv_today)
    inv_today <- inv_today %>% mutate(Qtte = ifelse(ItemID == item, Qtte + qtty, Qtte))
    #print(inv_today)
    #inv_today[inv_today['ItemID'] == item,"Qtte"] <- inv_today[inv_today['ItemID'] == item,"Qtte"] + qtty
  }
  #print(planif)
  #Add the commandes reception to planif
  fournisseurs_names <- fournisseurs_today['Fournisseur']
  f <- unique(fournisseurs_names)
  for (little_f in f){
    
    day_to_use <- as.character(day_today)
    #print(day_to_use)
    new_row <- c(little_f, day_to_use, NA, "Receptions fournisseurs", "point")
    planif[nrow(planif)+1,] <- new_row
  }
  #print(planif)
  return(list(inv_today, planif))
  
}

#Add commande in fournisseur
add_fournisseur_commande <- function(commandesfournisseurs, itemID, qtty, today_date, max_date){
  #Check if commande already there for this product and quantity
  #print(commandesfournisseurs)
  #print(itemID)
  next_commandes <- commandesfournisseurs %>% filter(Item == itemID)#, Statut == "En attente")
  
  
  #Only add the commande if no commande for this product is waiting
  if (nrow(next_commandes) == 0){
    #Get fournisseur from database
    #fournisseur <- Item %>% filter(ItemID = itemID) %>% select(Fournisseur)
    livraison = as.character(as.Date(today_date)+3)
    if (wday(livraison) == 1){ #Sunday
      livraison = as.character(as.Date(livraison) + 1)
    }
    if (wday(livraison) == 6){ #Saturday
      livraison = as.character(as.Date(livraison) + 2)
    }
    
    #Only plan within the planning window
    #print(as.Date(livraison) <= as.Date(max_date))
    #print(livraison)
    #print(max_date)
    if (as.Date(livraison) <= as.Date(max_date)){
      new_commande <- c("YY", livraison, itemID , qtty, "En attente")
      
      commandesfournisseurs <- rbind(commandesfournisseurs, new_commande)
    }
    
  }
  return(commandesfournisseurs)
}

# ------------------------- Plannification algo --------------------------------

#Get dates to plan for
today = "2023-06-01"
max_range = 5
plan_times <- get_dates_to_plan(today, max_range)
plan_times

#Initialize inventory list for each planning day
inventory <- vector(mode="list",length=max_range) #first inventory is today's inventory
inventory[[1]] <- Inventaire
inventory

#Get commandes eligible for planning by livraison date
commandes_to_plan <- Commande %>% filter(Statut != "Modifiable") %>% arrange(DateLivraison)
planned_commandes <- c()
planned_today <- data.frame() #commandes to plan

#Initialize planif
planif_data <- data.frame(matrix(ncol=5,nrow=0))
colnames(planif_data) <- c("content","start","end","group","type")
planif_data

#Loop on each day one at a time
for (j in 1:length(plan_times)){ 
  if (j >= 3){
    return(1)
  }
  print(j)
  print(plan_times[[j]])
  inv_today <- inventory[[j]]#[2][[1]]
  #print(inv_today)
  #print(inventory)
  #inventory[[1]]
  
  #Get fournisseurs receptions for today if any
  fournisseurs_today = CommandesFournisseurs %>% filter(DateReception == plan_times[[j]])#, Statut == "En attente") #mutate(DateReception = as.Date(DateReception)) #%>% filter("DateReception" == plan_times[[j]])
  print(fournisseurs_today)
  
  if (nrow(fournisseurs_today != 0)){

    fournisseur_output <- update_inventory_from_deliveries(fournisseurs_today, inv_today, planif_data, plan_times[[j]])
    #Update inventory
    inv_today <- fournisseur_output[[1]]
    print(inv_today)
    inventory[[j]] <- inv_today
    print(inventory)

    #Update planif with fournisseur data
    planif_data <- fournisseur_output[[2]]
    
    #Update status of fournisseur commande comme recue pour la première journée de la planif seulement
    CommandesFournisseurs <- CommandesFournisseurs %>% mutate(Statut = ifelse(DateReception == plan_times[[1]],"Processed","En attente"))
    #print("Updated status of commande")
    #inv_today
    #print(CommandesFournisseurs)
    #planif_data
  }
  
 
  for (c in 1:nrow(commandes_to_plan)) {
    #Get the next commande
    commande <- commandes_to_plan[c,]
    commande
    
    #only plan the ones not yet planned
    if (!(commande[["CommandeID"]] %in% planned_commandes)){
      
      #Get the list of planneau for this commande
      #Make this compatible with real data -- get panneau names from Item DB
      panneau_names <- c("P1","P2","P3")
      panneau_liste <- commande %>% select(c("P1","P2","P3"))
      panneau_liste
      all_good = TRUE
      
      #panneau_liste[[panneau_names[1]]]
      #inv_today[inv_today["ItemID"]==panneau_names[1],"Qtte"]
      #Check if we have all material to do the commande today
      for (p in 1:nrow(panneau_liste)){
        panneau_liste
        if (panneau_liste[[panneau_names[p]]] > inv_today[inv_today["ItemID"]==panneau_names[p],"Qtte"]){
          all_good = FALSE #Cannot start this project
        }
        
        #Account for projects half done :
        #Suppose here that the number total of panneaux per commande is updated to the remaining number
        #When panneau is completed, do panneau category - 1
      }
      all_good
      
      #Debug from herre
      #If we have all material, add to list of commandes to do
      if (all_good){
        planned_today <- rbind(planned_today, commande)
        planned_today
        
        Commande[Commande["CommandeID"] == commande[['CommandeID']],"Statut"] = "Planned" 
        #Update this in real data
        
        #Remove commande from commandes_to_plan
        #commandes_to_plan <- commandes_to_plan %>% filter(CommandeID != commande[["CommandeID"]])
        planned_commandes <- append(planned_commandes, commande[["CommandeID"]])
      }
      else{
        #print(commande)
        #print(inventory[j])
        Commande[Commande["CommandeID"] == commande[["CommandeID"]],"Statut"] = "Attente d'inventaire"
        
      }
    }
    
    
    
  } #Here we got all the commandes we can start today
  
  
  #Debugged up to here -- ok
  #Plan according to the list of planned_today -- this also updates the inventory
  #print(inventory)
  max_per_time = 1
  plan_output <- plan(planned_today, inventory, planif_data, plan_times[j:length(plan_times)], j, max_per_time)
  
  planif_data <- plan_output[[1]]
  inventory <- plan_output[[2]]
  
  planif_data
  inventory

  #Demander commande fournisseur si en bas des stocks
  #for (inv_idx in 1:length(inventory)){
  inv_of_the_day <- inventory[j][[1]]
  inv_of_the_day
  
  
  if (!is.null(inv_of_the_day)){
    
    to_buy <- inv_of_the_day %>% filter(Qtte < MinStock)
    to_buy
    
    if (nrow(to_buy) > 0){
      for (e in 1:nrow(to_buy)){
        CommandesFournisseurs <- add_fournisseur_commande(CommandesFournisseurs, to_buy[e,"ItemID"],to_buy[e,"MinStock"], plan_times[[j]], plan_times[[length(plan_times)]])
      }
    }
    
  }
  #}
  
}
#Update true inventory from planif of today
Inventory <- inventory[1]
inventory

print(planif_data)
print(Inventory)
print(CommandesFournisseurs)
print(Commande)

