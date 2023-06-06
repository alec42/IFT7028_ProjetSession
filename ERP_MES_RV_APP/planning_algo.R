library(dplyr)
library(lubridate)



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
plan <- function(plan_today, inventory, planif_data, days_forward, day_idx, max_per_time, prod_day, Commande, CommandeDetail, max_buffer_day){
  #Plan_today contains the row od the commande to plan
  #planif_data is the dataframe containing all already planned stuff
  list_of_times = c("08:00:00","10:00:00","12:00:00","14:00:00","16:00:00")
  counter_times = 1
  counter_days = 1
  #days_forward = plan_times
  #day_idx = 1
  #print(day_idx)
  if (nrow(plan_today) > 0){ #If we have something to plan for today
    for (i in 1:nrow(plan_today)){
      r = plan_today[i,]
      #print("Planning commande : ")
      #print(r)
      r["CommandeID"]
      panneaux_todo <- CommandeDetail %>% filter(CommandeID == r[["CommandeID"]], Statut == "TODO")
      #print(panneaux_todo)
  
      for (j in 1:nrow(panneaux_todo)){
        #Check that there is not already something planned on this day at that time when calling this again
        start_time = paste(days_forward[counter_days],list_of_times[counter_times],sep= " ")
        start_time
 
        current_work <- planif_data %>% filter(start == start_time, type == "range")
        current_work
        
        #print("Current work")
        #print(nrow(current_work))
        #Check if we need to change day
        
        while(nrow(current_work) == max_per_time){
          #Go to next spot
          counter_times = counter_times + 1
          
          
          if (counter_times > 4){
            counter_times = 1 #Restart the day
            counter_days = counter_days + 1
            day_idx = day_idx + 1
            inventory[day_idx] <-  inventory[day_idx-1]
          }
          
          if (counter_days > length(days_forward)){ #Do not go further than the horizon of days we want
            return(list(planif_data, inventory,Commande))
          }
            #else{
            #  counter_days = counter_days + 1
            #  day_idx = day_idx + 1 #move to next day
              #print(day_idx)
              #day_idx = 2
              
              #inventory[[day_idx-1]]
             # inventory[day_idx] <-  inventory[day_idx-1]
              #print("Updating next inventory day")
              #print(inventory[day_idx])
              #inventory[day_idx] <- list(days_forward[counter_days],inventory[[day_idx-1]]) #update the inventroy of next day to be same as previous day
            #}
          #}
          #print(counter_times)
          #print(counter_days)
          #print(day_idx)
          
          start_time = paste(days_forward[counter_days],list_of_times[counter_times],sep= " ")
          start_time
          
          current_work <- planif_data %>% filter(start == start_time, type == "range") #this also takes the commandes fournisseurs
          current_work
        }
        #print(counter_times)
        #print(counter_days)
        #print(day_idx)
        
        #Plan  the panneau
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
        #print(inventory)
        
        #Update status to En prod if this commande is done on prod day
        #print(Commande)
        #print(r[["CommandeID"]])
        status = Commande[Commande["CommandeID"] == r[["CommandeID"]], "Statut"]
        
        if (days_forward[[counter_days]] == prod_day){
          Commande[Commande["CommandeID"] == r[['CommandeID']],"Statut"] = "En Prod"
        }
        #Plan it if in the buffer period
        else if(days_forward[[counter_days]] <= max_buffer_day & status != "En Prod"){ #Planned if it is planned on some other day and not already en prod
          #print("PLanning")
          Commande[Commande["CommandeID"] == r[['CommandeID']],"Statut"] = "Planifiée"
        }
        
        
      } #This finishes all panneaux for one commande
      
      #Update the colle and vernis (and other) for each commande
      other_materials <- c("Colle","Vernis","Scie japonaise")
      for (i in 1:length(other_materials)){
        new_inv <- update_inventory_from_plan(inventory[[day_idx]], other_materials[[i]])
        inventory[[day_idx]] <- new_inv
      }
      
      #Emballer -- peut être fait juste après panneau done
      planif_data[nrow(planif_data)+1,] <- c(paste("Emballer commande",r[["CommandeID"]],sep=" "),paste(days_forward[counter_days],list_of_times[counter_times+1],sep= " "),NA,"À emballer","point")

      
    }#This finishes for all commandes we can start today
  }
  else{
    #day_idx = day_idx + 1 #move to next day
    
    #Copy inventory since it has not changed
    inventory[day_idx+1] <-  inventory[day_idx]
    #counter_days = counter_days + 1
  }
  return(list(planif_data, inventory, Commande))
}

update_inventory_from_plan <- function(inventory_day, panneau_ID, qtty_less = 1){
  #inv <- inventory[day_idx][[2]][1]
  #print(inventory_day)
  inv <- inventory_day %>% mutate(QuantiteDisponible = ifelse(ItemID == panneau_ID, QuantiteDisponible - qtty_less, QuantiteDisponible))
  #print(inv)
  return(inv)
}


#Update the inventory from receiving deliveries --- OK
update_inventory_from_deliveries <- function(fournisseurs_today, Item_pour_nom_fournisseur, inv_today, planif, day_today){
  #Adjust item values
  #print(day)
  for (i in 1:nrow(fournisseurs_today)){
    item = fournisseurs_today[i,'ItemID']
    #print(item)
    qtty = as.integer(fournisseurs_today[i,'Quantité'])
    #print(qtty)
    #print(inv_today)
    inv_today <- inv_today %>% mutate(QuantiteDisponible = ifelse(ItemID == item, QuantiteDisponible + qtty, QuantiteDisponible))
    #print(inv_today)
    #inv_today[inv_today['ItemID'] == item,"Qtte"] <- inv_today[inv_today['ItemID'] == item,"Qtte"] + qtty
    
    #Ajouter événement réception de commande
    day_to_use <- as.character(day_today)
    fournisseur_nom <- Item_pour_nom_fournisseur %>% filter(ItemID == item) %>% select(Fournisseur)
    new_row <- c(fournisseur_nom[[1]], paste(day_to_use,"08:00:00",sep=" "), NA, "Receptions fournisseurs", "point")
    planif[nrow(planif)+1,] <- new_row
  }
  
  #Add the commandes reception to planif
  #fournisseurs_names <- Item_pour_nom_fournisseur %>% filter(ItemID == item) %>% select(Fournisseur)
  #f <- unique(fournisseurs_names)
  #for (little_f in f){
    
  #  day_to_use <- as.character(day_today)
    #print(day_to_use)
  #  new_row <- c(little_f, paste(day_to_use,"08:00:00",sep=" "), NA, "Receptions fournisseurs", "point")
  #  planif[nrow(planif)+1,] <- new_row
  #}
  #print(planif)
  return(list(inv_today, planif))
  
}

#Add commande in fournisseur
add_fournisseur_commande <- function(commandesfournisseurs, to_buy, today_date, max_date){
  #Check if commande already there for this product and quantity
  #print(to_buy[["ItemID"]])
  next_commandes <- commandesfournisseurs %>% filter(ItemID == to_buy[["ItemID"]], Statut != "Reçue")
  
  #print(next_commandes)
  #Only add the commande if no commande for this product is waiting
  if (nrow(next_commandes) == 0){
    #Get fournisseur from database
    #fournisseur <- Item %>% filter(ItemID = itemID) %>% select(Fournisseur)
    livraison = as.character(as.Date(today_date)+3) #Read this from the file
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
      new_commande <- c(to_buy[["Fournisseur"]], livraison, to_buy[["ItemID"]] , to_buy[["MinStock"]], "En attente d'approbation")
      
      commandesfournisseurs <- rbind(commandesfournisseurs, new_commande)
    }
    
  }
  return(commandesfournisseurs)
}

#Get the data groups from the planif data
get_data_groups <- function(planif_data){
  groups <- unique(planif_data["group"])
  data_groups <- data.frame(c(groups,groups))
  colnames(data_groups) <- c("id","content")
  return(data_groups)
}

# ------------------------- Planification algo --------------------------------

MES_planif <- function(Commande, Inventaire, CommandesFournisseurs, CommandeDetail, Items, panneauDetail, today, max_range = 5, buffer = 3, nb_machines = 1){
  #Get dates to plan for
  #today = "2023-06-02" #Today date
  #max_range = 5 #Nb of days we want to plan for
  #buffer = 2 #Nb of days before we can look at the commande for planning
  #nb_machines = 1 #Nb of machines than can work at the same time
  
  plan_times <- get_dates_to_plan(today, max_range)
  plan_times
  
  #print(today, buffer)
  max_buffer_day <- get_dates_to_plan(today, buffer)[[buffer]]
  #print(max_buffer_day[[buffer]])
  
  #Initialize inventory list for each planning day
  inventory <- vector(mode="list",length=max_range) #first inventory is today's inventory
  inventory[[1]] <- Inventaire
  #print(inventory)
  
  #Get commandes eligible for planning (not Modifiable and ordered at max buffer days ago) -- order by closest DateLivraison
  #commandes_to_plan <- Commande %>% filter(Statut != "Modifiable", DateCreation <= as.Date(today) - buffer) %>% arrange(DateLivraison)
  commandes_to_plan <- Commande %>% filter(Statut != "Modifiable") %>% arrange(DateCommandeLivraison)
  
  #print(commandes_to_plan)
  
  planned_commandes <- c()
  
  #Initialize planif
  planif_data <- data.frame(matrix(ncol=5,nrow=0))
  colnames(planif_data) <- c("content","start","end","group","type")
  planif_data
  #print(planif_data)
  
  #Initialize commandes already planned
  planned_today <- data.frame()
  
  #Loop on each day one at a time
  for (j in 1:length(plan_times)){ 
    print(j)
    
    #Update next inventory if null
    if (is.null(inventory[[j]])){
      inventory[[j]] <- inventory[[j-1]]
    }
    #Get today's inventory
    inv_today <- inventory[[j]]
    
    
    #Get fournisseurs receptions for today if any
    fournisseurs_today = CommandesFournisseurs %>% filter(DateCommandeFReception == plan_times[[j]])#, Statut == "En attente") #mutate(DateReception = as.Date(DateReception)) #%>% filter("DateReception" == plan_times[[j]])
    #print(fournisseurs_today)
    
    if (nrow(fournisseurs_today != 0)){
      #Update the inventaire from fournisseur commande and change status of commande
      fournisseur_output <- update_inventory_from_deliveries(fournisseurs_today, Items, inv_today, planif_data, plan_times[[j]])
      #Update inventory
      inv_today <- fournisseur_output[[1]]
      #print(inv_today)
      inventory[[j]] <- inv_today
      #print(inventory)
  
      #Update planif with fournisseur data
      planif_data <- fournisseur_output[[2]]
      
      #Update status of fournisseur commande comme recue pour la première journée de la planif seulement
      CommandesFournisseurs <- CommandesFournisseurs %>% mutate(Statut = ifelse(DateCommandeFReception == plan_times[[1]],"Reçue",Statut))
      #print("Updated status of commande")
      #inv_today
      #print(CommandesFournisseurs)
      #planif_data
    }
    
    
    for (c in 1:nrow(commandes_to_plan)) {
      #print(commandes_to_plan)
      
      #Get the next commande
      commande <- commandes_to_plan[c,]
      #print(commande)
      
      #only plan the ones not yet planned -- TO ADJUST WITH BD
      if (!(commande[["CommandeID"]] %in% planned_commandes)){
        
        #Get the list of panneau for this commande
        #panneau_names <- c("P1","P2","P3")
        panneau_names <- unique(panneauDetail[["PanneauID"]])
        #print(panneau_names)
        #print(commande[["CommandeID"]])
        
        #AJUSTER ICI SELON LES DONNÉES
        #panneau_liste <- Commande %>% filter(CommandeID == commande[["CommandeID"]]) %>% select(Items)
        items = c(c("1"=2,"2":1,"3":1), c("1":2,"2":1,"3":2), c("1":1,"2":1,"3":1), c("1":1,"2":2,"3":2))
        panneau_liste <- items[[as.integer(commande[["CommandeID"]])]]
        #print(panneau_liste)
        all_good = TRUE
        
        #Check if we have all material to do the commande today
        for (p in 1:length(panneau_liste)){ #OR ROW TO CHECK
          if (panneau_liste[[panneau_names[p]]] > inv_today[inv_today["ItemID"]==panneau_names[p],"QuantiteDisponible"]){
            all_good = FALSE #Cannot start this project
          }
          
          #Account for projects half done :
          #Filter on status TODO
          #Suppose here that the number total of panneaux per commande is updated to the remaining number
          #When panneau is completed, do panneau category - 1
        }
        all_good
        
        #If we have all material, add to list of commandes to do
        if (all_good){
          #print("all good")
          #planned_today <- rbind(planned_today, commande)
          #print(planned_today)
          
          #Keep track of the commandes already planned
          planned_commandes <- append(planned_commandes, commande[["CommandeID"]])
          
          #Plan the commande
          #print("commande") #This is ok
          #print(commande)
          #print(planned_today) #Can this be empty?? if all en attent d'inventaire?
          plan_output <- plan(commande, inventory, planif_data, plan_times[j:length(plan_times)], j, nb_machines, today, Commande, CommandeDetail, max_buffer_day)
          
          planif_data <- plan_output[[1]]
          inventory <- plan_output[[2]]
          Commande <-  plan_output[[3]]
          
          #print("Planif data :")
          #print(planif_data)
          
          #print("inventory")
          #print(inventory)
          
          
        }
        else{
          #print(commande)
          #print(inventory[j])
          Commande[Commande["CommandeID"] == commande[["CommandeID"]],"Statut"] = "Attente d'inventaire"
          
        }
      
      }
      #Update inventory for the next commande
      inv_today <- inventory[[j]]
    
      
    } 
    
    #Plan according to the list of planned_today -- this also updates the inventory and commande status
    #print(inventory)
    #print(planif_data)
 #  print(planned_today) #Can this be empty?? if all en attent d'inventaire?
#    plan_output <- plan(planned_today, inventory, planif_data, plan_times[j:length(plan_times)], j, nb_machines, today, Commande, CommandeDetail)
    #print(planif_data)
    
#    planif_data <- plan_output[[1]]
#    inventory <- plan_output[[2]]
#    Commande <-  plan_output[[3]]
    
#    planif_data
 #   inventory
    
    #Demander commande fournisseur si en bas des stocks
    inv_of_the_day <- inventory[j][[1]]
    inv_of_the_day
    
    if (!is.null(inv_of_the_day)){
      #print(inv_of_the_day)
     
      new_inv_of_the_day <- merge(inv_of_the_day, Items, by.x = "ItemID", by.y = "ItemID")
      #print(new_inv_of_the_day)
      to_buy <- new_inv_of_the_day %>% filter(QuantiteDisponible < MinStock)
      to_buy
      
      if (nrow(to_buy) > 0){
        for (e in 1:nrow(to_buy)){
          CommandesFournisseurs <- add_fournisseur_commande(CommandesFournisseurs, to_buy[e,], plan_times[[j]], plan_times[[length(plan_times)]])
        }
      }

    }
    
    
  }
  #print("Out of the for loop")
  #Update true inventory from planif of today
  Inventory <- inventory[1]
  inventory
  
  print(planif_data)
  print(Inventory)
  print(CommandesFournisseurs)
  print(Commande)
  data_groups <- get_data_groups(planif_data)
  print(data_groups)

  return(list(Commande, Inventory, CommandesFournisseurs, planif_data, data_groups))
}
  
# ------------------------- Running this ---------------------


#today = "2023-06-01"
#MES_output <- MES_planif(Commande, Inventaire, CommandesFournisseurs, CommandeDetail, today, max_range = 5, buffer = 2, nb_machines = 2)

#Commande <- MES_output[[1]]
#Inventaire <- MES_output[[2]][[1]]
#CommandesFournisseurs <- MES_output[[3]]
#planif_data <- MES_output[[4]]
#data_groups <- MES_output[[5]]




