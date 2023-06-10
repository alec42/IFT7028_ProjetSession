library(dplyr)
library(lubridate)


#------------------- Utility functions for planification algo ----------------------------
#Get dates
get_dates_to_plan <- function(today, max_range){
  plan_times = c()
  counter = 0
  while (length(plan_times) != max_range){
    next_day <- (as.Date(today)+counter)

    if (wday(next_day) != 1 && wday(next_day) != 7){ #Sunday = 1, Saturday = 7, skip them
      plan_times <- append(plan_times, next_day)
    }
    counter = counter + 1
  }
  plan_times
}

#Planning from list of commandes we can start today + update inventory
plan <- function(DisposUsine, DisposEmployers, plan_today, inventory, planif_data, days_forward, day_idx, max_per_time, prod_day, Commande, PanneauDetail, max_buffer_day, Items){
  #Plan_today contains the row of the command to plan
  #planif_data is the dataframe containing all already planned stuff
  list_of_times <- sprintf("%02d:00:00", seq(8, 23))
  counter_times = 1
  counter_days = 1

  #If we have something to plan for today
  if (nrow(plan_today) > 0){
    #Loop on each panneau
    for (i in seq(nrow(plan_today))){
      r = plan_today[i,]

      panneaux_todo <- PanneauDetail %>%
        filter(CommandeID == r[["CommandeID"]], Statut == "TODO")

      if (nrow(panneaux_todo) > 0) {
        for (j in seq(nrow(panneaux_todo))) {
          #Get start and end time for day we are at
          min_max_times <- DisposUsine %>% select(weekdays(as.Date(days_forward[counter_days])))

          #Get fabrication time for this panneau
          fab_time <- Items |>
            filter(ItemID == pull(panneaux_todo[j,], PanneauType)) |>
            pull(TempsFabrication)

          #Check that there is not already something planned on this day at that time when calling this again
          start_time <- paste(days_forward[counter_days], list_of_times[counter_times], sep = " ")

          current_work <- planif_data %>%
            filter(start == start_time, type == "range")

          #Check if employee available today
          day_of_week <- weekdays(as.Date(days_forward[counter_days]))

          nb_available <- DisposEmployers %>% pull(day_of_week)
          if (sum(nb_available) >= 1){
            is_available <- TRUE
          } else {
            is_available <- FALSE
          }

          #Check if we need to change day
          while(
            nrow(current_work) == max_per_time |
            !is_available |
            list_of_times[counter_times] >= min_max_times[2, ]
          ) {
            #Go to next spot
            counter_times = counter_times + fab_time

            #Restart the day
            if(list_of_times[counter_times] >= min_max_times[2, ]) {
              counter_times <- 1
              counter_days <- counter_days + 1
              day_idx <- day_idx + 1
              inventory[day_idx] <- inventory[day_idx-1]
            }

            #Do not go further than the horizon of days we want
            if (counter_days > length(days_forward)){
              return(list(planif_data, inventory,Commande, PanneauDetail))
            }

            #Update start time and current work
            start_time = paste(days_forward[counter_days],list_of_times[counter_times],sep= " ")

            #updaye working or not
            current_work <- planif_data %>% filter(start == start_time, type == "range") #this also takes the commandes fournisseurs

            #Ipdate people available or not
            day_of_week = weekdays(as.Date(days_forward[counter_days]))
            nb_available <- DisposEmployers %>% pull(day_of_week)

            if (sum(nb_available) >= 1){
              is_available = TRUE
            }
            else{
              is_available = FALSE
            }
            min_max_times <- DisposUsine %>% select(weekdays(as.Date(days_forward[counter_days])))
          }#Done getting the next available spot

          #Plan  the panneau -- CommandeID-PanneauType-PanneauID
          planif_data <- planif_data |>
            add_row(
              content=paste0(pull(panneaux_todo[j,], PanneauID)),
              start=start_time, end=paste(days_forward[counter_days], list_of_times[counter_times+fab_time]),
              FichierDecoupe=paste0(pull(panneaux_todo[j,], FichierDecoupe)), group=paste("Commande", r[["CommandeID"]]), type="range"
            )

          new_inv <- update_inventory_from_plan(inventory[[day_idx]], pull(panneaux_todo[j,], PanneauType))

          inventory[[day_idx]] <- new_inv

          #Update date of fabrication
          PanneauDetail <- PanneauDetail %>% mutate(
            DatePrevue = ifelse(
              CommandeID == r[["CommandeID"]] &&
                PanneauID == pull(panneaux_todo[j,], PanneauID) &&
                PanneauType == pull(panneaux_todo[j,], PanneauType),
              paste(days_forward[counter_days]),
              DatePrevue))

          #Update status to En prod if this commande is done on prod day
          status <- pull(Commande[Commande["CommandeID"] == r[["CommandeID"]],], Statut)
          if (days_forward[[counter_days]] == prod_day){
            Commande[Commande["CommandeID"] == r[['CommandeID']],"Statut"] = "En production"
          }
          #Plan it if in the buffer period
          else if(days_forward[[counter_days]] <= max_buffer_day & status != "En Prod"){ #Planned if it is planned on some other day and not already en prod
            Commande[Commande["CommandeID"] == r[['CommandeID']],"Statut"] = "En Planifiée"
          }
        } #This finishes all panneaux for one commande
      }
      #Update the colle and vernis (and other) for each commande
      other_materials <- c(4,5,6,7)
      for (i in seq(length(other_materials))){
        new_inv <- update_inventory_from_plan(inventory[[day_idx]], other_materials[[i]])
        inventory[[day_idx]] <- new_inv
      }

      #Emballer
      planif_data <- planif_data |>
        add_row(
          content=paste("Emballer commande", r[["CommandeID"]], sep=" "),
          start=paste(days_forward[counter_days],list_of_times[counter_times+fab_time],sep= " "), end=NA,
          FichierDecoupe=NA, group="À emballer", type="point"
        )
      # planif_data[nrow(planif_data)+1,] <- c(paste("Emballer commande",r[["CommandeID"]],sep=" "),paste(days_forward[counter_days],list_of_times[counter_times+fab_time],sep= " "),NA,NA,"À emballer","point")


    }#This finishes for all commandes we can start today
  }
  else{

    #Copy inventory since it has not changed
    inventory[day_idx+1] <-  inventory[day_idx]
  }
  return(list(planif_data, inventory, Commande, PanneauDetail))
}

update_inventory_from_plan <- function(inventory_day, panneau_ID, qtty_less = 1){
  inv <- inventory_day %>% mutate(QuantiteDisponible = ifelse(ItemID == panneau_ID, QuantiteDisponible - qtty_less, QuantiteDisponible))
  return(inv)
}


#Update the inventory from receiving deliveries
update_inventory_from_deliveries <- function(fournisseurs_today, Item_pour_nom_fournisseur, inv_today, planif, day_today){
  #Adjust item values
  for (i in seq(nrow(fournisseurs_today))){
    item <- fournisseurs_today[i,] |> pull(ItemID)
    qtty <- as.integer(fournisseurs_today[i,'Quantité'])
    inv_today <- inv_today %>% mutate(QuantiteDisponible = ifelse(ItemID == item, QuantiteDisponible + qtty, QuantiteDisponible))

    #Ajouter événement réception de commande
    day_to_use <- as.character(day_today)
    fournisseur_nom <- Item_pour_nom_fournisseur %>% filter(ItemID == item) %>% pull(Fournisseur)
    planif <- planif |>
      add_row(
        content=paste(fournisseur_nom[[1]], "- id",as.integer(fournisseurs_today[i,'CommandeFournisseurID'])),
        start=paste(day_to_use,"08:00:00"), end=NA,
        FichierDecoupe=NA, group="Receptions fournisseurs", type="point"
      )
  }

  return(list(inv_today, planif))

}

#Add commande in fournisseur
add_fournisseur_commande <- function(commandesfournisseurs, to_buy, today_date, max_date){
  #Check if commande already there for this product and quantity
  next_commandes <- commandesfournisseurs %>% filter(ItemID == to_buy[["ItemID"]], Statut != "Reçue")

  #Only add the commande if no commande for this product is waiting
  if (nrow(next_commandes) == 0){
    #Get delai livraison from database Item
    delai_livraison <- to_buy[["DelaisLivraison"]]
    livraison = as.character(as.Date(today_date) + delai_livraison)

    #Get day of reception de livraison
    if (wday(livraison) == 1){ #Sunday
      livraison = as.character(as.Date(livraison) + 1)
    }
    if (wday(livraison) == 7){ #Saturday
      livraison = as.character(as.Date(livraison) + 2)
    }

    #Only plan within the planning window
    if (as.Date(livraison) <= as.Date(max_date)){
      commandesfournisseurs <- commandesfournisseurs |>
        add_row(
          CommandeFournisseurID=max(as.integer(commandesfournisseurs[["CommandeFournisseurID"]]))+1,
          Statut="En attente d'approbation",
          DateCommandeFCreation=as.POSIXct(today_date), DateCommandeFReception=as.POSIXct(livraison),
          ItemID=to_buy[["ItemID"]], Quantité=to_buy[["MinStock"]]
        )
    }

  }
  return(commandesfournisseurs)
}

#Get the data groups from the planif data
get_data_groups <- function(planif_data){
  groups <- unique(planif_data$group)
  data_groups <- tibble(id=groups, content=groups)
  return(data_groups)
}

# ------------------------- Planification algo --------------------------------

MES_planif <- function(Commande, Inventaire, CommandesFournisseurs, PanneauDetail, Items, DisposUsine, DisposEmployers, today, max_range = 5, buffer = 3, nb_machines = 1){
  #Get dates to plan for
  plan_times <- get_dates_to_plan(today, max_range)

  max_buffer_day <- get_dates_to_plan(today, buffer)[[buffer]]

  #Initialize inventory list for each planning day
  inventory <- vector(mode="list",length=max_range) #first inventory is today's inventory
  inventory[[1]] <- Inventaire

  #Get commandes eligible for planning (not Modifiable and ordered at max buffer days ago) -- order by closest DateLivraison
  commandes_to_plan <- Commande %>% filter(Statut != "Modifiable") %>% arrange(DateCommandeLivraison)
  #Track the commandes ID already planned
  planned_commandes <- c()

  #Initialize planif
  planif_data <- tibble(
    content = character(),
    start = character(),
    end = character(),
    FichierDecoupe = character(),
    group = character(),
    type = character()
  )

  #Initialize commandes already planned
  planned_today <- data.frame()

  #Loop on each day one at a time
  for (j in seq(length(plan_times))){
    #Update next inventory if null
    if (is.null(inventory[[j]])){
      inventory[[j]] <- inventory[[j-1]]
    }
    #Get today's inventory
    inv_today <- inventory[[j]]


    #Get fournisseurs receptions for today if any
    fournisseurs_today = CommandesFournisseurs %>% filter(as.Date(DateCommandeFReception) == plan_times[[j]])#, Statut == "En attente") #mutate(DateReception = as.Date(DateReception)) #%>% filter("DateReception" == plan_times[[j]])

    if (nrow(fournisseurs_today != 0)){
      #Update the inventaire from fournisseur commande and change status of commande
      fournisseur_output <- update_inventory_from_deliveries(fournisseurs_today, Items, inv_today, planif_data, plan_times[[j]])
      #Update inventory
      inv_today <- fournisseur_output[[1]]
      inventory[[j]] <- inv_today

      #Update planif with fournisseur data
      planif_data <- fournisseur_output[[2]]

      #Update status of fournisseur commande comme recue pour la première journée de la planif seulement
      #CommandesFournisseurs <- CommandesFournisseurs %>% mutate(Statut = ifelse(DateCommandeFReception == plan_times[[1]],"Reçue",Statut))

    }

    #Get the list of panneau for this commande
    panneau_names <- Items |> filter(Type=="Panneau") |> pull(ItemID)

    for (c in seq(nrow(commandes_to_plan))) {

      #Get the next commande
      commande <- commandes_to_plan[c,]

      #only plan the ones not yet planned
      if (!(commande[["CommandeID"]] %in% planned_commandes)){

        # Extraire les panneaux de la colonne items et convertir en liste
#        # !!!!! AJUSTER ICI SELON LES DONNÉES !!!!!!!
        #panneau_liste doit contenir la quantité totale de chaque type de panneau nécessaire pour fabriquer cette commande
        #panneau_liste <- Commande %>% filter(CommandeID == commande[["CommandeID"]]) %>% select(Items)
        # items = list(c(2,1,1), c(2,1,2), c(1,1,1), c(1,2,2))
        # items
        # panneau_liste <- items[[as.integer(commande[["CommandeID"]])]]
        panneau_liste <- Commande |>
          filter(CommandeID==commande[["CommandeID"]]) |> # filter order
          mutate(Items = str_extract_all(Items, "\\d+:\\d+")) |> unnest(Items) |> # separate items dictionnary
          separate(Items, into = c("ItemID", "Quantity"), sep = ":") |> mutate(across(c(ItemID, Quantity), as.integer)) |> # split into separate columns & convert to int
          select(ItemID, Quantity) |> filter(ItemID %in% panneau_names) |> # only keep panneaux, not other items
          tidyr::complete(ItemID = seq(1, 3), fill = list(Quantity = 0)) |> # fill in if some panneaux aren't present
          group_by() |> summarize(List = list(Quantity)) |> pull(List) |> unlist() # convert into list

        # Jusqu'ici -------

        #Can plan today
        all_good = TRUE

        #Check if we have all material to do the commande today
        for (p in seq(length(panneau_liste))){
          ## AJUSTER les indices ici si le format ne fit pas.
          if (panneau_liste[[panneau_names[p]]] > pull(filter(inv_today, ItemID==panneau_names[p]), QuantiteDisponible)){
            all_good = FALSE #Cannot start this project
          }
        }

        #If we have all material, add to list of commandes to do
        if (all_good){

          #Keep track of the commandes already planned
          planned_commandes <- append(planned_commandes, commande[["CommandeID"]])

          #Plan the commande
          plan_output <- plan(DisposUsine, DisposEmployers, commande, inventory, planif_data, plan_times[j:length(plan_times)], j, nb_machines, today, Commande, PanneauDetail, max_buffer_day, Items)
          # plan_today = commande;days_forward = plan_times[j:length(plan_times)]; day_idx = j; max_per_time = nb_machines; prod_day = today
          planif_data <- plan_output[[1]]
          inventory <- plan_output[[2]]
          Commande <-  plan_output[[3]]
          PanneauDetail <- plan_output[[4]]
        } else{
          #Note enough inventory to do commande today
          Commande[Commande["CommandeID"] == commande[["CommandeID"]],"Statut"] = "Attente d'inventaire"

        }

      }
      #Update inventory for the next commande
      inv_today <- inventory[[j]]

    }

    #Demander commande fournisseur si en bas des stocks
    inv_of_the_day <- inventory[j][[1]]
    inv_of_the_day

    if (!is.null(inv_of_the_day)){

      new_inv_of_the_day <- merge(inv_of_the_day, Items, by.x = "ItemID", by.y = "ItemID")
      to_buy <- new_inv_of_the_day %>% filter(QuantiteDisponible < MinStock)

      if (nrow(to_buy) > 0){
        for (e in seq(nrow(to_buy))){
          CommandesFournisseurs <- add_fournisseur_commande(CommandesFournisseurs, to_buy[e,], plan_times[[j]], plan_times[[length(plan_times)]])
        }
      }

    }


  }
  #Update true inventory from planif of today
  Inventory <- inventory[1]

  #Print the planif and new commandes, etc
  #print(planif_data)
  #print(Inventory)
  #print(CommandesFournisseurs)
  #print(Commande)
  data_groups <- get_data_groups(planif_data)
  #print(data_groups)
  #print(PanneauDetail)

  return(list(Commande, CommandesFournisseurs, PanneauDetail, planif_data, data_groups))
}