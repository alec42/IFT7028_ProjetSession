library(googledrive)
library(rjson)
library(dplyr)
library(googlesheets4)

#' @example GDriveJSONUpdate(
#'  dossier_racine = "Industrie_VR_IFT7028/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
#'  customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes'))
#'  
GDriveJSONUpdate <- function(
    dossier_racine = "Industrie_VR_IFT7028/", 
    dossier_commandee = "commandes_json/commandée/",
    dossier_importee = "commandes_json/importée/",
    dossier_3d = "commandes_3d/",
    customerOrders = read_sheet(link_gs_erp, sheet = 'Commandes'),
    customers = read_sheet(link_gs_erp, sheet = 'Clients')){
  
  # Fonction utilitaire, opposée de %in%
  `%notin%` <- Negate(`%in%`)
  
  # Liste des fichiers dans le dossier commandée
  JSON_commandee_list <- googledrive::drive_ls(paste0(dossier_racine, dossier_commandee))
  
  # Vérifie si le path vers commandée existe
  if (googledrive::is_folder(googledrive::as_dribble(paste0(dossier_racine, dossier_commandee))) != TRUE) {
    stop(paste0("Le dossier", dossier_racine, dossier_commandee, "n'existe pas"))
  } 
  
  # Vérifie si le path vers importée existe
  if (googledrive::is_folder(googledrive::as_dribble(paste0(dossier_racine, dossier_importee))) != TRUE) {
    stop(paste0("Le dossier", dossier_racine, dossier_importee, "n'existe pas"))
  } 
  
  # Vérifie s'il y a de nouvelles commandes
  if (length(JSON_commandee_list) == 0) {
    message(paste0("Le dossier", dossier_racine, dossier_commandee, "n'a pas de nouvelles commandes."))
    return()
  }
  
  # Vérifie si le path existe vers commande 3d existe
  if (googledrive::is_folder(googledrive::as_dribble(paste0(dossier_racine, dossier_3d))) != TRUE) {
    stop(paste0("Le dossier", dossier_racine, dossier_3d, "n'existe pas"))
  } 
  
  # Itération sur tous les JSON
  for (JSON_file in JSON_commandee_list$name) {
    
    # Chargement du JSON
    json_data <- rjson::fromJSON(drive_read_string(JSON_file, encoding = "utf-8"))
    
    # Création du data frame client
    json_client <- as.tibble(json_data[c("ClientID", "Nom", "Prenom", "Adresse", "Courriel", "Mot_de_passe")])
    json_client$Commandes <- "[]"
    json_client <- json_client |> mutate(across(c("Mot_de_passe"), as.list))
    
    # Création du data frame commandes
    json_commandes <- as.tibble(json_data[c("ClientID", "CommandeID", "FichiersFabrication", "Prix", "Statut", "DateCommandeCreation", "DateCommandeModification", "DateCommandeLivraison")])
    json_commandes$Items <- "{4:1; 5:2}"
    json_commandes$InformationsCommande <- toJSON(json_data[-(1:(which(names(json_data) == "overallDims"))-1)])
    json_commandes$FichierAssemblage <- "-"
    json_commandes$FichiersFabrication <- paste(dossier_racine,dossier_3d,json_data$CommandeID, sep="")
    print(json_commandes)
    json_commandes <- json_commandes |> mutate(across(starts_with("Date"), as.Date), across(c("Prix"), as.double))
    #json_commandes <- json_commandes |> mutate(across(starts_with("Date"), as.POSIXct), across(c("Prix"), as.double))
    
    # Ajustement de customerOrders
    print(customerOrders)
    customerOrders <- customerOrders |> mutate(across(starts_with("Date"), as.Date))
    #customerOrders <- customerOrders |> mutate(across(starts_with("Date"), as.POSIXct))
    
    # Si le client n'existe pas, ajoute la ligne
    if (json_client$ClientID %notin% customers$ClientID) {
      customers <- customers |> rows_append(json_client)
    }
    
    # Vérifie si la commande existe déjà dans la BD et écrase la ligne
    if (json_data$CommandeID %in% customerOrders$CommandeID) {
      customerOrders[customerOrders$CommandeID == json_data$CommandeID, ] <- json_commandes
    } 
    
    # Si la ligne n'existe pas, ajoutée à la table
    if (json_data$CommandeID %notin% customerOrders$CommandeID) {
      customerOrders <- customerOrders |> rows_append(json_commandes)
    }
    
    # Mise à jour de la liste des commandes du client
    commandesID <- customerOrders$CommandeID[customerOrders$ClientID == json_client$ClientID]
    customers <- customers |> rows_update(
      by = "ClientID",
      as_tibble(json_client) %>%
        mutate(
          across(c("Mot_de_passe"), as.list),
          Commandes = paste("[", paste(commandesID, collapse = ";"), "]", sep = "")))
    
    # Bouger le fichier dans importée
    googledrive::drive_mv(file = JSON_file, path = paste0(dossier_racine, dossier_importee))
  }
  
  customerOrders <- customerOrders |> mutate(across(starts_with("Date"), function(x) ifelse(is.na(x), "", paste(as.Date(x), "00:00:00"))))
  print(customerOrders)
  
  return(list(customerOrders, customers))
}


# GDriveUpdatePiecesDetail <- function(
#   piecesDetail = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'PiecesDetail'),
#   panneauxDetail = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'PanneauxDetail'),
#   items = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Items'),
#   dossier_racine = "Industrie_VR_IFT7028/Base_de_donnees_ERP/",dossier_importee = "commandes_json/importée/",
#   commandeID
# ) {
#   # JSONpath <- paste(dossier_racine, dossier_importee, commandeID, sep="")
#   startingPanneauID <- max(piecesDetail$PanneauID) + 1
#   startingPieceID <- max(piecesDetail$PieceID) + 1
# 
#   ####### fonction OPTIMISATION Equipe 3 #########
#   # GetPieceDetailDataFrame(paste0(dossier_racine, commandeID), startingPanneauID, startingPieceID)
#   # return un dataframe piecesDetail complété pour LA commande et List[(id_panneau, FichierDecoupe)]
# 
#   # le dataframe retourné par la fonction d'optimisation
#   piecesCommandeReturned <- piecesDetail
#   # liste des chemins de fichier de decoupe retournée par la fonction
#   pathDecoupe <- data.frame(PanneauID = c(22, 21, 20), FichierDecoupe = c("-", "-", "-"))
# 
#   nb_panneau <- n_distinct(piecesCommandeReturned$PanneauID)
# 
#   # get random panneau type
#   type_panneau <- length(items$ItemID[items$Type == "Panneau"])
# 
#   newPanneauxDetail <- piecesCommandeReturned %>% mutate(PieceID= NULL, Fichier3D = NULL, PanneauType=NULL)
# 
#   newPanneauxDetail <- distinct(newPanneauxDetail, PanneauID, .keep_all = TRUE)
# 
#   newPanneauxDetail <- newPanneauxDetail %>% mutate(PanneauType = sample(1:type_panneau, nb_panneau, replace = TRUE), Statut= "TODO", DatePrevue = "", DateFabrication="")
# 
#   newPanneauxDetail <- left_join(newPanneauxDetail, pathDecoupe)
# 
#   panneauxDetail <- panneauxDetail |> rows_append(
#         as_tibble(newPanneauxDetail) %>% mutate(across(starts_with("Date"), lubridate::ymd_hms)))
# 
#   piecesCommandeReturned$PanneauType[piecesCommandeReturned$PanneauID == newPanneauxDetail$PanneauID] <- 60
# 
#   print(piecesCommandeReturned)
# 
#   piecesDetail <- piecesDetail |> rows_append(
#         as_tibble(piecesCommandeReturned))
# }

#GDriveJSONUpdate()
# GDriveUpdatePiecesDetail(commandeID=1)
