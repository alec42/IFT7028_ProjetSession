library(googledrive)
library(rjson)
library(dplyr)
library(googlesheets4)

#' @example GDriveJSONUpdate(
#'  dossier_racine = "Industrie_VR_IFT7028/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
#'  customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes'))
GDriveJSONUpdate <- function(
    dossier_racine = "Industrie_VR_IFT7028/Base_de_donnees_ERP/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
    customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes'),
    customers = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Clients')
) {
  JSON_commandee_list <- googledrive::drive_ls(paste0(dossier_racine, dossier_commandee))

  if (googledrive::is_folder(googledrive::as_dribble(paste0(dossier_racine, dossier_commandee))) != TRUE) {
    stop(paste0("Le dossier", dossier_racine, dossier_commandee, "n'existe pas"))
  } else if (length(JSON_commandee_list) == 0) {
    message(paste0("Le dossier", dossier_racine, dossier_commandee, "n'a pas de nouvelles commandes."))
    return()
  }
  if (googledrive::is_folder(googledrive::as_dribble(paste0(dossier_racine, dossier_importee))) != TRUE)
    stop(paste0("Le dossier", dossier_racine, dossier_importee, "n'existe pas"))

  customerOrders <- customerOrders |>
    mutate(across("DateCommandeModification", lubridate::ymd_hms))

  for (JSON_file in JSON_commandee_list$name) {
    json_data <- rjson::fromJSON(drive_read_string(JSON_file, encoding = "utf-8")) # temp fix to get only first in json file

    json_data[c("FichierFabrication")] <- paste(dossier_racine,dossier_importee,JSON_file, sep="")
    json_data[c("InformationsCommande")] <- rjson::toJSON(json_data[c("overallDims", "leftSideOptions", "rightSideOptions")])
    json_customer <- json_data[c("ClientID", "Nom", "Prenom", "Adresse", "Courriel", "Mot_de_passe")]
    json_data[c("Nom", "Prenom", "Adresse", "Courriel", "Mot_de_passe", "overallDims", "leftSideOptions", "rightSideOptions")] <- NULL
    # Vérifie si la commande existe déjà dans la BD
    if (any(customerOrders$CommandeID == json_data$CommandeID)) {
      # if (json_data$Statut %in% c("Commandée", "Complétée", "Modifiable")) {
        customerOrders <- customerOrders |> rows_update(
          by = "ClientID",
          as_tibble(json_data) %>%
            mutate(
              across(c("ClientID", "CommandeID", "Prix"), as.double),
              across(starts_with("Date"), lubridate::ymd_hms),
              DateCommandeModification=lubridate::ymd_hms(Sys.time()),
              Items = NULL))
        # AJOUTER LES INFO DANS COMMANDE DETAILS !!!!!!
      # }
    } else {
      customerOrders <- customerOrders |> rows_append(
        as_tibble(json_data) %>%
          mutate(
            across(c("ClientID", "CommandeID", "Prix"), as.double),
            across(starts_with("Date"), ~lubridate::ymd_hms(Sys.time())),
            DateCommandeLivraison=NULL, Items=NULL, Prix=NULL))

        # Ajoute le client dans la BD si il n'existe pas
        if (!(any(customers$ClientID == json_data$ClientID))) {
            customers <- customers |> rows_append(
            as_tibble(json_customer) %>%
            mutate(
            across(c("ClientID"), as.double),
            across(c("Mot_de_passe"), as.list),
            Commandes = NULL))
        }

        commandesID <- customerOrders$CommandeID[customerOrders$ClientID == json_customer$ClientID]

        customers <- customers |> rows_update(
          by = "ClientID",
          as_tibble(json_customer) %>%
            mutate(
              across(c("Mot_de_passe"), as.list),
              Commandes = paste("[", paste(commandesID, collapse = ";"), "]", sep = "")))
    }

    print(customerOrders)
    #googledrive::drive_mv(
    #  file = paste0(dossier_racine, dossier_commandee, JSON_file),
    #  path = paste0(dossier_racine, dossier_importee, JSON_file),
    #  overwrite = TRUE)
  }
  return(customerOrders)
}


GDriveUpdatePiecesDetail <- function(
  piecesDetail = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'PiecesDetail'),
  panneauxDetail = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'PanneauxDetail'),
  items = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Items'),
  dossier_racine = "Industrie_VR_IFT7028/Base_de_donnees_ERP/",dossier_importee = "commandes_json/importée/",
  commandeID
) {
  # JSONpath <- paste(dossier_racine, dossier_importee, commandeID, sep="")
  startingPanneauID <- max(piecesDetail$PanneauID) + 1
  startingPieceID <- max(piecesDetail$PieceID) + 1

  ####### fonction OPTIMISATION Equipe 3 #########
  # GetPieceDetailDataFrame(commandeID, startingPanneauID, startingPieceID)
  # return un dataframe piecesDetail complété pour LA commande et List[(id_panneau, FichierDecoupe)]

  # le dataframe retourné par la fonction d'optimisation
  piecesCommandeReturned <- piecesDetail
  # liste des chemins de fichier de decoupe retournée par la fonction
  pathDecoupe <- data.frame(PanneauID = c(22, 21, 20), FichierDecoupe = c("-", "-", "-"))

  nb_panneau <- n_distinct(piecesCommandeReturned$PanneauID)

  # get random panneau type
  type_panneau <- length(items$ItemID[items$Type == "Panneau"])

  newPanneauxDetail <- piecesCommandeReturned %>% mutate(PieceID= NULL, Fichier3D = NULL, PanneauType=NULL)

  newPanneauxDetail <- distinct(newPanneauxDetail, PanneauID, .keep_all = TRUE)

  newPanneauxDetail <- newPanneauxDetail %>% mutate(PanneauType = sample(1:type_panneau, nb_panneau, replace = TRUE), Statut= "TODO", DatePrevue = "", DateFabrication="")

  newPanneauxDetail <- left_join(newPanneauxDetail, pathDecoupe)

  panneauxDetail <- panneauxDetail |> rows_append(
        as_tibble(newPanneauxDetail) %>% mutate(across(starts_with("Date"), lubridate::ymd_hms)))

  piecesCommandeReturned$PanneauType[piecesCommandeReturned$PanneauID == newPanneauxDetail$PanneauID] <- 60

  print(piecesCommandeReturned)

  piecesDetail <- piecesDetail |> rows_append(
        as_tibble(piecesCommandeReturned))
}

#GDriveJSONUpdate()
# GDriveUpdatePiecesDetail(commandeID=1)