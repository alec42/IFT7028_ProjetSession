library(googledrive)
library(rjson)
library(dplyr)
library(googlesheets4)
library(reticulate)
library(tidyverse)


reticulate::source_python("Production/ErpCommunication.py")


# Fonction temporaire : Génération de pièces aléatoires
# generateRandomPiece <- function(CommandeID, PanneauStart, PieceStart){
#   tibble(CommandeID = rep(CommandeID, 4),
#          PanneauID = c(1, 2, 3, 4) + PanneauStart,
#          PieceID = c(1, 2, 3, 4) + PieceStart,
#          PanneauType = round(runif(4, 1, 3)),
#          Fichier3D = c("-", "-", "-", "-"))
# }

# Fonction : load JSON
GDriveJSONUpdate <- function(
    dossier_racine = "Industrie_VR_IFT7028/",
    dossier_commandee = "commandes_json/commandée/",
    dossier_importee = "commandes_json/importée/",
    dossier_3d = "commandes_3d/",
    customerOrders = read_sheet(link_gs_erp, sheet = 'Commandes'),
    customers = read_sheet(link_gs_erp, sheet = 'Clients'),
    pieces = read_sheet(link_gs_erp, sheet = 'PiecesDetail'),
    panneaux = read_sheet(link_gs_erp, sheet = 'PanneauxDetail')){

  # Fonction utilitaire, inverse de %in%
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
    json_client <- json_client |> mutate(across(c("ClientID"), as.double))

    # Création du data frame commandes
    json_commandes <- as.tibble(json_data[c("ClientID", "CommandeID")]) |>
      add_column(FichiersFabrication = "-") |>
      add_column(as.tibble(json_data[c("Prix", "Statut", "DateCommandeCreation", "DateCommandeModification", "DateCommandeLivraison")]))
    json_commandes$Items <- "-"
    json_commandes$InformationsCommande <- toJSON(json_data[-(1:(which(names(json_data) == "overallDims"))-1)])
    json_commandes$FichierAssemblage <- "-"
    json_commandes$FichiersFabrication <- paste(dossier_racine,dossier_3d,json_data$CommandeID, sep="")
    json_commandes <- json_commandes |> mutate(across(starts_with("Date"), as.Date), across(c("Prix"), as.double), across(c("ClientID"), as.double))

    # Ajustement de customerOrders et customers
    customerOrders <- customerOrders |> mutate(across(starts_with("Date"), as.Date))
    customers <- customers |> mutate(across(c("Mot_de_passe"), unlist))

    # Si le client n'existe pas, ajoute la ligne
    if (json_client$ClientID %notin% customers$ClientID) {
      customers <- customers |> rows_append(json_client)
    }

    # Vérifie si la commande existe déjà dans la BD et écrase la ligne
    # Supprime les pièces et les panneaux si la commande est rechargée
    if (json_data$CommandeID %in% customerOrders$CommandeID) {
      customerOrders[customerOrders$CommandeID == json_data$CommandeID, ] <- json_commandes

      pieces <- pieces |> filter(CommandeID != json_data$CommandeID)

      panneaux <- panneaux |> filter(CommandeID != json_data$CommandeID)
    }

    # Si la ligne n'existe pas, ajoutée à la table
    if (json_data$CommandeID %notin% customerOrders$CommandeID) {
      customerOrders <- customerOrders |> rows_append(json_commandes)
    }

    ################################################################################
    #### Générer pièces aléatoires (À remplacer par le vrai code de l'équipe 3) ####
    ################################################################################
    # randomPieces <- generateRandomPiece(json_data$CommandeID, max(pieces$PanneauID), max(pieces$PieceID))
    PiecesDetail_Gen_1 <- GetProductionInformation(currentOrderId = as.character(json_data$CommandeID), firstPannelId = max(pieces$PanneauID), firstPieceId = max(pieces$PieceID))
    PiecesDetail_Gen <- PiecesDetail_Gen_1[[1]] |> as_tibble() |> mutate(across(c(PanneauID, PieceID, PanneauType), unlist)) |> mutate(CommandeID = as.numeric(CommandeID)) |>
      filter(!str_detect(FichierDecoupe, "truss"))
    PiecesDetail_Gen_f <- PiecesDetail_Gen |> select(CommandeID, PanneauID, PanneauType, Fichier3D) |> distinct() |> add_column(PieceID = max(pieces$PieceID)+seq(1, PiecesDetail_Gen |> select(CommandeID, PanneauID, PanneauType, Fichier3D) |> distinct() |> nrow())) |> select(CommandeID, PanneauID, PieceID, PanneauType, Fichier3D)

    # Append à la table pieces
    pieces <- pieces |> rows_upsert(PiecesDetail_Gen_f, by = 'PieceID')

    # Générer PanneauxDetail

    PanneauDetail <- tibble(
      PieceID = as.numeric(unlist(prod[[2]])[seq(1,length(prod[[2]])*2,2)]),
      FichierDecoupe = unlist(prod[[2]])[seq(2,length(prod[[2]])*2,2)]
    ) |>
      left_join(PiecesDetail, by="PieceID") |>
      group_by(PanneauID) |>
      select(CommandeID, PanneauID,PanneauType,  FichierDecoupe) |>
      filter(!str_detect(FichierDecoupe, "truss|2d")) |>
      add_column(Statut = "TODO", DatePrevue = NA, DateFabrication = NA) |>
      mutate(FichierDecoupe = googledrive::drive_link(googledrive::as_dribble(str_replace(FichierDecoupe, "\\.[23]d\\.", ".")))) |>
      distinct()
    panneaux <- panneaux |> rows_upsert(PanneauDetail, by=c("CommandeID", "PanneauID"))

    # Mise à jour de items des commandes du client
    countTemp <- PanneauDetail |> select(CommandeID, PanneauType) |> count( PanneauType)
    customerOrders[customerOrders$CommandeID == json_data$CommandeID, ]$Items <-
      paste0("{",paste(sapply(1:nrow(countTemp), function(i) paste0(countTemp$PanneauType[i],":", countTemp$n[i])), collapse = "; "),"; 4:1; 5:2}")

    # Mise à jour de la liste des commandes du client
    commandesID <- customerOrders$CommandeID[customerOrders$ClientID == json_client$ClientID]
    customers <- customers |> rows_update(
      by = "ClientID",
      as_tibble(json_client) %>%
        mutate(
          #across(c("Mot_de_passe"), unlist),
          Commandes = paste("[", paste(commandesID, collapse = ";"), "]", sep = "")))

    # Bouger le fichier dans importée
    googledrive::drive_mv(file = JSON_file, path = paste0(dossier_racine, dossier_importee))

  }

  customerOrders <- customerOrders |> mutate(across(starts_with("Date"), function(x) ifelse(is.na(x), "", paste(as.Date(x), "00:00:00"))))

  return(list(customerOrders, customers, pieces, panneaux))
}




