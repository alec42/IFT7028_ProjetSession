library(googledrive)
library(rjson)

#' @example GDriveJSONUpdate(
#'  dossier_racine = "Industrie_VR_IFT7028/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
#'  customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes'))
GDriveJSONUpdate <- function(
    dossier_racine = "Industrie_VR_IFT7028/", dossier_commandee = "commandes_json/commandée/", dossier_importee = "commandes_json/importée/",
    customerOrders = read_sheet("https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611", sheet = 'Commandes')
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
    json_data <- rjson::fromJSON(drive_read_string(JSON_file, encoding = "utf-8"))[[1]] # temp fix to get only first in json file
    json_data[c("overallDims", "leftSideOptions", "rightSideOptions")] <- NULL
    # Vérifie si la commande existe déjà dans la BD
    if (any(customerOrders$CommandeID == json_data$CommandeID)) {
      # if (json_data$Statut %in% c("Commandée", "Complétée", "Modifiable")) {
        customerOrders <- customerOrders |> rows_update(
          by = "ClientID",
          as_tibble(json_data) %>%
            mutate(
              across(c("ClientID", "CommandeID", "Prix"), as.double),
              across(starts_with("Date"), lubridate::ymd_hms),
              DateCommandeModification=lubridate::ymd_hms(Sys.time())))
        # AJOUTER LES INFO DANS COMMANDE DETAILS !!!!!!
      # }
    } else {
      customerOrders <- customerOrders |> rows_append(
        as_tibble(json_data) %>%
          mutate(
            across(c("ClientID", "CommandeID", "Prix"), as.double),
            across(starts_with("Date"), ~lubridate::ymd_hms(Sys.time())),
            DateCommandeLivraison=NULL, Items=NULL, Prix=NULL))
      # AJOUTER LES INFO DANS COMMANDE DETAILS !!!!!!
      #db_commandesDetails <- read_sheet(link_gs, sheet = 'CommandeDetail')
      #sheet_append(link_gs, new_commandeDetail, sheet='CommandeDetail')
    }
    googledrive::drive_mv(
      file = paste0(dossier_racine, dossier_commandee, JSON_file),
      path = paste0(dossier_racine, dossier_importee, JSON_file),
      overwrite = TRUE)
  }
  return(customerOrders)
}
