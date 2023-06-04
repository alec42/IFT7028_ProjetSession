library(googledrive)
library(rjson)
library(googlesheets4)

drive_auth(email = "eloise.duhotprevot@gmail.com")

gs4_auth(email = "eloise.duhotprevot@gmail.com")

link_gs <- "https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit?usp=sharing"

# Path du dossier COMPLETEE dans Google Drive
nom_dossier <- "Industrie_VR_IFT7028/Base_de_donnees_ERP/completee/"

# Path du dossier IMPORTEE dans Google Drive
nom_dossier_cible <- "Industrie_VR_IFT7028/Base_de_donnees_ERP/importee"

# Path du fichier JSON permettant l'update dans le drive
json_file <- paste(getwd(), "/JSON/test.json",  sep = "")

# Cherche les fichiers dans le dossier Google Drive
fichiers <- drive_ls(nom_dossier)

# Vérifie si le dossier est vide ou non
if (length(fichiers) > 0) {

    for (fichier in fichiers) {

    json_data <- fromJSON(drive_read_string(fichier))
    db_commandes <- read_sheet(link_gs, sheet = 'Commandes')

    json_data$CommandeID <- 2
    #json_data$Statut <- "Complétée"

    json_commandeID <- json_data$CommandeID

    # Vérifie si la commande existe déjà dans la BD
    if (any(db_commandes$CommandeID == json_commandeID)){

        # Modifie la commande si si elle a un statut MODIFIABLE ou COMPLÉTÉE
        if (json_data$Statut == "Complétée" || json_data$Statut == "Modifiable"){ 
        commande <- filter(db_commandes, CommandeID == json_commandeID)
        commande$Statut <- json_data$Statut
        commande$Items <- "None"
        db_commandes[db_commandes$CommandeID == commande$CommandeID,] <- commande
        write_sheet(db_commandes, link_gs, sheet = "Commandes")
        # AJOUTER LES INFO DANS COMMANDE DETAILS !!!!!!

        }
    } else {

        # Ajoute la nouvelle commande dans la BD
        new_commande <- data.frame(ClientID=json_data$ClientID, CommandeID=json_commandeID, FichiersFabrication="-", Prix=0, Statu=json_data$Statut, DateCommandeCreation=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),DateCommandeLivraison="",Items="None"  ) # nolint
        sheet_append(link_gs, new_commande, sheet='Commandes')

        # AJOUTER LES INFO DANS COMMANDE DETAILS !!!!!!
        #db_commandesDetails <- read_sheet(link_gs, sheet = 'CommandeDetail')
        #sheet_append(link_gs, new_commandeDetail, sheet='CommandeDetail')
    }
  # filter(db_commandes, CommandeID==json_commandeID)

    # Enregistre les modifications effectuées sur le JSON et le déplace dans l'autre dossier
    json_string <- toJSON(json_data)
    write(json_string, json_file)
    #drive_update(file = fichier, media = json_file)
    #drive_mv(file = fichier, path = as_dribble(nom_dossier_cible))
    }

} else {
  print("Le dossier spécifié n'a pas été trouvé dans Google Drive.")
}