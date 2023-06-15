library(googlesheets4)
library(googledrive)
library(rjson)

# Google Sheets DB --------------------------------------------------------
gs4_auth(cache = ".secrets", email = "dexstroy.gaming@gmail.com") #alecvanrassel dexstroy.gaming
link_gs_erp <- "https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit#gid=2103113611"

# Google Drive ------------------------------------------------------------
drive_auth(cache = ".gdrive_secrets", scopes = "https://www.googleapis.com/auth/drive", email = "dexstroy.gaming@gmail.com") # eloise.duhotprevot