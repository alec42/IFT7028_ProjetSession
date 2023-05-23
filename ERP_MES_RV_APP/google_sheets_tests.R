library(tidyverse)
source("scripts/googlesheets_access.R") # get link to gs

db_inventory <- read_sheet(link_gs, sheet = 'inventaire')
db_orders <- read_sheet(link_gs, sheet = 'commandes')
gs_add_row <- function(gs = link_gs, sheet, data) {
    sheet_append(ss = gs, sheet = sheet, data = data |> as.list() |> data.frame())
}
# gs_add_row(link_gs, sheet = "inventaire", data = db_inventory)

gs_overwrite <- function(gs = link_gs, sheet, data) {
    sheet_write(data, gs, sheet)
}
# db_inventory <- db_inventory |>
#     add_row(item='planche dessous', quantite=2, materiel='bois', epaisseur=4, longueur=16, largeur=8)
# gs_overwrite(link_gs, "inventaire", db_inventory)