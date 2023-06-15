library(reticulate)
library(tidyverse)
# setwd("./3-Production-Assemblage/")
packages_install <- pull(read_table("packages_py.txt", col_names = "packages"), packages) |> as.list.data.frame()
reticulate::py_install(packages = packages_install, pip = T)

reticulate::source_python("Production/ErpCommunication.py")

prod <- GetProductionInformation(currentOrderId = '97297049', firstPannelId = 89, firstPieceId = 89)
PiecesDetail <- prod[[1]] |> as_tibble() |> mutate(across(c(PanneauID, PieceID, PanneauType), unlist)) |> mutate(CommandeID = as.numeric(CommandeID))
PiecesDetail

PanneauDetail <- tibble(
    PieceID = as.numeric(unlist(prod[[2]])[seq(1,length(prod[[2]])*2,2)]),
    FichierDecoupe = unlist(prod[[2]])[seq(2,length(prod[[2]])*2,2)]
) |>
    left_join(PiecesDetail, by="PieceID") |>
    group_by(PanneauID) |>
    select(CommandeID, PanneauID,PanneauType,  FichierDecoupe) |>
    filter(!str_detect(FichierDecoupe, "truss")) |>
    add_column(Statut = "TODO", DatePrevue = NA, DateFabrication = NA) |>
    mutate(FichierDecoupe = googledrive::drive_link(googledrive::as_dribble(str_replace(FichierDecoupe, "\\.[23]d\\.", ".")))) |>
    distinct()
PanneauDetail
assembly <- GetAssemblyInformation(currentOrderId = '97297049')
assembly
