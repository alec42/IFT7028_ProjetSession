library(googlesheets4)

gs4_auth(email = "eloise.duhotprevot@gmail.com")

link_gs <- "https://docs.google.com/spreadsheets/d/11JaAXM2rWh7VzRD3BWCzxzcQ1TJDrMQi9Inu8aflRLE/edit?usp=sharing"


db_inventory <- read_sheet(link_gs, sheet = 'Inventaire')

print(db_inventory)
print(length(db_inventory))

test <- filter(db_inventory, ItemID==1)

sheet_append(link_gs, test, sheet='Inventaire')

#test <- filter(db_inventory, ItemId==113)

#print(test)