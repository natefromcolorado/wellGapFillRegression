
## to merge the model area with the other metadata ##
mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"
model.name = "NFSEG"
GIS.inventory = read.csv(paste(mainDir, "data/inventory/All_agency_well_inventory.csv", sep ="")) # CSM model inventory
model.inventory = read.csv(paste(mainDir, "data/inventory/archive/",model.name,"_well_inventory.csv", sep ="")) # CSM model inventory
ind = match(GIS.inventory$univName, model.inventory$univName) # match model inventory with all agency mdata
model.inventory = cbind(GIS.inventory, ind)
model.inventory$model.name = ifelse(model.inventory$ind >= 0, model.name, NA)
colnames(model.inventory)[ncol(model.inventory)] = model.name
model.inventory = subset(model.inventory, select = -c(ind))
GIS.inventory = model.inventory
# names(GIS.inventory)
write.csv(GIS.inventory, paste(mainDir, "data/inventory/All_agency_well_inventory_models.csv", sep = ""))

## ADD A COLUMN FOR WELLS IN GEORGIA BASED ON "USGSGA" ##
# georgia = data.frame(univName = GIS.inventory[
#     grep(GIS.inventory$univName, pattern = "USGSGA"),c("univName")], GA = "GA")
GIS.inventory = subset(GIS.inventory, select = -c(GA))
georgia = data.frame(subset(GIS.inventory, GIS.inventory$latDD > 30.3, select = "univName"), GA = "GA")
GIS.inventory = merge(GIS.inventory, georgia, by = "univName", all = TRUE)

## to create sepearate dataframes for each modeling area ##
GIS.inventory.model = model.inventory[!is.na(model),]

## merge the aquifer with the other metadata ##
mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"
GIS.inventory = read.csv(paste(mainDir, "data/inventory/All_agency_well_inventory_models.csv", sep ="")) # CSM model inventory
model.strat =  read.csv(paste(mainDir, "data/inventory/aquifer_inventory_hydrostratigraphic_units.csv", sep =""))
model.strat = subset(model.strat, select = c("univName", "aquiferFinal"))
GIS.inventory = merge.data.frame(GIS.inventory, model.strat, by.x = "univName",by.y = "univName")
GIS.inventory = subset(GIS.inventory, select = -c(X.1,X))
# names(GIS.inventory)
# GIS.inventory[grep(GIS.inventory$univName, pattern = "SRWMD."),]



write.csv(GIS.inventory, paste(mainDir, "data/inventory/All_agency_well_inventory_strat.csv", sep = ""))
