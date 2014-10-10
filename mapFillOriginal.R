# This file has no longer any way of git push
# Title: mapFillOriginal.R
# Author: Nathan Johnson
# Date: 2014.06.13

require(reshape2)
require(gdata) #nobs function
require(cluster)
require(clusterSim)
require(zoo)
require(maptools)
require(maps)
require(mapdata)
require(mapproj)
require(lubridate) # year() function

# Data 
mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"
model.area = "All_agency" # which region for cluster analysis (CSM, NFSEG, All_agency)
aquifer.type = c("UFA") # "UFA", "MultiAquifer", "SAS", "ICU", "UZLFA", "check", "LFA", "ULFA", "FPZ", "MCU", "Crystalline Ride Aquifer", "Brunswick Aquifer System", "LSCU", "SECPA", "Bottom Aquf", "Crystalline Rock Aquifer", "Valley and Ridge Aquifer"
percent.threshold = 0.001 # percent of data available over the period selected
new.data = "N"
filled.Data = "Filled"

## Data
if(new.data == "Y"){
  if(filled.Data =="Original"){
    model.domain.data1 = read.csv(paste("C:/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata
  } else {model.domain.data1 = read.csv(paste(mainDir, "data/Filled_stations_stack_second_All_agency_MaxCorr_0.9_MatchPair_10_MinRegPeriod3.csv", sep = ""), header = TRUE)} ### all agency mdata
}
model.domain.data = model.domain.data1

GIS.inventory = read.csv(paste(mainDir, "data/inventory/All_agency_well_inventory_strat.csv", sep =""))
if(model.area != "All_agency"){
  GIS.inventory = subset(GIS.inventory, !is.na(GIS.inventory[model.area]))}
ind = match(model.domain.data$univName, GIS.inventory$univName) # match model inventory with all agency mdata
model.domain.data = model.domain.data[!is.na(ind),] # subset model.domain.data based on model area
# unique(model.domain.data.aquifer$aquiferFinal)}  # all aquifer types
if(aquifer.type != ""){ # create column with the aquifer type
  GIS.inventory.aquifer = subset(GIS.inventory, 
                      GIS.inventory$aquiferFinal %in% aquifer.type) # subset stations based on aquifer.type
  model.domain.data = subset(model.domain.data, model.domain.data$univName %in% GIS.inventory.aquifer$univName)
}
model.domain.data$year = year(model.domain.data$monthYear)

#### DEFINED PARAMETERS ####
# station.list = c("SJRWMD15282844","SJRWMD19324384") # Well selected for greatest correlation with another
start.date <- as.Date("01/01/2001",format = "%m/%d/%Y")  # Begin date for the analysis
end.date <- as.Date("12/31/2001",format = "%m/%d/%Y")  # End data for the analysis
period = paste(" ",year(start.date), "-", year(end.date))

if(model.area == "CSM"){
  map.xlim = c(-84,-80.5) # CSM longitute min and max
  map.ylim = c(27.5,30) # CSM latitude min and max
  county.lines = "Y" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 550
  leg.loc = "bottomleft"
} else if(model.area == "NFSEG"){
  map.xlim = c(-85,-80) # NFSEG longitute min and max
  map.ylim = c(28.5,33.5) # NFSEG latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 700
  leg.loc = "bottomleft"
} else if(model.area == "All_agency"){
  map.xlim = c(-86,-80) # All agency longitute min and max
  map.ylim = c(25,34) # All agency latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 1000
  leg.loc = "bottomleft"
} else if(model.area == "GA"){
  map.xlim = c(-85.5,-80.5) # NFSEG longitute min and max
  map.ylim = c(30.3,33.5) # NFSEG latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
  png.width = 650
  png.height = 550
  leg.loc = "topleft"
} else stop("Select appropriate model.area that has and inventory .csv file 
            with appropriate latDD and lonDD columns or create a model domain in the previous if else statements")

good.stations.stack = subset(model.domain.data, subset = as.Date(monthYear) >= start.date & 
                               as.Date(monthYear) <= end.date)

subDir = paste("figures/aquiferStationMap/", model.area, "/",sep = "")
for(aq in 1:length(aquifer.type)){
    png(paste(file = mainDir,subDir,model.area,"_",aquifer.type[aq],"_Compare_Fill_",period,"_Run_",as.Date(Sys.time()),".png", sep =""), width = png.width, height = png.height, units= "px", res=90,  antialias = "cleartype")
  if (county.lines == "Y"){map("county", xlim = map.xlim, ylim = map.ylim , col ="gray90", fill = TRUE)
  } else {map("state", xlim = map.xlim, ylim = map.ylim , col ="gray90", fill = TRUE)}
  map.axes()
  GIS.good.inventory = GIS.inventory[GIS.inventory$univName %in% unique(good.stations.stack$univName), ]
  aquifer.subset = subset(GIS.good.inventory, GIS.good.inventory$aquiferFinal == aquifer.type[aq])
  points(x = aquifer.subset$lonDD, y = aquifer.subset$latDD, col = "red", pch = 1, cex = 0.75)
  }

### ORIGINAL ###
model.domain.data = read.csv(paste("C:/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata
GIS.inventory = read.csv(paste(mainDir, "data/inventory/All_agency_well_inventory_strat.csv", sep =""))
if(model.area != "All_agency"){
  GIS.inventory = subset(GIS.inventory, !is.na(GIS.inventory[model.area]))}
ind = match(model.domain.data$univName, GIS.inventory$univName) # match model inventory with all agency mdata
model.domain.data = model.domain.data[!is.na(ind),] # subset model.domain.data based on model area
# unique(model.domain.data.aquifer$aquiferFinal)}  # all aquifer types

if(aquifer.type != ""){ # create column with the aquifer type
  GIS.inventory.aquifer = subset(GIS.inventory, 
                      GIS.inventory$aquiferFinal %in% aquifer.type) # subset stations based on aquifer.type
  model.domain.data = subset(model.domain.data, model.domain.data$univName %in% GIS.inventory.aquifer$univName)
} 
model.domain.data$year = year(model.domain.data$monthYear)
good.stations.stack = subset(model.domain.data, subset = as.Date(monthYear) >= start.date & 
                               as.Date(monthYear) <= end.date)

####  SELECTION OF WELLS THAT HAVE DATA THAT MEET THRESHOLDS OF % COMPLETE AND PERIOD OF RECORD ####
for(aq in 1:length(aquifer.type)){
  GIS.good.inventory = GIS.inventory[GIS.inventory$univName %in% unique(good.stations.stack$univName),]
  aquifer.subset = subset(GIS.good.inventory, GIS.good.inventory$aquiferFinal == aquifer.type[aq])
  points(x = aquifer.subset$lonDD, y = aquifer.subset$latDD, col = "black", pch = 16)
  legend("topleft", title = paste(aquifer.type[aq],period, sep = " "),merge = FALSE, border=F, cex = 1, legend = c("Original", "Filled"), pch = c(16, 1), col = c("black", "red"))
}
dev.off()

### PLOT WELLS IN INDIVIDUAL AQUIFER BY YEAR###
# subDir = paste("figures/aquiferStationMap/", model.area, "/",sep = "")
# aquifer.type = as.character(unique(GIS.inventory$aquiferFinal))
# # aquifer.type = "SAS"
# for (aq in 1:length(aquifer.type)){
# png(paste(file = mainDir,subDir,model.area,"_",aquifer.type[aq],"_byYear_Run_",as.Date(Sys.time()),".png", sep =""),
#     width = 650, height = 1000, units= "px", res=90,  antialias = "cleartype")
# par(mfrow = c(4,3), mar = c(1,0,0,0))
# for (year.type in 2000:2011){
# #     year.type = "2010"
#   if (county.lines == "Y"){map("county", xlim = map.xlim, ylim = map.ylim , col ="gray90", fill = TRUE)
#   } else {map("state", xlim = map.xlim, ylim = map.ylim , col ="gray90", fill = TRUE)}
#   map.axes()
#   aquifer.subset = subset(GIS.inventory, GIS.inventory$aquiferFinal == aquifer.type[aq])
#   model.domain.data.year = subset(model.domain.data, subset = model.domain.data$year == year.type)
#   aquifer.subset = subset(aquifer.subset, subset = aquifer.subset$univName %in% unique(model.domain.data.year$univName))
#   points(x = aquifer.subset$lonDD, y = aquifer.subset$latDD, col = 1, pch = 3)
#   legend("topleft", legend = c(aquifer.type[aq], year.type), merge = FALSE, border=F, cex = 1.2)
# }
# dev.off()
# }

# aquifer.types = as.character(unique(GIS.inventory$aquiferFinal))
# model.domain.data.aquifer = merge(model.domain.data, GIS.inventory[c("univName", "aquiferFinal")], by = "univName", all = FALSE)
# aquifer.count = do.call("rbind",lapply(unique(model.domain.data.aquifer$aquiferFinal), function(x) data.frame(aquifer.count = length(grep(model.domain.data.aquifer$aquiferFinal, pattern = x)),aquifer.type = x)))
# aquifer.count
# write.csv(GIS.inventory, paste(mainDir, "data/",model.area,"_GIS_inventory.csv", sep =""))
# write.csv(model.domain.data, paste(mainDir, "data/",model.area,"_model_data.csv", sep =""))
