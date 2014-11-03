# Title: dataAvailability.R
# Author: Nathan Johnson
# Date: 2014.06.13

require(reshape2)
require(gdata) # nobs function
require(zoo)
require(maptools)
require(maps)
require(mapdata)
require(mapproj)
require(lubridate) # year() function
require(plyr) # count() function

mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"
model.area = "GA" # which region for cluster analysis (CSM, NFSEG, All_agency, GA)
# aquifer.type = c("UFA") # "UFA", "MultiAquifer", "SAS", "ICU", "UZLFA", "check", "LFA", "ULFA", "FPZ", "MCU", "Crystalline Ride Aquifer", "Brunswick Aquifer System", "LSCU", "SECPA", "Bottom Aquf", "Crystalline Rock Aquifer", "Valley and Ridge Aquifer"
obs.min = 3 # minimum data available over the period selected
obs.max = 396 # maximum data available over the period selected
filled.Data = "Filled" # "Original" "Filled"
new.data = "Y" # "Y", "N"

## Data
if(new.data == "Y"){
  if(filled.Data =="Original"){
    model.domain.data1 = read.csv(paste(mainDir, "data/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata

  } else {model.domain.data1 = read.csv(paste(mainDir, "data/gapFill/Filled_stations_stack_second_All_agency_MaxCorr_0.9_MatchPair_10_MinRegPeriod3.csv", sep = ""), header = TRUE)}
}
model.domain.data = model.domain.data1

GIS.inventory = read.csv(paste(mainDir, "data/inventory/All_agency_well_inventory_strat_20141009.csv", sep =""))
if(model.area != "All_agency"){
  GIS.inventory = subset(GIS.inventory, !is.na(GIS.inventory[model.area]))}
ind = match(model.domain.data$univName, GIS.inventory$univName) # match model inventory with all agency mdata
model.domain.data = model.domain.data[!is.na(ind),] # subset model.domain.data based on model area
# unique(model.domain.data.aquifer$aquiferFinal)}  # all aquifer types
if(length(aquifer.type >= 0)){ # create column with the aquifer type
  GIS.inventory.aquifer = subset(GIS.inventory, 
                      GIS.inventory$aquiferFinal %in% aquifer.type) # subset stations based on aquifer.type
  model.domain.data = subset(model.domain.data, model.domain.data$univName %in% GIS.inventory.aquifer$univName)
}
model.domain.data$year = year(model.domain.data$monthYear)

#### DEFINED PARAMETERS ####
# station.list = c("SJRWMD15282844","SJRWMD19324384") # Well selected for greatest correlation with another
start.date <- as.Date("01/01/2000",format = "%m/%d/%Y")  # Begin date for the analysis
end.date <- as.Date("12/31/2012",format = "%m/%d/%Y")  # End data for the analysis
period = paste(" ",year(start.date), "-", year(end.date))
range = as.numeric(end.date - start.date)/30.5/2

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

model.domain.data = subset(model.domain.data, subset = as.Date(monthYear) >= start.date & 
                               as.Date(monthYear) <= end.date)
subDir = paste("figures/aquiferStationMap/", model.area, "/",sep = "")

aquifer.type = as.character(unique(GIS.inventory$aquiferFinal))
for(aq in 1:length(aquifer.type)){
  png(paste(file = mainDir,subDir,model.area,"_",aquifer.type[aq],"_", 
            filled.Data ,"_Data_Quantity_",period,
            "_Run_",as.Date(Sys.time()),".png", sep =""), 
      width = png.width, height = png.height, units= "px", 
      res=90,  antialias = "cleartype")
  if (county.lines == "Y"){
    map("county", xlim = map.xlim[1:2], ylim = map.ylim[1:2], col ="gray90", fill = TRUE)
  } else {map("state", xlim = map.xlim, ylim = map.ylim , col ="gray90", fill = TRUE)}
  map.axes()
  GIS.good.inventory = GIS.inventory[GIS.inventory$univName %in% unique(model.domain.data$univName),]
  aquifer.subset = subset(GIS.good.inventory, GIS.good.inventory$aquiferFinal == aquifer.type[aq])
  if (nrow(aquifer.subset) == 0){
    dev.off()
    next}
  data.avail = count(model.domain.data, "univName")
  aquifer.subset = merge(x=aquifer.subset, y = data.avail, by = "univName")
  aquifer.subset = subset(aquifer.subset, subset = aquifer.subset$freq >= obs.min & 
                            aquifer.subset$freq <= obs.max)
  subset.data = merge(x=aquifer.subset, y=model.domain.data, by = "univName")
  subset.data = subset(subset.data, select = c("univName","monthYear", "levelNAVD88"))
  
  points(x = aquifer.subset$lonDD, y = aquifer.subset$latDD, 
         col = "blue", pch = 1, cex = aquifer.subset$freq/range)
  text(x = aquifer.subset$lonDD, y = aquifer.subset$latDD, 
       labels = aquifer.subset$freq, col = "black", pos = 4, cex = 0.6)  
  legend(leg.loc, title = paste(aquifer.type[aq],period, sep = " "),merge = FALSE, border=F, 
         pt.cex = c(max(aquifer.subset$freq/range), min(aquifer.subset$freq/range)),
         legend = c(paste(max(aquifer.subset$freq),"Monthly_Values"), 
                    paste(min(aquifer.subset$freq),"Monthly_Values")), pch = c(1,1), col = c("blue"))
dev.off()
}
