#HEy guys, we are moving into the future!
#########
# TITLE: GAP FILLING FOR GROUNDWATER WELLS 
# AUTHOR: GW group
# DATE: 2014-03-27

#packages for data formatting
require(reshape2)
require(stats)
require(gdata)
require(graphics)
require(grDevices)
require(utils)
require(lubridate) # year() function
require(maptools)
require(maps)
require(mapdata)
require(mapproj)

#### USER INPUT ####
##   CORRELATION ANALYSIS ##

#### DATA DIRECTORY ####
mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"
model.name = "All_agency" # which region for cluster analysis (CSM, NFSEG, All_agency, GA)
aquifer.type = c("") # c("FAS", "SAS")

model.data = read.csv(paste(mainDir, "data/common.mdata.QA.csv", sep = ""), header = TRUE) ### all agency mdata
GIS.inventory = read.csv(paste(mainDir, "data/inventory/well_inventory_commonName_20141030.csv", sep =""))
if(model.name != "All_agency"){
  GIS.inventory = subset(GIS.inventory, !is.na(GIS.inventory[model.name]))
  ind = match(model.data$commonName, GIS.inventory$commonName) # match model inventory with all agency mdata
  model.data = model.data[!is.na(ind),] # subset model.data based on model area
}

# unique(model.data.aquifer$aquiferFinal)}  # all aquifer types
if(aquifer.type != ""){ # create column with the aquifer type
  model.data.aquifer = merge(x = model.data, y = GIS.inventory, 
                             by.x = "commonName", 
                             by.y = "commonName")[,c("commonName", "monthYear", "levelNAVD88", "aquiferFinal")] 
  model.data = subset(model.data.aquifer, model.data.aquifer$aquiferFinal %in% aquifer.type) # subset stations based on aquifer.type
}
## DEFINED PARAMETERS ##
station.list = c(NA) #"NWFWMD1311","SJRWMD01140471","SJRWMD01140472") "SRWMD.82202001"# subset well selected for greatest correlation with another, need the N/A placeholder in the first value to select for all stations
# station.list = c("N/A") # subset well selected for greatest correlation with another, need the N/A placeholder in the first value to select for all stations
station.var = "commonName"
level.var = "levelNAVD88"
date.var = "monthYear"
start.date <- as.Date("01/01/1950",format = "%m/%d/%Y")  # Begin date for the analysis
end.date <- as.Date("12/01/2012",format = "%m/%d/%Y")  # End data for the analysis
percent.threshold = 0.01 # percent of data available over the period selected
matched.pairs = 10 # minimum number of pairs in the regresion
max.cor.threshold = 0.9 # must exceed threshold to be considered for filling
regression.period.overlap.threshold = 3 # years of overlapping data for regression

if(model.name == "CSM"){
  map.xlim = c(-84,-80.5) # CSM longitute min and max
  map.ylim = c(27.5,30) # CSM latitude min and max
  county.lines = "Y" # display county lines on map ("Y" or "N")
} else if(model.name == "NFSEG"){
  map.xlim = c(-85,-80) # NFSEG longitute min and max
  map.ylim = c(28.5,33.5) # NFSEG latitude min and max
  county.lines = "Y" # display county lines on map ("Y" or "N")
} else if(model.name == "All_agency"){
  map.xlim = c(-86,-80) # All agency longitute min and max
  map.ylim = c(25,34) # All agency latitude min and max
  county.lines = "N" # display county lines on map ("Y" or "N")
} else stop("Select appropriate model.name that has and inventory .csv file 
            with appropriate latDD and lonDD columns or create a model domain in the previous if else statements")

####  DATA FORMATTING AND PERIOD SELECTION ####
wide.data = dcast(model.data, monthYear~commonName, value.var = "levelNAVD88")
wide.data$monthYear = as.Date(wide.data$monthYear, format = "%Y-%m-%d") # create data for monthYear
wide.data = wide.data[order(wide.data$monthYear),] # order the dataframe
wide.data = wide.data[which((wide.data$monthYear >= start.date) & 
                              (wide.data$monthYear <= end.date)),] # period of record 
# plot(wide.data$monthYear, wide.data$NWFWMD392, main = "NWFWMD392", ylab = "Level NAVD88", xlab = "Date")

####  SELECTION OF WELLS THAT HAVE DATA THAT MEET THRESHOLDS OF % COMPLETE AND PERIOD OF RECORD ####
col.good = numeric(0) # placeholder for numeric vector to record the stations that have adequate data

for (w in 2:length(wide.data[,2:length(wide.data[1,])])){
  num.row = length(wide.data[,w]) # total number of rows
  num.row.obs = nobs(wide.data[,w]) # counts the number of !NA values
  ratio.obs = (num.row.obs/num.row)
  if(ratio.obs > percent.threshold){
    col.good = c(col.good, w) # columns that meet the threshold
  }
}

if(length(col.good) == 1)(
  stop("There are no variables that meet this threshold"))
good.stations = data.frame(wide.data$monthYear,wide.data[col.good]) # dataframe of stations that meet the threshold of % missing
colnames(good.stations)[1] =paste("monthYear")
row.names(good.stations) = wide.data$monthYear
wide.data = good.stations
GIS.good.inventory = GIS.inventory[GIS.inventory$commonName %in% colnames(wide.data),] # remove inventory that does not have data
wide.data = wide.data[c("monthYear", as.character(GIS.good.inventory$commonName))] # remove data that does not have inventory data

if(is.na(station.list)){
  station.list <- as.character(unique(colnames(wide.data[2:length(colnames(wide.data))])))
} # all stations for correlation

# Create several variables for the nested loops that follow
stat.model.merge = data.frame(monthYear = character(), level = numeric(), stringsAsFactors = FALSE)
stat.model.merge.all.wide = data.frame(monthYear = wide.data$monthYear)
stat.model.merge.all.stack = data.frame(monthYear = as.Date(character()), 
                                        levelNAVD88 = numeric(), commonName = character(), 
                                        data.type = character())
sum.stat.table = data.frame(orig_station = character(), 
                            fill_station = character(), 
                            R2 = numeric(), 
                            DF = numeric(), 
                            intercept = numeric(),
                            slope = numeric(),
                            RMSE = numeric())
ind = match(colnames(wide.data), colnames(good.stations))

#### GREATEST CORRELATION BETWEEN SELECTED STATION AND ALL OTHER STATIONS
for (w in 1:length(station.list)) {
  subset.label = station.list[[w]] # define which station
  rsquare = data.frame(r.squared = numeric(0), df = numeric(0), 
                       station = character(0), stringsAsFactors = FALSE)  #create data.frame for R2 values
  subset.meta = subset(GIS.good.inventory,subset = commonName == subset.label)
  GIS.subset.meta =  subset(GIS.good.inventory, latDD > (subset.meta$latDD - 0.5) 
                            & latDD < (subset.meta$latDD + 0.5)
                            & lonDD > (subset.meta$lonDD - 0.5)
                            & lonDD < (subset.meta$lonDD + 0.5))
  location.data = subset(wide.data, select = c("monthYear", as.character(GIS.subset.meta$commonName))) # data that only is within the radius of lat and lon +/- 0.5
  
  for(u in 3:length(colnames(location.data))) # Correlation between subset.label well and all other wells
  {
    check.pairs = nobs(location.data[[subset.label]] + location.data[[u]]) # Number of shared observations
    fillable.pairs = nobs(location.data[[u]]) - check.pairs # Fillable pairs >0 states that there must be data to be filled in.
    end.date.u = max(row.names(na.omit(location.data[u])))
    ind.dates = match(row.names(na.omit(location.data[subset.label])), row.names(na.omit(location.data[u])))  # index of matched dates used in regression
    match.dates = row.names(na.omit(location.data[subset.label]))[!is.na(ind.dates)] # actual matched dates used for regression
    regression.fit.period = (year(max(match.dates, na.rm = TRUE))-year(min(match.dates, na.rm = TRUE))) # number of years of overlap in regression
    
    if(check.pairs > matched.pairs & fillable.pairs > 3  
       & end.date.u > as.Date("2000-05-01")
       & regression.period.overlap.threshold < regression.fit.period)
    {reg =lm(location.data[[subset.label]]~location.data[[u]])
     sum = summary(reg)
     rsquare = rbind(rsquare, 
                     data.frame(r.squared = sum$r.squared, # dataframe of R2,DF for all stations
                                df = sum$df[2],
                                station = colnames(location.data)[[u]]))
    }
  }
  
  rsquare$r.squared[rsquare$r.squared == 1] = 0 # replace perfect correlation with 0
  max.cor = which.max(rsquare$r.squared) # row with max correlation station
  
  if(length(max.cor) == 0){ # if there is no max.cor value, then skip to the next station and fill in spreadsheet with original values
    stat.model.merge.all.wide = cbind(stat.model.merge.all.wide, location.data[subset.label])
    stat.model.merge.all.stack = rbind(
      stat.model.merge.all.stack, 
      data.frame(monthYear = as.character(location.data$monthYear), 
                 levelNAVD88 = location.data[[subset.label]], 
                 commonName = colnames(location.data[subset.label]), 
                 data.type = c("original"), stringsAsFactors = FALSE))
    next}
  
  L = row.names(rsquare) == max.cor # List of T/F rows that match the max.cor between subset.label and all other wells
  best.station = rsquare[L,] # best correlated station
  best.station.name = as.character(best.station[[3]]) # name of best correlated station
  final.reg =lm(location.data[[subset.label]]~location.data[[best.station.name]]) # best correlated well regression model
  
  if(summary(final.reg)$adj.r.squared<= max.cor.threshold){ # if the best final regression does not meet the threshold, then 
    stat.model.merge.all.wide = cbind(stat.model.merge.all.wide, location.data[subset.label])
    stat.model.merge.all.stack = rbind(
      stat.model.merge.all.stack, 
      data.frame(monthYear = as.character(location.data$monthYear),
                 levelNAVD88 = location.data[[subset.label]], 
                 commonName = colnames(location.data[subset.label]), 
                 data.type = c("original"), stringsAsFactors = FALSE))
    next}
  
  stat.model = predict.lm(object = final.reg, 
                          newdata = data.frame(location.data[[subset.label]])) # create lm statistical model of subset.label using the best.station
  station.fill = data.frame(stat.model, location.data[[subset.label]]) # filled station and stat.model dataframe
  ind = match(station.fill[,2], station.fill[,1]) # index of points that stat model will fill of original station
  ind1 = data.frame(data.type =as.character(ind), filled ="filled", 
                    original = "original", stringsAsFactors = FALSE)
  ind1[is.na(ind),1] = ind1[is.na(ind),3]
  ind1[!is.na(ind),1] = ind1[!is.na(ind),2]
  
  station.fill[!is.na(ind),2] = station.fill[!is.na(ind),1] # create a dataframe with filled station and stat model
  stat.model.merge = data.frame(location.data$monthYear, station.fill[,2]) # station filled in with best regressed station.
  colnames(stat.model.merge) = c("monthYear", paste(subset.label, "_filled_station", sep = ""))
  stat.model.merge.all.wide = cbind(stat.model.merge.all.wide, stat.model.merge[2])
  colnames(stat.model.merge) = c("monthYear", "levelNAVD88")
  
  stat.model.merge.all.stack = rbind(
    stat.model.merge.all.stack, 
    data.frame(monthYear = as.character(stat.model.merge$monthYear), 
               levelNAVD88 = stat.model.merge$levelNAVD88, 
               commonName = subset.label, data.type = ind1[1]))
  
  sum.stat = data.frame(orig_station = subset.label, 
                        fill_station = best.station.name, 
                        R2 = format(summary(final.reg)$adj.r.squared, digits = 3), 
                        DF = format(summary(final.reg)$df[2]), 
                        intercept = final.reg$coefficients[1], 
                        slope = final.reg$coefficients[2], 
                        RMSE = format(sqrt(mean(final.reg$residuals^2)), digits = 3))
  
  sum.stat.table = rbind(sum.stat.table, sum.stat)
  
  ##### FIGURES #####
  subset.station.GIS = subset(GIS.inventory, commonName == subset.label)
  best.station.GIS = subset(GIS.inventory, commonName == best.station.name)
  subDirParam = paste("MaxCorr_",max.cor.threshold, "_MatchPair_",
                      matched.pairs, "_MinRegPeriod",regression.period.overlap.threshold, sep = "") # Place several parameters in folder naming convention
  subDir = paste("figures/gapFill/", model.name,"_filled_figure",subDirParam, sep = "")
  dir.create(paste(mainDir, subDir, sep = ""), showWarnings = TRUE)
  png(paste(file = mainDir,subDir,"/Hydrograph_",subset.label,".png", sep =""),
      width = 1000, height = 650, units= "px", res=100)        
  par(mfrow = c(2,2), mar = c(3,4,3,1))
  
  ## Hydrographs of two stations
  ylimlow = min(c(location.data[[subset.label]],location.data[[best.station.name]]), na.rm = TRUE)
  ylimhigh = max(c(location.data[[subset.label]],location.data[[best.station.name]]), na.rm = TRUE)
  plot(location.data$monthYear,location.data[[subset.label]], type = "p",
       main= paste("Original station: ", subset.label," (",subset.station.GIS$aquiferFinal,")", "\n", "Fill station: ", best.station.name," (",best.station.GIS$aquiferFinal,")", sep = ""),
       pch = 20, xlab = "Date", ylab = "Level (NAVD88,ft)", 
       ylim = c(ylimlow, (ylimhigh+((ylimhigh-ylimlow)/5))), 
       xlim = c(start.date, end.date))
  points(location.data$monthYear,location.data[[best.station.name]], 
         type = "p", pch = 1, col = "red")
  leg.text <- c(paste("Station:", subset.label), paste("Fill Station:", best.station.name))
  legend("topleft", leg.text, ,bty = "n",col = c("black","red"), 
         lty = c(0, 0),pch=c(20,1), merge = F, border=F)
  
  ## Correlation between stations
  plot(location.data[[subset.label]]~location.data[[best.station.name]], 
       xlab = best.station.name, ylab = subset.label, main = "Regression")
  abline(final.reg)
  legend(x = "topleft", bty= "n",
         legend = c(paste("R2 =", format(summary(final.reg)$adj.r.squared, digits = 3)), 
                    paste("DF =", format(summary(final.reg)$df[2])),
                    paste("RMSE =", format(sqrt(mean(final.reg$residuals^2)), digits = 3))
         )
  )
  
  ## Filled station
  ylimlow = min(stat.model.merge[2], na.rm = TRUE)
  ylimhigh = max(stat.model.merge[2], na.rm = TRUE)
  plot(location.data$monthYear[!is.na(ind)], 
       station.fill[!is.na(ind),1], col = "red", type = "p", pch = 1, 
       main = paste("Filled station:", subset.label), ylab = "Level (NAVD88,ft)", xlab = "Date", 
       ylim = c(ylimlow, (ylimhigh+((ylimhigh-ylimlow)/5))), xlim = c(start.date, end.date))
  points(location.data$monthYear, location.data[[subset.label]],
         col = "black", type = "p", pch = 20)
  legend("topleft", bty = "n", c("Original data", "Filled data"), col = c("black","red"), 
         lty = c(0, 0),pch=c(20,1), merge = F, border=F)
  
  ### Observed vs. Estimated plot
  #   plot(station.fill[,2], station.fill[,1], col = "black", type = "p", pch = 1, 
  #        main = paste("Filled station:", subset.label), ylab = "Observed", xlab = "Predicted")
  #   abline(lm(station.fill[,2] ~station.fill[,1]))
  #   legend(x = "topleft", bty= "n",
  #          legend = c(paste("R2 =", format(summary(final.reg)$adj.r.squared, digits = 3)), 
  #                     paste("DF =", format(summary(final.reg)$df[2])),
  #                     paste("RMSE =", format(sqrt(mean(final.reg$residuals^2)), digits = 3))
  #                     )
  #   )
  
  
  # Map of original and filled well
  if (county.lines == "Y"){map("county", xlim=map.xlim, ylim=map.ylim, col="gray90", fill=TRUE)
  } else {map("state", xlim=map.xlim, ylim=map.ylim, col="gray90", fill=TRUE)}
  map.axes()
  points(x = subset.station.GIS$lonDD, y = subset.station.GIS$latDD, col = 'black', pch = 19)
  points(x = best.station.GIS$lonDD, y = best.station.GIS$latDD, col = 'red', pch = 1)
  dev.off()
}

# ##### PRINT DATA FILES #####
write.csv(sum.stat.table, file = paste(mainDir, "data/Sum_stat_table_",model.name,"_MaxCorr_",max.cor.threshold, "_MatchPair_",matched.pairs,"_MinRegPeriod",regression.period.overlap.threshold,".csv", sep="")) # summary stats table of statistical models for filling data
write.csv(stat.model.merge.all.wide, file = paste(mainDir, "data/Filled_stations_wide_",model.name,"_MaxCorr_",max.cor.threshold, "_MatchPair_",matched.pairs,"_MinRegPeriod",regression.period.overlap.threshold,".csv", sep="")) # wide format .csv
stat.model.merge.all.stack = na.omit(stat.model.merge.all.stack)
write.csv(stat.model.merge.all.stack, file = paste(mainDir, "data/Filled_stations_stack_",model.name,"_MaxCorr_",max.cor.threshold, "_MatchPair_",matched.pairs,"_MinRegPeriod",regression.period.overlap.threshold,".csv", sep="")) # stacked format .csv