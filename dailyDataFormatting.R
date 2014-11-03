#########
# TITLE: DAILY DATA AGGREGATION AND MONTHLY DATA FORMATTING 
# AUTHOR: NATHAN JOHNSON
# DATE: 2014-03-27

require(plyr)

#### compiling the daily data for editing
mainDir = "H:/Engineering_and_Hydro_Science/Projects/Groundwater_Resource_Assessment/groundwater_levels_NAVD88_Florida_Georgia/groundwater/"

## create daily data.frames of individual agencies 
SFWMD_ddata = read.csv(paste(mainDir,"/SFWMD/data/well_data_cont_sfwmd.csv", sep =""), header = FALSE)
colnames(SFWMD_ddata) = c("SFWMD_ID", "date", "levelNGVD29", "qual")
SFWMD_ddata1 = read.csv(paste(mainDir,"/SFWMD/data/well_data_disc_sfwmd.csv", sep =""), header = FALSE)
colnames(SFWMD_ddata1) = c("SFWMD_ID", "date", "levelNGVD29", "qual")
SFWMD_ddata = rbind(SFWMD_ddata, SFWMD_ddata1)
SFWMD_ddata = na.omit(SFWMD_ddata)
agency.ddata = data.frame(univName = paste("SFWMD",SFWMD_ddata$SFWMD_ID, sep = ""), date = as.Date(as.character(SFWMD_ddata$date), "%Y%m%d"), levelNGVD29 = SFWMD_ddata$level, agency = "SFWMD")
SFWMD.GIS.inventory = read.csv(paste(mainDir,"/SFWMD/data/SFWMD_GW_inventory.csv", sep =""), header = TRUE)
SFWMD.GIS.inventory.offset = subset(SFWMD.GIS.inventory, select = c("univName", "NAVD88_offset"))
agency.ddata.off = merge(agency.ddata, SFWMD.GIS.inventory.offset)
agency.ddata.off[,"level"] = agency.ddata.off[,"levelNGVD29"] + agency.ddata.off[,"NAVD88_offset"]
df.SFWMD_ddata = agency.ddata.off[,c("univName","date", "level", "agency")]

NWFWMD_ddata = read.csv(paste(mainDir,"/NWFWMD/data/NWFWMD_GW_daily.csv", sep =""), header = TRUE)
df.NWFWMD_ddata = data.frame(univName = paste("NWFWMD",NWFWMD_ddata$stationName, sep = ""), date = as.Date(NWFWMD_ddata$date, format = "%m/%d/%Y"), level = NWFWMD_ddata$GW_level_NAVD88, agency = "NWFWMD")

SWFWMD_ddata = read.csv(paste(mainDir,"/SWFWMD/data/SWFWMD_GW_daily.csv", sep =""), header = TRUE)
df.SWFWMD_ddata = data.frame(univName = SWFWMD_ddata$univName, date = as.Date(SWFWMD_ddata$date, format = "%m/%d/%Y"), level = SWFWMD_ddata$level, agency = "SWFWMD")

SRWMD_ddata = read.csv(paste(mainDir,"/SRWMD/data/SRWMD_GW_daily.csv", sep =""), header = TRUE)
df.SRWMD_ddata = data.frame(univName = SRWMD_ddata$univName, date = as.Date(SRWMD_ddata$date, format = "%m/%d/%Y"), level = SRWMD_ddata$level, agency = "SRWMD")

SJRWMD_ddata = read.csv(paste(mainDir,"/SJRWMD/data/SJRWMD_GW_daily_NAVD88.csv", sep =""), header = TRUE)
df.SJRWMD_ddata = data.frame(univName = SJRWMD_ddata$univName, date = as.Date(SJRWMD_ddata$date, format = "%m/%d/%Y"), level = SJRWMD_ddata$level, agency = "SJRWMD")

USGSFL_ddata = read.csv(paste(mainDir,"/USGS Florida/data/USGSFL_daily.csv", sep =""), header = TRUE)
df.USGSFL_ddata= data.frame(univName = USGSFL_ddata$univName, date = as.Date(USGSFL_ddata$date, format = "%m/%d/%Y"), level = USGSFL_ddata$level, agency = "USGSFL")

USGSGA_ddata = read.csv(paste(mainDir,"/USGS Georgia/data/USGSGA_GW_daily.csv", sep =""), header = TRUE)
df.USGSGA_ddata= data.frame(univName = USGSGA_ddata$univName, date = as.Date(USGSGA_ddata$date, format = "%m/%d/%Y"), level = USGSGA_ddata$level_NAVD88, agency = "USGSGA")

USGSSC_ddata = read.csv(paste(mainDir, "/USGS South Carolina/data/USGS_SC_GW_daily_NAVD88.csv", sep =""), header = TRUE)
df.USGSSC_ddata= data.frame(univName = USGSSC_ddata$univName, date = as.Date(USGSSC_ddata$date, format = "%m/%d/%Y"), level = USGSSC_ddata$level, agency = "USGSSC")

## create data.frame of daily values in stacked format
agency.ddata = data.frame()
agency.ddata = rbind(df.NWFWMD_ddata, df.SWFWMD_ddata, df.SRWMD_ddata, df.SJRWMD_ddata, df.USGSFL_ddata, df.USGSGA_ddata, df.USGSSC_ddata, df.SFWMD_ddata)
# write.csv(agency.ddata, paste(mainDir, "data/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata

## Read the ddata and put data with common USGS IDs into the same 
# agency.ddata.orig = read.csv(file = paste(mainDir, "data/all.agency.ddata.csv", sep = ""), header = TRUE, colClasses = c("character", "character", "Date", "numeric", "character"))  ### all agency
agency.ddata.usgs = read.csv(file = paste(mainDir, "data/common.ddata.QA.csv", sep = ""), header = TRUE, colClasses = c("character", "character", "Date", "numeric", "character", "character"))  ### all agency

GIS.inventory = read.csv(paste(mainDir, "data/inventory/well_inventory_commonName_20141029.csv", sep =""), header = TRUE, colClasses = "character")
df = data.frame(GIS.inventory["univName"],GIS.inventory["commonName"])
agency.ddata.usgs = merge(agency.ddata.orig, df, by = "univName", all.x = TRUE, all.y = FALSE)
agency.ddata.usgs[is.na(agency.ddata.usgs$commonName), c('commonName')] = agency.ddata.usgs[is.na(agency.ddata.usgs$commonName), c('univName')]
agency.ddata = agency.ddata.usgs

## create data.frame of monthly values using the commonName
agency.mdata = aggregate(x = agency.ddata.usgs$level, 
                  by = list(commonName = agency.ddata.usgs$commonName, 
                            monthYear = format(agency.ddata.usgs$date, "%Y-%m")), FUN = median) ## aggregate based on month and station
## create data.frame of monthly values using the univName
agency.mdata = aggregate(level = agency.ddata$level, 
                  by = list(univName = agency.ddata$univName, 
                            monthYear = format(agency.ddata$date, "%Y-%m")), FUN = median) ## aggregate based on month and station

names(agency.mdata)[2:3] = c("monthYear", "levelNAVD88")
agency.mdata$monthYear = paste(agency.mdata$monthYear,"-01", sep = "") ### add 1 day to treat the column as a date
agency.mdata$monthYear = as.Date(agency.mdata$monthYear, format = "%Y-%m-%d")
agency.mdata$commonName = sub(pattern='-', replacement= '.', x= agency.mdata$commonName)
agency.mdata1 = agency.mdata  # keep the original agency.mdata stored as agency.mdata1
write.csv(agency.mdata, file = paste(mainDir, "data/common.mdata.QA.csv", sep = ""), row.names = FALSE)  ### all agency mdata

## import the new .csv file of agency.mdata
agency.mdata = read.csv(file = paste(mainDir, "data/all.agency.mdata.csv", sep = ""), header = TRUE) ### all agency mdata
agency.mdata$monthYear = as.Date(agency.mdata$monthYear, format = "%Y-%m-%d")
agency.mdata$monthYear = as.Date(agency.mdata$monthYear, format = "%m/%d/%Y")
ddata = agency.ddata.usgs

#### REMOVED DATA ####
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18967" & (level < 84.1)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18440" & (level < 58)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18464" & (level < 57)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18927" & (level < 54.5)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20866" & (level < 63)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18341" & (level < 72.1)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18360" & (level < 69.435)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18383" & (level < 70.978)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18929" & (level < 65.09)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23197" & (level < 72.7)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18373" & (level < 71.5)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD23334"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23130" & (level == 37.827)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18398" & (level < 54.5)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20733" & (level == 9.51))) 
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD15870291" & (level < 20))) # 5 points
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD28735081" & (level == 212.886)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD726894"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23519"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23523" & (level == 62.219)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD670762"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23523" & (level == 62.219)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD31453498" & (level < 0.17)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA335743084003901" & (level < 990)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20879" & (level == 64.514)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18379" & (level == 72.534)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD19347" & (level == 87.217)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23203" & (level == 71.944)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD22940" & (level == 32.489)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "NWFWMD2196" & (date == "2009-12-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "NWFWMD2957"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "NWFWMD3181" & (date == "2009-12-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23523" & (level == 62.219)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23067" & (level == 47.493)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20970" & (level == 42.01)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA311454081210503" & (date == "2006-04-18")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20970" & (level == 42.01)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23231"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "NWFWMD4026" & (date == "2002-06-04")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL263955082083102" & (level < 5.0)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD21036"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL285124082245601" & (date == "1987-10-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA311049081212801" & (level < -10.0))) # 6 points 1970's 1960's
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA310845081222601" & (level < -8.0))) # 4 points 1970's
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA310918081400801" & (level < -8.0))) # 2 points 1960's
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL302410084200002" & (date < "1974-01-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA310918081400801" & (level < 0))) 
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA323301083263601"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD18661"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD18660"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SJRWMD02301223"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SRWMD-101429020" & (date == "2005-05-27")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD70591581" & (date < "1977-04-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD11211584" & (date < "1977-11-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL290112082371101" & (date < "1967-12-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD19369" & (level == 88.387)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD19369" & (level == 88.487)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18484" & (level == 58.783)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20936" & (date == "2002-05-14")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA320134084003603" & (date == "1994-10-28")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA320154083593401" & (date == "1994-06-16")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSGA310900081341401" & (date == "1968-01-18")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL272655080401601" & (date == "1986-05-22")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL262724081260701" & (date < "1976-01-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SJRWMD02201151" & (date < "1995-01-01")))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD19365" & (level < 85.597)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD23544" & (level < 59.4)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD25356"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD18975" & (level < 73.5)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD20477" & (level < 53.5)))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD18438"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD19038"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName ==  "SWFWMD23559"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "USGSFL255709080223701"))
agency.ddata.usgs = subset(agency.ddata.usgs, !(univName == "SWFWMD19653" & (date == "2009-11-02")))


## rewrite the agency.ddata.usgs file after all the ouliers and bad data have been removed
colnames(agency.mdata)[3] = c("levelNAVD88")
write.csv(agency.mdata, file = paste(mainDir, "data/common.mdata.QA.csv", sep = ""), row.names = FALSE) ### all agency ddata tha thas been QAed using cluster analysis

## examine a single cluster after the cluster script has been run
clus_num = 20
cluster.subset = subset(GIS.inventory.cluster, GIS.inventory.cluster$cluster_num  == clus_num)
bad.station = as.character(cluster.subset$commonName)
bad.station

stationName = bad.station
stationName = "USGSGA312853084275101"
station.data = subset(agency.ddata.usgs, subset = agency.ddata.usgs$univName == stationName)
# plot(as.Date(station.data$monthYear, format = "%Y-%m-%d"), station.data$level,
plot(station.data$date,station.data$level,
     main = paste("Groundwater Level:", stationName[1]),
     xlab = "Date",
     ylab = "Level NAVD88, ft",
     type = "p")

## plot the station that shows as an anomalous cluster
stationName = bad.station
stationName = "NWFWMD1846"
station.data = subset(model.domain.data, subset = model.domain.data$commonName == stationName)
plot(as.Date(station.data$monthYear, format = "%m/%d/%Y"), station.data$level,
     main = paste("Groundwater Level:", stationName[1]),
     xlab = "Date",
     ylab = "Level NAVD88, ft",
     type = "p")

## print graphic to file
png(filename= paste(mainDir, "/figures/Bad_station", stationName,".png", sep=""))
plot(as.Date(station.data$date, format = "%Y-%m-%d"), station.data$level,
     main = paste("Groundwater Level:", stationName),
     xlab = "Date",
     ylab = "Level NAVD88, ft")
dev.off()

## to determine the min or max oulier in a dataset
ind = match(station.data$level, max(station.data$level))
outlier = station.data[!is.na(ind),]
outlier

## to edit daily data
station.list = unique(agency.ddata$commonName)
num = 2
station.data = subset.data.frame(x=agency.ddata, commonName == station.list[[num]])
plot(station.data$date, station.data$level)
ind = match(station.data$level, min(station.data$level))
station.data[!is.na(ind),]