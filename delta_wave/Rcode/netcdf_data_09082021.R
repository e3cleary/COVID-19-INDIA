#install.packages('ncdf4')
#install.packages('PCICt')
#install.packages('RNetCDF')

library(ncdf4)
library(PCICt)
library(RNetCDF)
library(dplyr)
library(data.table)
library(tidyr)
library(reshape2)
library(stringr)
library(raster)
library(rgdal)
library(sp)


nc <- nc_open("~/Documents/adaptor.mars.internal-1626354093.9817944-6733-5-679df68d-a2c9-45a5-b126-3188ec115c42.nc", write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
print(nc)

lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
station <- expand.grid(lon, lat) 
colnames(station) <- c("Lon", "Lat")
station$StationName <- paste(station$Lon, station$Lat, sep = "_")
head(station)

iTime <- ncvar_get(nc, "time")
startDate <- "1900-01-01 00:00:0.0";
fTime <- as.PCICt(iTime*3600, cal = "gregorian", origin = startDate); 
targetTime <- format(fTime, "%m")

out <- as.data.frame(matrix(nrow=length(targetTime), ncol = length(station$StationName) + 1))                         
colnames(out) <- c("Time", station$StationName)
out$Time <- format(fTime, "%m/%d/%Y %H:%M")

# 2 metre temperature
var <- ncvar_get(nc, "t2m") 
var <- (var - 273.15) # Covert to Degree

# 10 metre U wind component
var <- ncvar_get(nc, "u10") 

# 10 metre V wind component
var <- ncvar_get(nc, "v10") 

# total precipitation
var <- ncvar_get(nc, "tp") 
var <- (var*1000) # Covert to mm

# Downward UV radiation at the surface
var <- ncvar_get(nc, "uvb") 

input <- var
# Write variables in data frame
for (i in 1:length(station$StationName)) {
  out[, station$StationName[i]] <- input[lon == station$Lon[i], lat == station$Lat[i] , ]
}

#print(input)

inData<-out
uniqueDates <- unique(as.Date(inData[, 1], format = "%m/%d/%Y %R"));
stationNames <- colnames(inData)[-1];
outVar <- data.frame(matrix(nrow = length(uniqueDates), ncol = length(stationNames) + 1));
colnames(outVar) <- c("Time", stationNames);
outVar$Time <- uniqueDates;
for (uDate in 1:length(uniqueDates)) {
  outVar[uDate, stationNames] <- apply(inData[as.Date(inData$Time, format = "%m/%d/%Y %R") == uniqueDates[uDate], stationNames], MARGIN = 2, FUN = mean, na.rm = TRUE);
}
#head(outVar[,1:4])

#print(outVar)

#Create daily spatial data
df<-t(outVar)
my.names <- df[1,]
colnames(df) <- my.names
df <- cbind(Log_Lat = rownames(df), df)
rownames(df) <- 1:nrow(df)
mf<-as.data.frame(df)
mf = mf[-1,]
lon_lat<-mf[,1]
lon_lat<-str_split_fixed(lon_lat, "_", 2)
lon_lat.df<-as.data.frame(lon_lat)
colnames(lon_lat.df) <- c("lon","lat")
data<-mf[2:91] # January to March 2021
#data<-mf[2:366] # One year
#data<-mf[2:732] # Two years
data_xy<-cbind(lon_lat.df,data)
my.data <- mutate_all(data_xy, function(x) as.numeric(as.character(x)))


write.csv(my.data,"~/Documents/t2m_2019_to_2021_data.csv")
write.csv(my.data,"~/Documents/u10_2019_to_2021_data.csv")
write.csv(my.data,"~/Documents/v10_2019_to_2021_data.csv")
write.csv(my.data,"~/Documents/tp_2019_to_2021_data.csv")
write.csv(my.data,"~/Documents//uv_2019_to_2021_data.csv")


