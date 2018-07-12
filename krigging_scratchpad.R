##### Load relevant packages #####
library(readr)
library(reshape2)
library(automap)
library(raster)
library(gstat)
library(sp)
library(rgdal)
library(ggmap)
library(gstat)
library(ncdf4)
library(RJSONIO)
library(curl)
library(base64enc)
library(zoo)
library(openair)
library(stringi)


##### Set the working directory DB ####
setwd("~/repositories/cona_live/mapping/")
##### Read the credentials file (ignored by GIT repository) ####
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

##### Moving Average function ####
ma <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}
##### Get data ####

# Get the devices ID
base_url <- "https://dashboard.hologram.io/api/1/devices?"
tag <- "cona2018"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "tagname=",tag,"&",
                    "apikey=",secret_hologram$apikey)
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
nsites <- length(jreq1)
curr_data <- data.frame(deviceid = (1:nsites),ODIN = NA)
for (i in (1:nsites)){
  curr_data$deviceid[i] <- jreq1[[i]]$id
  curr_data$ODIN[i] <- jreq1[[i]]$name
}

# Get the latest measurements
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
curr_data$PM1 <- NA
curr_data$PM2.5 <- NA
curr_data$PM10 <- NA
curr_data$Temperature <- NA
curr_data$RH <- NA
curr_data$Timestamp <- as.POSIXct("2018-05-01 00:00:00",tz='UTC')
i <- 1
for (i in (1:nsites)){
  built_url <- paste0(base_url,
                      "deviceid=",curr_data$deviceid[i],"&",
                      "limit=1&",
                      "apikey=",secret_hologram$apikey)
  req2 <- curl_fetch_memory(built_url)
  jreq2 <- fromJSON(rawToChar(req2$content))$data
  xxx <- rawToChar(base64decode(fromJSON(jreq2[[1]]$data)$data))
  x_payload <- try(fromJSON(paste0(stri_split_fixed(xxx,",\"recordtime")[[1]][1],"}")),silent = TRUE)
  if (inherits(x_payload,"try-error")) {
    next
  }
  
  payload <- unlist(x_payload)
  if (length(payload)<5){
    next
  }
  
  curr_data$Timestamp[i] <- as.POSIXct(jreq2[[1]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  curr_data$PM1[i] <- as.numeric(payload[1])
  curr_data$PM2.5[i] <- as.numeric(payload[2])
  curr_data$PM10[i] <- as.numeric(payload[3])
  curr_data$Temperature[i] <- as.numeric(payload[7])
  curr_data$RH[i] <- as.numeric(payload[8])
}

curr_data$delay <- floor(difftime(Sys.time(),curr_data$Timestamp, units = 'secs'))
curr_data$mask <- 0
for (i in (1:nsites)){
  curr_data$mask[i] <- max(as.numeric(curr_data$delay[i] < 120),0.2)
}

# Get devices locations
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
odin_locations <- read_delim("./odin_locations.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
curr_data$lat <- NA
curr_data$lon <- NA
for (i in (1:nsites)){
  loc_id <- which(substr(odin_locations$Serialn,7,11)==substr(curr_data$ODIN[i],6,9))
  p <- project(cbind(odin_locations$Easting[loc_id],odin_locations$Northing[loc_id]),proj = proj4string,inv = TRUE)
  curr_data$lon[i] <- p[1]
  curr_data$lat[i] <- p[2]
}

centre_lat <- mean(curr_data$lat)
centre_lon <- mean(curr_data$lon)

# Now get datafor last 12 hours and calculate average for mapping
# Cycle through each deviceID and calculate the 12hr average up to now
curr_data$PM1 <- NA
curr_data$PM2.5 <- NA
curr_data$PM10 <- NA
curr_data$Temperature <- NA
curr_data$RH <- NA
max_nmeas <- 60*12
ndev <- length(curr_data$deviceid)
# t_start is 12 hours before now
t_start <- floor(as.numeric(Sys.time()-12*3600))

base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
for (i_dev in (1:ndev)){
  built_url <- paste0(base_url,
                      "deviceid=",curr_data$deviceid[i_dev],"&",
                      "limit=",max_nmeas,"&",
                      "timestart=",t_start,"&",
                      "orgid=",secret_hologram$orgid,"&",
                      "apikey=",secret_hologram$apikey)
  req2 <- curl_fetch_memory(built_url)
  jreq2 <- fromJSON(rawToChar(req2$content))$data
  
  ndata <- length(jreq2)
  c_data <- data.frame(id = (1:ndata))
  c_data$PM1 <- NA
  c_data$PM2.5 <- NA
  c_data$PM10 <- NA
  c_data$PMc <- NA
  c_data$GAS1 <- NA
  c_data$Tgas1 <- NA
  c_data$GAS2 <- NA
  c_data$Temperature <- NA
  c_data$RH <- NA
  c_data$date <- as.POSIXct("2018-05-01 00:00:00",tz='UTC')
  c_data$lat <- curr_data$lat[i_dev]
  c_data$lon <- curr_data$lon[i_dev]
  c_data$siteid <- i_dev
  if (ndata < 1){
    next
  }
  for (i in (1:ndata)){
    xxx <- rawToChar(base64decode(fromJSON(jreq2[[i]]$data)$data))
    x_payload <- try(fromJSON(paste0(stri_split_fixed(xxx,",\"recordtime")[[1]][1],"}")),silent = TRUE)
    if (inherits(x_payload,"try-error")) {
      next
    }
    
    payload <- unlist(x_payload)
    if (length(payload)<5){
      next
    }
    # {"PM1":4,"PM2.5":6,"PM10":6,"GAS1":-999,"Tgas1":0,"GAS2":204,"Temperature":7.35,"RH":80.85}
    c_data$PM1[i] <- as.numeric(payload[1])
    c_data$PM2.5[i] <- as.numeric(payload[2])
    c_data$PM10[i] <- as.numeric(payload[3])
    c_data$PMc[i] <- as.numeric(payload[3]) - as.numeric(payload[2])
    c_data$GAS1[i] <- payload[4]
    c_data$Tgas1[i] <- payload[5]
    c_data$GAS2[i] <- payload[6]
    c_data$Temperature[i] <- payload[7]
    c_data$RH[i] <- payload[8]
    c_data$date[i] <- as.POSIXct(jreq2[[i]]$logged,format = "%Y-%m-%d %H:%M:%OS",tz="UTC")
  }
  
  if (i_dev == 1){
    all_data <- c_data
    all_data.10min <- timeAverage(c_data,avg.time = '10 min')
  } else {
    all_data <- rbind(all_data,c_data)
    all_data.10min <- rbind(all_data.10min,timeAverage(c_data,avg.time = '10 min'))
  }
  curr_data$PM1[i_dev] <- mean(c_data$PM1,na.rm = TRUE)
  curr_data$PM2.5[i_dev] <- mean(c_data$PM2.5,na.rm = TRUE)
  curr_data$PM10[i_dev] <- mean(c_data$PM10,na.rm = TRUE)
  curr_data$Temperature[i_dev] <- mean(c_data$Temperature,na.rm = TRUE)
  curr_data$RH[i_dev] <- mean(c_data$RH,na.rm = TRUE)
  rm(c_data)
}
curr_data$Last_reading <- curr_data$Timestamp
curr_data$mask <- as.numeric(curr_data$delay < 120)
reboot_odins <- subset(curr_data,mask == 0)

coordinates(all_data.10min) <- ~ lon + lat
proj4string(all_data.10min) <- CRS('+init=epsg:4326')
# Re-project to NZTM
spTransform(all_data.10min,CRS('+init=epsg:2193'))

print("Starting the kriging")

#Setting the  prediction grid properties
cellsize <- 100 #pixel size in projection units (NZTM, i.e. metres)
min_x <- all_data.10min@bbox[1,1] - cellsize#minimun x coordinate
min_y <- all_data.10min@bbox[2,1] - cellsize #minimun y coordinate
max_x <- all_data.10min@bbox[1,2] + cellsize #mximum x coordinate
max_y <- all_data.10min@bbox[2,2] + cellsize #maximum y coordinate

x_length <- max_x - min_x #easting amplitude
y_length <- max_y - min_y #northing amplitude

ncol <- round(x_length/cellsize,0) #number of columns in grid
nrow <- round(y_length/cellsize,0) #number of rows in grid

grid <- GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))

#Convert GridTopolgy object to SpatialPixelsDataFrame object.
grid <- SpatialPixelsDataFrame(grid,
                               data=data.frame(id=1:prod(ncol,nrow)),
                               proj4string=CRS('+init=epsg:2193'))



all_dates <- sort(unique(all_data.10min$date))
ndates <- length(all_dates)
breaks <- as.numeric(quantile((1:ndates),c(0,0.2,0.4,0.6,0.8,1), type = 1))
nbreaks <- length(breaks)
fidx <- 1
for (j in (1:(nbreaks-1))){
  i <- 0
  if (j == 1){
    j1 <- 1
    j2 <- breaks[j+1]
  } else {
    j1 <- breaks[j]
    j2 <- breaks[j+1]
  }
  for (d_slice in (j1:j2)){
    c_data <- subset(all_data.10min,subset = (date==all_dates[d_slice]))
    
    if (length(unique(c_data$siteid))<4){
      next
    }
    #  surf.krig <- autoKrige(pm2.5 ~ 1,data=c_data,new_data = grid, input_data=c_data)
    #  surf.krig$krige_output$timestamp <-d_slice
    #  proj4string(surf.krig$krige_output) <- CRS('+init=epsg:2193')
    
    surf.idw <- idw(pm2.5 ~ 1,newdata = grid, locations = c_data, idp = 1)
    surf.idw$timestamp <-d_slice
    proj4string(surf.idw) <- CRS('+init=epsg:2193')
    
    surf.idw2 <- idw(pm2.5 ~ 1,newdata = grid, locations = c_data, idp = 2)
    surf.idw2$timestamp <-d_slice
    proj4string(surf.idw2) <- CRS('+init=epsg:2193')
    
    if (i==0){
      #    x_data <- surf.krig$krige_output@data
      #    x_bbox <- surf.krig$krige_output@bbox
      #    x_coords <- surf.krig$krige_output@coords
      #    x_coords.nrs <- c(1,2)
      #    to_rast.krig <- surf.krig$krige_output
      #    r0.krig <- rasterFromXYZ(cbind(to_rast.krig@coords,to_rast.krig@data$var1.pred))
      #    crs(r0.krig) <- '+init=epsg:2193'
      #    raster_cat.krig <- r0.krig
      
      to_rast.idw <- surf.idw
      r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
      crs(r0.idw) <- '+init=epsg:2193'
      raster_cat.idw<- r0.idw
      
      to_rast.idw2 <- surf.idw2
      r0.idw2 <- rasterFromXYZ(cbind(surf.idw2@coords,surf.idw2$var1.pred))
      crs(r0.idw2) <- '+init=epsg:2193'
      raster_cat.idw2<- r0.idw2
      i <- 1
    }
    else {
      #    x_data <- rbind(x_data,surf.krig$krige_output@data)
      #    x_coords <- rbind(x_coords,surf.krig$krige_output@coords)
      #    to_rast.krig <- surf.krig$krige_output
      #    r0.krig <- rasterFromXYZ(cbind(to_rast.krig@coords,to_rast.krig@data$var1.pred))
      #    crs(r0.krig) <- '+init=epsg:2193'
      #    raster_cat.krig <- addLayer(raster_cat.krig,r0.krig)
      
      to_rast.idw <- surf.idw
      r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
      crs(r0.idw) <- '+init=epsg:2193'
      raster_cat.idw<- addLayer(raster_cat.idw,r0.idw)
      
      to_rast.idw2 <- surf.idw2
      r0.idw2 <- rasterFromXYZ(cbind(surf.idw2@coords,surf.idw2$var1.pred))
      crs(r0.idw2) <- '+init=epsg:2193'
      raster_cat.idw2<- addLayer(raster_cat.idw2,r0.idw2)
    }
    #  if (min(to_rast.krig@data$var1.pred)<0){
    print(all_dates[d_slice])
    #  }
  }
  save('raster_cat.idw',file = paste0('/data/data_gustavo/cona/raster_cat.idw.',fidx,'.RData'))
  save('raster_cat.idw2',file = paste0('/data/data_gustavo/cona/raster_cat.idw2.',fidx,'.Rdata'))
  rm('raster_cat.idw')
  rm('raster_cat.idw2')
  fidx <- fidx + 1
}
fidx <- 6
for (i in (1:(fidx-1))){
  load(paste0('/data/data_gustavo/cona/raster_cat.idw.',i,'.RData'))
  load(paste0('/data/data_gustavo/cona/raster_cat.idw2.',i,'.Rdata'))
  if (i == 1){
    raster_stack.idw <- raster_cat.idw
    raster_stack.idw2 <- raster_cat.idw2
  } else {
    raster_stack.idw <- addLayer(raster_stack.idw,raster_cat.idw)
    raster_stack.idw2 <- addLayer(raster_stack.idw2,raster_cat.idw2)
  }
}

#x_bbox[1,] <-c(min(x_coords[,1]),min(x_coords[,2]))
#x_bbox[2,] <-c(max(x_coords[,1]),max(x_coords[,2]))
#krigged_odin_data <- SpatialPointsDataFrame(coords = x_coords, data = x_data, coords.nrs = x_coords.nrs, bbox = x_bbox)
#proj4string(krigged_odin_data) <- CRS('+init=epsg:2193')
#krigged_odin_data <- spTransform(krigged_odin_data,CRS("+proj=longlat +datum=WGS84"))


#writeOGR(krigged_odin_data, ".", "JuneJuly2017_pm25_10_min_krigged", driver = "ESRI Shapefile", overwrite_layer = TRUE)


#save(krigged_odin_data,file='/data/data_gustavo/cona/krigged_data_JuneJuly2017.RData')

#raster_cat_LL <- projectRaster(raster_cat,crs = "+proj=longlat +datum=WGS84")
raster_cat_idw_LL <- projectRaster(raster_stack.idw,crs = "+proj=longlat +datum=WGS84")
raster_cat_idw2_LL <- projectRaster(raster_stack.idw2,crs = "+proj=longlat +datum=WGS84")
save(list = c('raster_cat_idw_LL','raster_cat_idw2_LL'),file = '/data/data_gustavo/cona/raster_odin_IDW_JuneJuly2017.RData')

#writeRaster(raster_cat_LL, filename="./odin_June-July2017_autokrig.nc", overwrite=TRUE)
writeRaster(raster_cat_idw_LL, filename="./odin_June-July2017_idw.nc", overwrite=TRUE)
writeRaster(raster_cat_idw2_LL, filename="./odin_June-July2017_idw2.nc", overwrite=TRUE)
