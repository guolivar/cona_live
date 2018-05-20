# Fetch data and create spatial summaries
library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(ggplot2)
library(zoo)
library(openair)
library(sp)
library(raster)

# Read the secrets
secret_hologram <- read_delim("./secret_hologram.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
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
devices <- data.frame(deviceid = (1:nsites),ODIN = NA)
for (i in (1:nsites)){
  devices$deviceid[i] <- jreq1[[i]]$id
  devices$ODIN[i] <- jreq1[[i]]$name
}

# Get devices locations
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
odin_locations <- read_delim("./odin_locations.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
devices$lat <- NA
devices$lon <- NA
for (i in (1:nsites)){
  loc_id <- which(substr(odin_locations$Serialn,7,11)==substr(curr_data$ODIN[i],6,9))
  p <- project(c(odin_locations$Easting[loc_id],odin_locations$Northing[loc_id]),proj = proj4string,inverse = T)
  devices$y <- odin_locations$Northing[loc_id]
  devices$x <- odin_locations$Easting[loc_id]
  devices$lon[i] <- p[1]
  devices$lat[i] <- p[2]
}

# Get the last X measurements
nmeas <- 60*48
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
ndev <- length(devices$deviceid)
for (i_dev in (1:ndev)){
    n_steps <- ceiling(nmeas/1000)
    for (step in (1:n_steps)){
      if (step == 1){
        built_url <- paste0(base_url,
                            "deviceid=",devices$deviceid[i_dev],"&",
                            "limit=",min(nmeas,1000),"&",
                            "timestart=1526249648&",
                            "orgid=",secret_hologram$orgid,"&",
                            "apikey=",secret_hologram$apikey)
        req2 <- curl_fetch_memory(built_url)
        jreq2 <- fromJSON(rawToChar(req2$content))$data
      } else {
        built_url <- paste0(base_url,
                            "deviceid=",devices$deviceid[i_dev],"&",
                            "limit=",min(nmeas-1000*step,1000),"&",
                            "timestart=1526249648&",
                            "startat=",startat,"&",
                            "orgid=",secret_hologram$orgid,"&",
                            "apikey=",secret_hologram$apikey)
        req2 <- curl_fetch_memory(built_url)
        jreq2 <- append(jreq2,fromJSON(rawToChar(req2$content))$data)
      }
      last <- length(jreq2)
      startat <- jreq2[[last]]$id
    }
  
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
  c_data$timestamp <- NA
  
  for (i in (1:ndata)){
    payload <- fromJSON(rawToChar(base64decode(fromJSON(jreq2[[i]]$data)$data)))
    # {"PM1":4,"PM2.5":6,"PM10":6,"GAS1":-999,"Tgas1":0,"GAS2":204,"Temperature":7.35,"RH":80.85}
    c_data$PM1[i] <- payload[1]
    c_data$PM2.5[i] <- payload[2]
    c_data$PM10[i] <- payload[3]
    c_data$PMc[i] <- payload[3] - payload[2]
    c_data$GAS1[i] <- payload[4]
    c_data$Tgas1[i] <- payload[5]
    c_data$GAS2[i] <- payload[6]
    c_data$Temperature[i] <- payload[7]
    c_data$RH[i] <- payload[8]
    c_data$timestamp[i] <- jreq2[[i]]$logged
  }
  c_data$date <- as.POSIXct(c_data$timestamp,tz="UTC")
  if (i_dev == 1){

    all_data.10min <- timeAverage(c_data,avg.time = '10 min')
    all_data.10min$serialn <- devices$ODIN[i_dev]
    all_data.10min$device <- devices$deviceid[i_dev]
    all_data.10min$lat <- devices$lat[i_dev]
    all_data.10min$lon <- devices$lon[i_dev]
    all_data.10min$x <- devices$x[i_dev]
    all_data.10min$y <- devices$y[i_dev]
    
    all_data.1hour <- timeAverage(c_data,avg.time = '1 hour')
    all_data.1hour$serialn <- devices$ODIN[i_dev]
    all_data.1hour$device <- devices$deviceid[i_dev]
    all_data.1hour$lat <- devices$lat[i_dev]
    all_data.1hour$lon <- devices$lon[i_dev]
    all_data.1hour$x <- devices$x[i_dev]
    all_data.1hour$y <- devices$y[i_dev]
    
    c_data$serialn <- devices$ODIN[i_dev]
    c_data$device <- devices$deviceid[i_dev]
    c_data$lat <- devices$lat[i_dev]
    c_data$lon <- devices$lon[i_dev]
    c_data$x <- devices$x[i_dev]
    c_data$y <- devices$y[i_dev]
    all_data <- c_data
    
  } else {

    x_all_data.10min <- timeAverage(c_data,avg.time = '10 min')
    x_all_data.10min$serialn <- devices$ODIN[i_dev]
    x_all_data.10min$device <- devices$deviceid[i_dev]
    x_all_data.10min$lat <- devices$lat[i_dev]
    x_all_data.10min$lon <- devices$lon[i_dev]
    x_all_data.10min$x <- devices$x[i_dev]
    x_all_data.10min$y <- devices$y[i_dev]
    all_data.10min <- rbind(all_data.10min,x_all_data.10min)
    
    x_all_data.1hour <- timeAverage(c_data,avg.time = '1 hour')
    x_all_data.1hour$serialn <- devices$ODIN[i_dev]
    x_all_data.1hour$device <- devices$deviceid[i_dev]
    x_all_data.1hour$lat <- devices$lat[i_dev]
    x_all_data.1hour$lon <- devices$lon[i_dev]
    x_all_data.1hour$x <- devices$x[i_dev]
    x_all_data.1hour$y <- devices$y[i_dev]
    all_data.1hour <- rbind(all_data.1hour,x_all_data.1hour)
    
    c_data$serialn <- devices$ODIN[i_dev]
    c_data$device <- devices$deviceid[i_dev]
    c_data$lat <- devices$lat[i_dev]
    c_data$lon <- devices$lon[i_dev]
    c_data$x <- devices$x[i_dev]
    c_data$y <- devices$y[i_dev]
    all_data <- rbind(all_data,c_data)
  }
  rm(c_data)
}

# Add coordinate information to the data frames
coordinates(all_data.1hour) <- ~ x + y
proj4string(all_data.1hour) <- CRS('+init=epsg:2193')

coordinates(all_data.10min) <- ~ x + y
proj4string(all_data.10min) <- CRS('+init=epsg:2193')

coordinates(all_data) <- ~ x + y
proj4string(all_data) <- CRS('+init=epsg:2193')

ggplot(data = subset(all_data,date > as.POSIXct("2018-05-16 00:00:00",tz="UTC")),aes(x=date)) +
  geom_line(aes(y=rollmean(PM2.5,60,fill = NA),colour=serialn))

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
    
    if (length(unique(c_data$serialn))<4){
      next
    }

    surf.idw <- idw(pm2.5 ~ 1,newdata = grid, locations = c_data, idp = 1)
    surf.idw$timestamp <-d_slice
    proj4string(surf.idw) <- CRS('+init=epsg:2193')
    
    surf.idw2 <- idw(pm2.5 ~ 1,newdata = grid, locations = c_data, idp = 2)
    surf.idw2$timestamp <-d_slice
    proj4string(surf.idw2) <- CRS('+init=epsg:2193')
    
    if (i==0){

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
      
      to_rast.idw <- surf.idw
      r0.idw <- rasterFromXYZ(cbind(surf.idw@coords,surf.idw$var1.pred))
      crs(r0.idw) <- '+init=epsg:2193'
      raster_cat.idw<- addLayer(raster_cat.idw,r0.idw)
      
      to_rast.idw2 <- surf.idw2
      r0.idw2 <- rasterFromXYZ(cbind(surf.idw2@coords,surf.idw2$var1.pred))
      crs(r0.idw2) <- '+init=epsg:2193'
      raster_cat.idw2<- addLayer(raster_cat.idw2,r0.idw2)
    }
    print(all_dates[d_slice])
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


raster_cat_idw_LL <- projectRaster(raster_stack.idw,crs = "+proj=longlat +datum=WGS84")
raster_cat_idw2_LL <- projectRaster(raster_stack.idw2,crs = "+proj=longlat +datum=WGS84")
save(list = c('raster_cat_idw_LL','raster_cat_idw2_LL'),file = '/data/data_gustavo/cona/raster_odin_IDW_CONA2018.RData')

writeRaster(raster_cat_idw_LL, filename="./odin_CONA2018_idw.nc", overwrite=TRUE)
writeRaster(raster_cat_idw2_LL, filename="./odin_CONA2018_idw2.nc", overwrite=TRUE)


