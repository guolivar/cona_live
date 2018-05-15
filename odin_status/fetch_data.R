# Sample data ... fetch data and play
library(tidyverse)
library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(ggplot2)
library(proj4)
library(maps)
library(mapproj)
library(ggmap)
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
curr_data <- data.frame(deviceid = (1:nsites),ODIN = NA)
for (i in (1:nsites)){
  curr_data$deviceid[i] <- jreq1[[i]]$id
  curr_data$ODIN[i] <- jreq1[[i]]$name
}

# Get the latest measurements
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
curr_data$PM1 <- -1
curr_data$PM2.5 <- -1
curr_data$PM10 <- -1
curr_data$Temperature <- -99
curr_data$RH <- -1
curr_data$Timestamp <- as.POSIXct("2018-05-01 00:00:00",tz='UTC')
i <- 1
for (i in (1:nsites)){
  built_url <- paste0(base_url,
                      "deviceid=",curr_data$deviceid[i],"&",
                      "limit=1&",
                      "orgid=",secret_hologram$orgid,"&",
                      "apikey=",secret_hologram$apikey)
  req2 <- curl_fetch_memory(built_url)
  jreq2 <- fromJSON(rawToChar(req2$content))$data
  payload <- fromJSON(rawToChar(base64decode(fromJSON(jreq2[[1]]$data)$data)))
  curr_data$Timestamp[i] <- as.POSIXct(jreq2[[1]]$logged,tz='UTC')
  curr_data$PM1[i] <- payload[1]
  curr_data$PM2.5[i] <- payload[2]
  curr_data$PM10[i] <- payload[3]
  curr_data$Temperature[i] <- payload[7]
  curr_data$RH[i] <- payload[8]
}

curr_data$delay <- Sys.time() - curr_data$Timestamp

# Get devices locations
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
odin_locations <- read_delim("./odin_locations.txt", 
           "\t", escape_double = FALSE, trim_ws = TRUE)
curr_data$lat <- NA
curr_data$lon <- NA
for (i in (1:nsites)){
  loc_id <- which(substr(odin_locations$Serialn,7,11)==substr(curr_data$ODIN[i],6,9))
  p <- project(c(odin_locations$Easting[loc_id],odin_locations$Northing[loc_id]),proj = proj4string,inverse = T)
  curr_data$lon[i] <- p[1]
  curr_data$lat[i] <- p[2]
}

centre_lat <- mean(curr_data$lat)
centre_lon <- mean(curr_data$lon)
cmap <- get_map(c(centre_lon,centre_lat),zoom=15)
ggmap(cmap) +
  geom_point(data = curr_data,
             aes(x=lon,y=lat,colour=as.numeric(PM2.5)),
             alpha=(1-as.numeric(curr_data$delay)/max(as.numeric(curr_data$delay))),
             size=20)

