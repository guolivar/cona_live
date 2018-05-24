# Fetch data from 1 site
library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(ggplot2)
library(zoo)
library(openair)

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

# Choose ODIN
i_dev <- grep(x=devices$ODIN,pattern = '0012')
# Get the last 1000 measurements

base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
        built_url <- paste0(base_url,
                            "deviceid=",devices$deviceid[i_dev],"&",
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
  rm(c_data)
  
  timePlot(all_data,pollutant = 'Temperature')
