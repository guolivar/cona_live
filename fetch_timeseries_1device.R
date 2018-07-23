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
tag <- "development"
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
i_dev <- grep(x=devices$ODIN,pattern = 'Dev-1')
# Get the last 1000 measurements
nmeas <- 10000
n_steps <- ceiling(nmeas/1000)
for (step in (1:n_steps)){
  if (step == 1){
    built_url <- paste0(base_url,
                        "deviceid=",devices$deviceid[i_dev],"&",
                        "limit=",min(nmeas,1000),"&",
                        "timestart=1525132800&",
                        "orgid=",secret_hologram$orgid,"&",
                        "apikey=",secret_hologram$apikey)
    req2 <- curl_fetch_memory(built_url)
    jreq2 <- fromJSON(rawToChar(req2$content))$data
  } else {
    built_url <- paste0(base_url,
                        "deviceid=",devices$deviceid[i_dev],"&",
                        "limit=",min(nmeas-1000*step,1000),"&",
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
if (i_dev == 1){
  all_data <- c_data
  all_data.10min <- timeAverage(c_data,avg.time = '10 min')
} else {
  all_data <- rbind(all_data,c_data)
  all_data.10min <- rbind(all_data.10min,timeAverage(c_data,avg.time = '10 min'))
}
rm(c_data)

  timePlot(all_data,pollutant = 'PM10',main = devices$ODIN[i_dev])
  plot(all_data$date[1:(i-2)],all_data$date[2:(i-1)] - all_data$date[1:(i-2)],main = devices$ODIN[i_dev])
  