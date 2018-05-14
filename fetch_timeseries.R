# Sample data ... fetch data and play
library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(ggplot2)

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
tseries_data <- data.frame(deviceid = (1:nsites),ODIN = "a")
for (i in (1:nsites)){
  tseries_data$deviceid[i] <- jreq1[[i]]$id
  tseries_data$ODIN[i] <- jreq1[[i]]$name
}

# Get the last 600 measurements
base_url <- "https://dashboard.hologram.io/api/1/csr/rdm?"
tseries_data$PM1 <- -1
tseries_data$PM2.5 <- -1
tseries_data$PM10 <- -1
tseries_data$Temperature <- -99
tseries_data$RH <- -1
tseries_data$Timestamp <- as.POSIXct("2018-05-01 00:00:00",tz='UTC')
i <- 1
  built_url <- paste0(base_url,
                      "deviceid=165071&",
                      "limit=600&",
                      "orgid=",secret_hologram$orgid,"&",
                      "apikey=",secret_hologram$apikey)
  req2 <- curl_fetch_memory(built_url)
  jreq2 <- fromJSON(rawToChar(req2$content))$data
  payload <- fromJSON(rawToChar(base64decode(fromJSON(jreq2[[1]]$data)$data)))
  


c_plot <- ggplot(data = curr_data,aes(x=deviceid))+
  geom_bar(aes(y=PM1),stat = StatIdentity) +
  geom_text(aes(y=PM1,label=Timestamp),hjust=0, vjust=0) +
  ylim(0,max(curr_data$PM1))

c_plot



