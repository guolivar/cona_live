#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(RJSONIO)
library(curl)
library(base64enc)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    # Get last 50 PM1 readings
    req1 <- curl_fetch_memory("https://dashboard.hologram.io/api/1/csr/rdm?deviceid=162382&limit=50&apikey=5h9fH1YlmsWp3SW2L6IdP8c7DefyB3")
    jreq <- fromJSON(rawToChar(req1$content))$data
    xdata <- as.data.frame(matrix(1,nrow = length(jreq),ncol = 9))
    names(xdata) <- c('timestamp','PM1','PM2.5','PM10','GAS1','Tgas1','GAS2','Temperature','RH')
    xdata$PM1 <- 
    for (i in (1:length(jreq))){
      jjreq <- fromJSON(jreq[[i]]$data)
      xdata$timestamp[i] <- jjreq$received
      payload <- fromJSON(rawToChar(base64decode(jjreq$data)))
      xdata[i,] <- c(jjreq$received,(payload))
    }
    
    x    <- as.numeric(xdata$PM2.5)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
