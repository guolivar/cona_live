#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer({
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  options(shiny.maxRequestSize = 9*1024^2)
  
  function(input, output) {
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      pacman.data <- read.csv(inFile$datapath,
                              ";",
                              escape_double = FALSE,
                              col_names = FALSE,
                              trim_ws = TRUE)
      pacman.summary <- c(mean(pacman.data$X2),
                          mean(pacman.data$X3),
                          mean(pacman.data$X4),
                          mean(pacman.data$X5),
                          mean(pacman.data$X6),
                          mean(pacman.data$X7))
      return(pacman.summary)
    })
  }
}
)
