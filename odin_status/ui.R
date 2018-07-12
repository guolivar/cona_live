#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(ggplot2)
library(proj4)
library(maps)
library(mapproj)
library(ggmap)
library(stringi)



# Define UI for application that draws a histogram
pageWithSidebar(
  headerPanel('Summary of ODIN'),
  sidebarPanel('Non-working ODIN',
    DT::dataTableOutput("table")
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


