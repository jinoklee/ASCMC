library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(shinycssloaders)
library(readr)
library(htmlwidgets)
library(shinyWidgets)
library(stringr)
library(shinyBS)
library(shinyjs)
library(png)
library(shinydashboard)
library(dqshiny)
library(RColorBrewer)


setwd("/data11/jinoklee/ASCMC/")

source("app/ui.R")
source("app/server.R")

  
shinyApp(ui = ui, server = server)
