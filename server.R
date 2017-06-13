#Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)

## Source cleaned data
source("dataclean.R")

## Source functions 
source("fcdashboard.R")

## Stop spurious warnings
options(warn = - 1)

shinyServer(function(input, output) {


  ## Set up downloadable scripts
  output$downloadData0 <- downloadHandler(filename = "dataclean.R",
                                          content = function(file) {
                                            file.copy("dataclean.R", file, overwrite = TRUE)
                                          }
  )
  output$downloadData1 <- downloadHandler(filename = "fcdashboard.R",
                                          content = function(file) {
                                            file.copy("fcdashboard.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData2 <- downloadHandler(filename = "ui.R",
                                          content = function(file) {
                                            file.copy("ui.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData3 <- downloadHandler(filename = "server.R",
                                          content = function(file) {
                                            file.copy("server.R", file, overwrite = TRUE)
                                            }
                                          )
  
})
