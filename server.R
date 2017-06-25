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

  ## Filter data
  fcloanbook <- reactive(
    loanbook
  )
  
  ##Summary stats
  sumstats <- reactive(
    summary_stats(fcloanbook())
  )

  ## Plot total lent
  output$plottotal <- renderPlotly(
    plot_by_date(fcloanbook(), 
                             by = "loan_amount", 
                             strat = "credit_band",
                             plotly = TRUE)
  )
  
  ## Amount Lent
  output$amount_lent <- renderInfoBox(
    infoBox("Amount Lent", sumstats() %>% 
              select(amount_lent) %>% 
      convert_million)
  )
  
  ## Amount repaid
  output$repaid <- renderInfoBox(
    infoBox("Amount Repaid", return_with_per(sumstats(), principal_repaid,
                                                amount_lent))
  )
  
  ## Amount defaulted
  output$defaulted <- renderInfoBox(
    infoBox("Amount Defaulted", return_with_per(sumstats(), defaulted,
                                                amount_lent))
  )
  
  ## Amount recovered
  output$recovered <- renderInfoBox(
    infoBox("Amount Recovered", return_with_per(sumstats(), recoveries,
                                                defaulted))
  )
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
