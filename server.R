#Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(caret)
library(ggfortify)


## Source cleaned data
source("dataclean.R")

## Source functions 
source("fcdashboard.R")

## Stop spurious warnings
options(warn = - 1)

shinyServer(function(input, output) {

  ## Filter data
  fcloanbook <- reactive(
    loanbook %>% filter(loan_accepted_date >= input$dates[1],
                        loan_accepted_date <= input$dates[2])
  )
  
  ##Summary stats
  sumstats <- reactive(
    summary_stats(fcloanbook())
  )

  ## FC dashboard
  
  ## Plot total lent by time
  output$plottotal <- renderPlotly(
    if (input$yaxis %in% "defaulted") {
      fcloanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
      plot_by_date( 
                   by = "defaulted", 
                   strat = input$strat_var,
                   plotly = TRUE)
    }else{
      plot_by_date(fcloanbook(), 
                   by = input$yaxis, 
                   strat = input$strat_var,
                   plotly = TRUE)
    }

  )
  
  ## Plot amount as violin
  output$plotdist <- renderPlotly(
    if (input$yaxis %in% "defaulted") {
      fcloanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
        plot_dist( 
          by = "defaulted", 
          strat = input$strat_var,
          plotly = TRUE)
    }else{
      plot_dist(fcloanbook(), 
                   by = input$yaxis, 
                   strat = input$strat_var,
                   plotly = TRUE)
    }
    
  )
  
  ## Plot variable scatter
  output$plotscatter <- renderPlotly(
plot_scatter(fcloanbook(), 
                 by = input$yaxis, 
                 also_by = input$com_var,
                 strat = input$strat_var,
                 plotly = TRUE)
  )
  
  ## Amount Lent
  output$amount_lent <- renderInfoBox(
    infoBox("Amount Lent", sumstats() %>% 
              select(amount_lent) %>% 
      convert_million,
      color = "black")
  )
  
  ## Amount repaid
  output$repaid <- renderInfoBox(
    infoBox("Amount Repaid", return_with_per(sumstats(), principal_repaid,
                                                amount_lent),
            color = "black")
  )
  
  ## Amount defaulted
  output$defaulted <- renderInfoBox(
    infoBox("Amount Defaulted", return_with_per(sumstats(), defaulted,
                                                amount_lent),
            color = "black")
  )
  
  ## Amount recovered
  output$recovered <- renderInfoBox(
    infoBox("Amount Recovered", return_with_per(sumstats(), recoveries,
                                                defaulted), 
            color = "black")
  )
  
  ##PCA
  pca <- reactive(
    fcloanbook() %>% 
      pca_on_loanbook(no_pca = input$no_pca)
  )
  
  ## plot pca
  output$plotpca <- renderPlotly(
    plot_pca(pca(), 
             pc_1 = input$pca_1, 
             pc_2 = input$pca_2,
             strat = input$strat_var2, 
             plotly = TRUE)
  )
  
  ## Downloads from scripts
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
