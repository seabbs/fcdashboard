#Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(rmarkdown)
library(caret)
library(ggfortify)
library(plotly)
library(lubridate)
library(wrapr)


## Source cleaned data
source("clean_fc_loanbook.R")

## Source utility functions
source("utility_functions.R")

## Source functions for personal loanbook
source("personal_loanbook.R")

## Stop spurious warnings
options(warn = -1)

shinyServer(function(input, output) {

  ## Filter data
  fc_loanbook <- reactive(
    loanbook %>% filter(loan_accepted_date >= input$dates[1],
                        loan_accepted_date <= input$dates[2])
  )
  
  combined_loanbook <- reactive(
    personal_loanbook %>%
      bind_loanbooks(loanbook, verbose = TRUE)  %>% 
      filter(loan_accepted_date >= input$dates[1],
             loan_accepted_date <= input$dates[2])
  )
  
  p_loanbook <- reactive(
    combined_loanbook() %>% 
      filter(invested_in %in% "Yes")
  )
  
  cleaned_p_loanbook <- reactive(
    p_loanbook() %>% 
      rename(`Loan ID` = id) %>% 
      rename( Region = region_name,
             `Repayment type` = repayment_type,
             `Security taken` = security_taken,
             `Loan term` = term) %>% 
      select(`Loan ID`, `Loan title`, `Sector`,
             `Number of loan parts`, Risk, `Repayments left`,
             `Principal remaining`, Rate, `Next payment date`, 
             `Loan status`, `Loan term`, Region, `Repayment type`, `Security taken`)
  )
  
  
  ##Summary stats
  fc_sumstats <- reactive(
    summary_stats(fc_loanbook())
  )

  ##Summary stats
  p_sumstats <- reactive(
    summary_stats(p_loanbook())
  )
  
  ## Exploratory plots
  
  ## Plot total lent by time
  output$fc_plottotal <- renderPlotly(
    if (input$fc_yaxis %in% "defaulted") {
      fc_loanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
      plot_by_date( 
                   by = "defaulted", 
                   strat = input$fc_strat_var,
                   plotly = TRUE)
    }else{
      plot_by_date(fc_loanbook(), 
                   by = input$fc_yaxis, 
                   strat = input$fc_strat_var,
                   plotly = TRUE)
    }

  )
  
  output$p_plottotal <- renderPlotly(
    if (input$p_yaxis %in% "defaulted") {
      p_loanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
        plot_by_date( 
          by = "defaulted", 
          strat = input$p_strat_var,
          plotly = TRUE)
    }else{
      plot_by_date(p_loanbook(), 
                   by = input$p_yaxis, 
                   strat = input$p_strat_var,
                   plotly = TRUE)
    }
    
  )
  
  ## Plot amount as violin
  output$fc_plotdist <- renderPlotly(
    if (input$fc_yaxis %in% "defaulted") {
      fc_loanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
        plot_dist( 
          by = "defaulted", 
          strat = input$fc_strat_var,
          plotly = TRUE)
    }else{
      plot_dist(fc_loanbook(), 
                   by = input$fc_yaxis, 
                   strat = input$fc_strat_var,
                   plotly = TRUE)
    }
    
  )
  
  output$p_plotdist <- renderPlotly(
    if (input$p_yaxis %in% "defaulted") {
      p_loanbook() %>% 
        filter(status %in% "defaulted") %>% 
        mutate(defaulted = principal_remaining) %>% 
        plot_dist( 
          by = "defaulted", 
          strat = input$p_strat_var,
          plotly = TRUE)
    }else{
      plot_dist(p_loanbook(), 
                by = input$p_yaxis, 
                strat = input$p_strat_var,
                plotly = TRUE)
    }
    
  )
  
  ## Plot variable scatter
  output$fc_plotscatter <- renderPlotly(
plot_scatter(fc_loanbook(), 
                 by = input$fc_yaxis, 
                 also_by = input$fc_com_var,
                 strat = input$fc_strat_var,
                 plotly = TRUE)
  )

  
  output$p_plotscatter <- renderPlotly(
    plot_scatter(p_loanbook(), 
                 by = input$p_yaxis, 
                 also_by = input$p_com_var,
                 strat = input$p_strat_var,
                 plotly = TRUE,
                 alpha = 0.8)
  )
  
  ## Amount Lent
  output$fc_amount_lent <- renderInfoBox(
    infoBox("Amount Lent", fc_sumstats() %>% 
              select(amount_lent) %>% 
      convert_million,
      color = "black")
  )
  
  output$p_amount_lent <- renderInfoBox(
    infoBox("Amount Lent", p_sumstats() %>% 
              select(amount_lent) %>% 
              convert_million,
            color = "black")
  )
  
  ## Amount repaid
  output$fc_repaid <- renderInfoBox(
    infoBox("Amount Repaid", return_with_per(fc_sumstats(), principal_repaid,
                                                amount_lent),
            color = "black")
  )
  
  output$p_repaid <- renderInfoBox(
    infoBox("Amount Repaid", return_with_per(p_sumstats(), principal_repaid,
                                             amount_lent),
            color = "black")
  )
  
  ## Amount defaulted
  output$fc_defaulted <- renderInfoBox(
    infoBox("Amount Defaulted", return_with_per(fc_sumstats(), defaulted,
                                                amount_lent),
            color = "black")
  )
 
  output$p_defaulted <- renderInfoBox(
    infoBox("Amount Defaulted", return_with_per(p_sumstats(), defaulted,
                                                amount_lent),
            color = "black")
  ) 
  
  ## Amount recovered
  output$fc_recovered <- renderInfoBox(
    infoBox("Amount Recovered", return_with_per(fc_sumstats(), recoveries,
                                                defaulted), 
            color = "black")
  )
  
  ## Amount recovered
  output$p_recovered <- renderInfoBox(
    infoBox("Amount Recovered", return_with_per(p_sumstats(), recoveries,
                                                defaulted), 
            color = "black")
  )
  
  ##PCA
  fc_pca <- reactive(
    fc_loanbook() %>% 
      pca_on_loanbook(no_pca = input$fc_no_pca)
  )
  
  p_pca <- reactive(
    p_loanbook() %>% 
      pca_on_loanbook(no_pca = input$p_no_pca)
  )
  
  ## plot pca
  output$plot_fc_pca <- renderPlotly(
    plot_pca(fc_pca(), 
             pc_1 = input$fc_pca_1, 
             pc_2 = input$fc_pca_2,
             strat = input$fc_strat_var2, 
             plotly = TRUE)
  )
  
  ## plot pca
  output$plot_p_pca <- renderPlotly(
    plot_pca(p_pca(), 
             pc_1 = input$p_pca_1, 
             pc_2 = input$p_pca_2,
             strat = input$p_strat_var2, 
             plotly = TRUE,
             alpha = 0.8)
  )
  
  ## Personal Dashboard
  
  ## Raw data table
  output$p_loanbook_table <- renderDataTable(
    cleaned_p_loanbook(),
    options = list(
      pageLength = 5)
  )
  
  ## Summarised data table
  output$p_loanbook_sum_table <- renderDataTable(
    cleaned_p_loanbook() %>% 
      p_loanbook_sum_table(strat = input$p_dash_strat)
  )
  
  
  ## Downloads from scripts
  ## Set up downloadable scripts
  output$downloadData0 <- downloadHandler(filename = "clean_fc_loanbook.R",
                                          content = function(file) {
                                            file.copy("clean_fc_loanbook.R", file, overwrite = TRUE)
                                          }
  )
  output$downloadData1 <- downloadHandler(filename = "utility_functions.R",
                                          content = function(file) {
                                            file.copy("utility_functions.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData2 <- downloadHandler(filename = "personal_loanbook.R",
                                          content = function(file) {
                                            file.copy("personal_loanbook.R", file, overwrite = TRUE)
                                          }
  )
  output$downloadData3 <- downloadHandler(filename = "ui.R",
                                          content = function(file) {
                                            file.copy("ui.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData4 <- downloadHandler(filename = "server.R",
                                          content = function(file) {
                                            file.copy("server.R", file, overwrite = TRUE)
                                            }
                                          )
  
})
