#Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(rmarkdown)
library(caret)
library(ggfortify)
library(plotly)
library(lubridate)
library(wrapr)
library(stringr)

## Source cleaned data
source("clean_fc_loanbook.R")

## Source utility functions
source("utility_functions.R")

## Source functions for personal loanbook
source("personal_loanbook.R")

## Stop spurious warnings
options(warn = -1)

## Increase upload limit
options(shiny.maxRequestSize=10*1024^2) 

shinyServer(function(input, output) {

  ## Clean/Load FC data
  clean_fc_loanbook <- reactive(
    load_clean_loanbook(input$loanbook)
  )
  
  ##Clean/Load personal data
  clean_personal_loanbook <- reactive(
    loan_clean_personal_loanbook(input$personal_loanbook)
  )
  
  ## Set up reactive filtering slider
  output$date_slider <- renderUI({
    sliderInput(inputId = 'dates', 
                label = 'Time Range',
                min = min(clean_fc_loanbook()$loan_accepted_date),
                max = max(clean_fc_loanbook()$loan_accepted_date),
                value = range(clean_fc_loanbook()$loan_accepted_date),
                timeFormat="%b %Y")
    })

  ## Filter data
  fc_loanbook <- reactive(
    clean_fc_loanbook() %>% filter(loan_accepted_date >= input$dates[1],
                        loan_accepted_date <= input$dates[2])
  )
  
  combined_loanbook <- reactive(
    clean_personal_loanbook() %>%
      bind_loanbooks(fc_loanbook(), verbose = TRUE)  %>% 
      filter(loan_accepted_date >= input$dates[1],
             loan_accepted_date <= input$dates[2])
  )
  
  p_loanbook <- reactive(
    combined_loanbook() %>% 
      filter(invested_in %in% "Yes")
  )
  
  ## Clean loan book for dashboard: note repaid loans are dropped here

  ## This means that the dashboard represents your loanbook as it is now
  cleaned_p_loanbook <- reactive({
clean_loanbook <- p_loanbook() %>% 
      rename(`Loan ID` = id) %>% 
      rename( Region = region_name,
             `Repayment type` = repayment_type,
             `Security taken` = security_taken,
             `Loan term` = term,
             `Loan purpose` = loan_purpose) %>% 
      select(`Loan ID`, `Loan title`, `Sector`,
             `Number of loan parts`, Risk, `Loan status`,
             `Repayments made`, `Repayments left`, `Percentage repaid`,
             `Principal remaining`, Rate, `Next payment date`,
             `Loan term`, `Loan purpose`, Region, `Repayment type`, 
             `Security taken`)

if(input$filter_repaid) {
  clean_loanbook <-  clean_loanbook  %>% 
    filter(!`Loan status` %in% "Repaid")
}else{
  clean_loanbook <- clean_loanbook
}

})
  
  ## Set up reactive filtering variable
  output$filter_var_picker <- renderUI({
    
    choices <- cleaned_p_loanbook() %>% 
      pull(`Loan status`) %>% 
      unique %>% 
      as.list
    
    pickerInput(
      inputId = "p_dash_filt_var", 
      label = "Select/deselect all options", 
      choices = choices, options = list(`actions-box` = TRUE), 
      multiple = TRUE
    )
  })
  
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
  
  ## Loanbook overview
  output$p_loanbook_overview <- renderTable(
    p_loanbook_overall_sum_info(cleaned_p_loanbook(),
                                aplus_bad = input$aplus_bad,
                                a_bad = as.double(input$a_bad),
                                b_bad = as.double(input$b_bad),
                                c_bad = as.double(input$c_bad),
                                d_bad = as.double(input$d_bad),
                                e_bad = as.double(input$e_bad))
  )
  
  ## Raw data table
  output$p_loanbook_table <- DT::renderDataTable(
    cleaned_p_loanbook(),
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      scrollY = TRUE,
      rownames = FALSE)
  )
  
  ## Summarised personal loanbook
  p_sum_tab <- reactive(
    cleaned_p_loanbook() %>% 
      p_loanbook_sum_table(strat = input$p_dash_strat)
  )
  ##Summarise facet personalised loanbook
  p_sum_tab_strat <- reactive({
    if(input$p_dash_facet %in%  "no_facet") {
      strat <- input$p_dash_strat
    }else {
      strat <- c(input$p_dash_strat, input$p_dash_facet)
    }
    
    cleaned_p_loanbook() %>% 
      p_loanbook_sum_table(strat = strat)
  })
  
  ## Summarised data table
  output$p_loanbook_sum_table <- DT::renderDataTable(
    p_sum_tab_strat(),
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      scrollY = TRUE,
      orderClasses = TRUE,
      rownames = FALSE)
  )
  
  ## Summary plots of loan book make up
  output$p_loanbook_sum_plot_amount <- renderPlotly(
    p_sum_tab_strat() %>%
      plot_p_loanbook_summary(yvar = "`Amount lent (£)`", 
                              strat = input$p_dash_strat,
                              facet = input$p_dash_facet,
                              plotly = TRUE)
  )
  
  output$p_loanbook_sum_plot_no <- renderPlotly(
    p_sum_tab_strat() %>%  
      plot_p_loanbook_summary(yvar = "`Number of loan parts`", 
                              strat = input$p_dash_strat,
                              facet = input$p_dash_facet,
                              plotly = TRUE)
  )
  
  output$p_loanbook_sum_plot_per <- renderPlotly(
    p_sum_tab_strat() %>% 
      plot_p_loanbook_summary(yvar = "`Percentage of loanbook (%)`", 
                              strat = input$p_dash_strat,
                              facet = input$p_dash_facet,
                              plotly = TRUE)
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
