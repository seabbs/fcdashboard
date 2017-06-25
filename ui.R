## Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(rmarkdown)
library(caret)
library(ggfortify)

## Source cleaned data
source("dataclean.R")

## Source functions 
source("fcdashboard.R")

## Slider in menu bar to control dates of data shown
## Buttons showing summary data at bottom
## two summary graphs in main dashboard

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "menu",
              menuItem("Dashboard", tabName = "dashboard", icon=icon("dashboard"), selected=TRUE),
              menuItem("PCA", tabName = "pca", icon = icon("line-chart")),
              menuItem("About", tabName = "readme", icon=icon("info")),
              menuItem("Code",  icon = icon("code"),
                       menuSubItem("Github", href = "https://github.com/seabbs/fcdashboard", icon = icon("github")),
                       menuSubItem("dataclean.R", tabName = "dataclean", icon = icon("angle-right")),
                       menuSubItem("fcdashboard.R", tabName = "fcdashboard", icon = icon("angle-right")),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              )
  ),
  hr(),
  tipify(sliderInput(inputId = 'dates', 
                     label = 'Time Range',
                     min = min(loanbook$loan_accepted_date),
                     max = max(loanbook$loan_accepted_date),
                     value = range(loanbook$loan_accepted_date),
                     timeFormat="%b %Y"),
         title = "Select the data range. Data out of date? Send me a tweet."),
  conditionalPanel(condition = 'input.menu == "dashboard"',
selectInput("yaxis", 
                               "Variable to summarise:",
                               list(`Loan amount` = 
                                      "loan_amount",
                                    Recoveries = 
                                      "recoveries",
                                    `Principal remaining` = 
                                      "principal_remaining",
                                    Defaulted = "defaulted"
                               )
                   ),
                   selectInput("strat_var", 
                               "Variable to stratify by:",
                               list(`Credit band` = 
                                      "credit_band",
                                    Status = 
                                      "status",
                                    `Loan purpose` = 
                                      "loan_purpose",
                                    Sector = "sector",
                                    Region = "region_name",
                                    `Loan term` = "term",
                                    `Whole loan` = "whole_loan",
                                    `Repayment type` = "repayment_type",
                                    `Security taken` = "security_taken"
                               )
                               
                   ),
                   selectInput("com_var", 
                               "Variable to compare against:",
                               list(`Interest rate` = "interest_rate",
                                    `Loan amount` = 
                                      "loan_amount",
                                    Recoveries = 
                                      "recoveries",
                                    `Principal remaining` = 
                                      "principal_remaining",
                                    Defaulted = "defaulted",
                                    `Payments remaining` = "payments_remaining",
                                    `Term` = "term"
                               ))),
conditionalPanel(condition = 'input.menu == "pca"',
                 sliderInput(inputId = "no_pca", 
                             label = "Number of Principal components:",
                             min = 0,
                             max = 10,
                             value = 2),
                 sliderInput(inputId = "pca_1", 
                             label = "First component to plot:",
                             min = 0,
                             max = 10,
                             value = 1),
                 sliderInput(inputId = "pca_2", 
                             label = "First component to plot:",
                             min = 0,
                             max = 10,
                             value = 2),
                 selectInput("strat_var2", 
                             "Variable to stratify by:",
                             list(`Credit band` = 
                                    "credit_band",
                                  Status = 
                                    "status",
                                  `Loan purpose` = 
                                    "loan_purpose",
                                  Sector = "sector",
                                  Region = "region_name",
                                  `Loan term` = "term",
                                  `Whole loan` = "whole_loan",
                                  `Repayment type` = "repayment_type",
                                  `Security taken` = "security_taken"
                             ))),
  helpText("Developed by ", 
           a("Sam Abbott", href="http://samabbott.co.uk"), ".",
           style="padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.md")
    ),
    tabItem(tabName = "dashboard",
            fluidRow(
              tags$head(includeScript("google-analytics.js")),
              tabBox( width = 12,
                      title = "Summary Plots",
                      side = "right",
                      tabPanel(title = "By Year",
                               plotlyOutput("plottotal")),
                      tabPanel(title = "By Stratified Variable",
                               plotlyOutput("plotdist")),
                      tabPanel(title = "Variable vs. Variable",
                               plotlyOutput("plotscatter"))),
            infoBoxOutput("amount_lent"),
            infoBoxOutput("repaid"),
            infoBoxOutput("defaulted"),
            infoBoxOutput("recovered")
    )
    ),
    tabItem(tabName = "pca",
            fluidRow(
              tabBox( width = 12,
                      title = "Principal Components",
                      side = "right",
                      tabPanel(title = "Scatter",
                               plotlyOutput("plotpca", height = "100%"))
              )
            )
            ),
    tabItem(tabName = "dataclean",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Data Cleaning",                
                 downloadButton('downloadData0', 'Download'),
                 br(),br(),
                 pre(includeText("dataclean.R"))
            )
    ),
    tabItem(tabName = "fcdashboard",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Functions",                
                 downloadButton('downloadData1', 'Download'),
                 br(),br(),
                 pre(includeText("fcdashboard.R"))
            )
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="UI",
                 downloadButton('downloadData2', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Server",
                 downloadButton('downloadData3', 'Download'),
                 br(),br(),
                 pre(includeText("server.R"))
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "FC Dashboard"),
  sidebar,
  body,
  skin = "black"
)