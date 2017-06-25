## Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(rmarkdown)

## Source cleaned data
source("dataclean.R")

## Source functions 
source("fcdashboard.R")

## Slider in menu bar to control dates of data shown
## Buttons showing summary data at bottom
## two summary graphs in main dashboard

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Dashboard", tabName="dashboard", icon=icon("dashboard"), selected=TRUE),
              menuItem("Exploratory Plots", tabName = "explore_plots", icon = icon("line-chart")),
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
  sliderInput(inputId = 'dates', 
              label = 'Time Range',
              min = min(loanbook$loan_accepted_date),
              max = max(loanbook$loan_accepted_date),
              value = range(loanbook$loan_accepted_date),
              timeFormat="%b %Y"),
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
                               plotlyOutput("plotdist"))),
            infoBoxOutput("amount_lent"),
            infoBoxOutput("repaid"),
            infoBoxOutput("defaulted"),
            infoBoxOutput("recovered")
    )
    ),
    tabItem(tabName = "explore_plots",
            fluidRow(
              column(width = 4, 
                     box(width = NULL)
                     ),
              column(width = 8)
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