## Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
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

## Slider in menu bar to control dates of data shown
## Buttons showing summary data at bottom
## two summary graphs in main dashboard

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "menu",
              menuItem("Personal Dashboard", icon = icon("dashboard"),
                       menuSubItem("Dashboard", tabName = "p_dashboard", icon=icon("dashboard")),
                       menuSubItem("Exploratory Plots", tabName = "p_exploratory", icon=icon("line-chart")),
                       menuSubItem("PCA", tabName = "p_pca", icon = icon("line-chart"))
              ),
              menuItem("FC Dashboard", icon = icon("dashboard"),
                       menuSubItem("Dashboard", tabName = "fc_dashboard", icon=icon("dashboard")),
                       menuSubItem("PCA", tabName = "fc_pca", icon = icon("line-chart"))
                       ),
              menuItem("About", tabName = "readme", icon=icon("info"), selected = TRUE),
              menuItem("Code",  icon = icon("code"),
                       menuSubItem("Github", href = "https://github.com/seabbs/fcdashboard", icon = icon("github")),
                       menuSubItem("clean_fc_loanbook.R", tabName = "dataclean", icon = icon("angle-right")),
                       menuSubItem("utility_functions.R", tabName = "utility_functions", icon = icon("angle-right")),
                       menuSubItem("personal_loanbook.R", tabName = "personal_loanbook", icon = icon("angle-right")),
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
         title = "Select the data range. Data out of date? Tweet @seabbs."),
  conditionalPanel(condition = 'input.menu == "fc_dashboard"',
selectInput("fc_yaxis", 
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
                   selectInput("fc_strat_var", 
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
                   selectInput("fc_com_var", 
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
conditionalPanel(condition = 'input.menu == "p_exploratory"',
                 selectInput("p_yaxis", 
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
                 selectInput("p_strat_var", 
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
                 selectInput("p_com_var", 
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
conditionalPanel(condition = 'input.menu == "fc_pca"',
                 sliderInput(inputId = "fc_no_pca", 
                             label = "Number of Principal components:",
                             min = 0,
                             max = 10,
                             value = 2),
                 sliderInput(inputId = "fc_pca_1", 
                             label = "First component to plot:",
                             min = 0,
                             max = 10,
                             value = 1),
                 sliderInput(inputId = "fc_pca_2", 
                             label = "Second component to plot:",
                             min = 0,
                             max = 10,
                             value = 2),
                 selectInput("fc_strat_var2", 
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

conditionalPanel(condition = 'input.menu == "p_pca"',
                 sliderInput(inputId = "p_no_pca", 
                             label = "Number of Principal components:",
                             min = 0,
                             max = 10,
                             value = 2),
                 sliderInput(inputId = "p_pca_1", 
                             label = "First component to plot:",
                             min = 0,
                             max = 10,
                             value = 1),
                 sliderInput(inputId = "p_pca_2", 
                             label = "Second component to plot:",
                             min = 0,
                             max = 10,
                             value = 2),
                 selectInput("p_strat_var2", 
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
conditionalPanel(condition = 'input.menu == "p_dashboard"',
                 selectInput("p_dash_strat", 
                             "Variable to stratify by:",
                             list(Risk = 
                                    "Risk",
                                  `Loan purpose` = 
                                    "`Loan purpose`",
                                  Sector = "`Sector`",
                                  `Next payment date` = 
                                    "`Next payment date`",
                                  `Loan status` = "`Loan status`",
                                  Region = "Region",
                                  `Loan term` = "`Loan term`",
                                  `Repayments made` = "`Repayments made`",
                                  `Repayments left` = "`Repayments left`",
                                  `Percentage repaid` = "`Percentage repaid`",
                                  `Repayment type` = "`Repayment type`",
                                  `Security taken` = "`Security taken`"
                             )),
                 checkboxInput('filter_repaid',
                               "Filter repaid loans",
                               value = TRUE),
                 checkboxInput("show_bad_debt_opts",
                               "Edit bad debt estimates:",
                               value = FALSE),
                 conditionalPanel(condition = "input.show_bad_debt_opts",
                                        numericInput(inputId = "aplus_bad", 
                                                       label = "A+: Percentage bad debt",
                                                       min = 0,
                                                       max = 100,
                                                       value = 0.6,
                                                       step = 0.1
                                          ),
                                          numericInput(inputId = "a_bad", 
                                                       label = "A: Percentage bad debt",
                                                        min = 0,
                                                        max = 100,
                                                        value = 1.5,
                                                        step = 0.1),
                                           numericInput(inputId = "b_bad", 
                                                        label = "B: Percentage bad debt",
                                                        min = 0,
                                                        max = 100,
                                                        value = 2.3,
                                                        step = 0.1),
                                            numericInput(inputId = "c_bad", 
                                                         label = "C: Percentage bad debt",
                                                         min = 0,
                                                         max = 100,
                                                         value = 3.3,
                                                         step = 0.1),
                                            numericInput(inputId = "d_bad", 
                                                         label = "D: Percentage bad debt",
                                                         min = 0,
                                                         max = 100,
                                                         value = 5,
                                                         step = 0.1),
                                            numericInput(inputId = "e_bad", 
                                                         label = "E: Percentage bad debt",
                                                         min = 0,
                                                         max = 100,
                                                         value = 8,
                                                         step = 0.1)
                        )
                 ),
  helpText("Developed by ", 
           a("Sam Abbott", href = "http://samabbott.co.uk"), ".",
           style = "padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.md")
    ),
    tabItem(tabName = "fc_dashboard",
            fluidRow(
              tags$head(includeScript("google-analytics.js")),
              tabBox( width = 12,
                      title = "Summary Plots",
                      side = "right",
                      tabPanel(title = "By Year",
                               plotlyOutput("fc_plottotal", height = "200%")),
                      tabPanel(title = "By Stratified Variable",
                               plotlyOutput("fc_plotdist", height = "200%")),
                      tabPanel(title = "Variable vs. Variable",
                               plotlyOutput("fc_plotscatter", height = "200%"))),
            infoBoxOutput("fc_amount_lent"),
            infoBoxOutput("fc_repaid"),
            infoBoxOutput("fc_defaulted"),
            infoBoxOutput("fc_recovered")
    )
    ),
    tabItem(tabName = "p_dashboard",
            fluidRow(
              box(title = "Overview",
                  solidHeader = FALSE,
                  tableOutput("p_loanbook_overview"),
                  footer = "* The adjusted interest rate accounts for the 1% Funding Circle fee and estimated
                  bad debt (settable in the options menu).",
                  width = 12
                  ),
              column(width = 6,
                     tabBox( width = NULL,
                             title = "Summary Plots",
                             side = "right",
                             tabPanel(title = "Amount lent (£)",
                                      plotlyOutput("p_loanbook_sum_plot_amount", height = "200%")),
                             tabPanel(title = "No. of loan parts",
                                      plotlyOutput("p_loanbook_sum_plot_no", height = "200%")),
                             tabPanel(title = "Percentage of loanbook (%)",
                                      plotlyOutput("p_loanbook_sum_plot_per", height = "200%"))
                     )
              ),
              column(width = 6,
                     tabBox( width = NULL,
                             title = "Summary Tables",
                             side = "right",
                             tabPanel(title = "Summary",
                                      DT::dataTableOutput("p_loanbook_sum_table")),
                             tabPanel(title = "Loanbook",
                                      DT::dataTableOutput("p_loanbook_table", width = "auto")
                             ))
              )
            )
    ),
    tabItem(tabName = "p_exploratory",
            fluidRow(
              tags$head(includeScript("google-analytics.js")),
              tabBox( width = 12,
                      title = "Summary Plots",
                      side = "right",
                      tabPanel(title = "By Year",
                               plotlyOutput("p_plottotal", height = "200%")),
                      tabPanel(title = "By Stratified Variable",
                               plotlyOutput("p_plotdist", height = "200%")),
                      tabPanel(title = "Variable vs. Variable",
                               plotlyOutput("p_plotscatter", height = "200%"))),
              infoBoxOutput("p_amount_lent"),
              infoBoxOutput("p_repaid"),
              infoBoxOutput("p_defaulted"),
              infoBoxOutput("p_recovered")
            )
    ),
    tabItem(tabName = "fc_pca",
            fluidRow(
              tabBox( width = 12,
                      title = "Principal Components",
                      side = "right",
                      tabPanel(title = "Scatter",
                               plotlyOutput("plot_fc_pca", height = "200%"))
              )
            )
            ),
   tabItem(tabName = "p_pca",
          fluidRow(
            tabBox( width = 12,
                    title = "Principal Components",
                    side = "right",
                    tabPanel(title = "Scatter",
                             plotlyOutput("plot_p_pca", height = "200%"))
            )
          )
  ),
    tabItem(tabName = "dataclean",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Data Cleaning of FC Loanbook",                
                 downloadButton('downloadData0', 'Download'),
                 br(),br(),
                 pre(includeText("clean_fc_loanbook.R"))
            )
    ),
    tabItem(tabName = "utility_functions",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Utility Functions",                
                 downloadButton('downloadData1', 'Download'),
                 br(),br(),
                 pre(includeText("utility_functions.R"))
            )
    ),
  tabItem(tabName = "personal_loanbook",
          box( width = NULL, status = "primary", solidHeader = TRUE, title="Personal Loanbook Functions",                
               downloadButton('downloadData2', 'Download'),
               br(),br(),
               pre(includeText("personal_loanbook.R"))
          )
  ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="UI",
                 downloadButton('downloadData3', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title="Server",
                 downloadButton('downloadData4', 'Download'),
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