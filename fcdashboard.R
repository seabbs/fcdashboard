#' Load packages
library(tidyverse)
library(plotly)
library(lubridate)
library(wrapr)

#' Load cleaned data
source("dataclean.R")

#' Set up buttons for total loanbook funding all time
#' Current loanbook funding
#' Amount in defaulted loans
#' Default loads recovered

#' Summary statistics
summary_stats <- function(df) { 
  df %>% 
    summarise(amount_lent = sum(loan_amount, na.rm = TRUE),
              principal_repaid = sum(loan_amount - principal_remaining, na.rm = TRUE),
              recoveries = sum(recoveries, na.rm = TRUE),
              defaulted = principal_remaining %>%
                replace(!status %in% "defaulted", NA) %>% 
                sum(na.rm = TRUE)
              ) %>% 
    format
}

#' Plot amount lent
plot_by_date <- function(df, 
                         by = "loan_amount", 
                         strat = "credit_band",
                         plotly = TRUE) {
wrapr::let(
  list(X = by, Y = strat), {
    df <- df %>% 
      mutate(`Loan Acceptance (by month)` = 
               lubridate::round_date(loan_accepted_date, unit = "month"),
             Year = lubridate::year(loan_accepted_date)) %>% 
      dplyr::group_by(`Loan Acceptance (by month)`, Y, Year)  %>% 
      dplyr::summarise(`Amount (Pounds)` =  sum(X, na.rm = TRUE)) %>% 
      na.omit
    
    p <- df %>% 
      ggplot(aes(x = `Loan Acceptance (by month)`,
                 y = `Amount (Pounds)`, 
                 colour = Y,
                 group = Y)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
)


 
 if (plotly) {
   ggplotly(p)
 }else {
   p
 }
}