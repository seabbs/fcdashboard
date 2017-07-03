library(tidyverse)

source("dataclean.R")
#' Dataset by loan
#' Data set contains:
#' - the number of loan parts owned
#' - loan id
#' - loan status (differs from overall loanbook status)
#' - loan rate (is this the rate that you paid i.e if purchased from secondary
#' market or is this the actual loan rate)
#' - amount remaining 
#' - number of loan parts held
#' 
#' Dataset by loan part
#' Data set contain:
#' - same as above but converted into individual status (i.e what if one part is for sale
#' and another is not)
#' - contains the amount for each loan part. could own the same loan with parts of different amounts
#' - no additional information on how much loans are for sale for etc
#' 
#' Data upload button option, for testing have data in local repo (remove when uploaded to server)
#' Need to add  option to upload complete loanbook as well if it is out of date.
#' 
#' Data munging:
#' - Load data
#' - Rename unique varibles
#' - Full join to funding circle loanbook
#' - If number of loan parts is NA add variable saying not in personal loanbook
#' if it has a value then declare it to be in personal loan book
#' 
#' Data visualisation: Implement tab 1 and tabs reusing code and release. Then implement comparision with FC loanbook and release
#' Then add improvements from list and feddback.
#' - First tab - Personal dashboard:
#'   - option to change bad debt estimates source from FC.
#'   - First table is a table of your data
#'   - Second table summarises your data (skimr table)
#'   - Lollipop chart of variable statified category y axis is either loan ampount held, or number of parts, proportion of amount lent
#'   - Loan repayment schedule (not stratified) and same graph stratified.
#'   - Button information: amount lent, crude interest rate, expected interest after fees and bad debt
#' - Second tab - Explore data set:
#'   - Reuse code for PCA and exploratory plotting from the FC loan book as 2 and third tabs
#' filtering so only loans in personal loanbook are presented and loan amount is set to be the 
#' amount that you hold. Add filter to switch between loan book amount and the amount you hold.
#' - Third tab: - 
#'   - Comparing personal loanbook and fc loan book
#'   - For comparisions need to use proportions or will not be comparable - reuse graphs but with proportions if possible
#'   - Sub tab PCA your data set main dataset
#'   - Bad debt interest by number of loans and sample from loanbook, add option to sample based on your distribution


personal_loanbook <- read_csv("personal_loanbook.csv")

bind_loanbooks <- function(personal_loanbook, fc_loanbook, verbose= TRUE) {
  personal_loanbook <- personal_loanbook %>% 
  rename(id = `Loan ID`)

combined_loanbook <- fc_loanbook %>% 
  full_join(personal_loanbook)

if (verbose) {
  loans_without_data <- is.na(combined_loanbook$credit_band) %>% sum
  message("Loan books are bound with ", loans_without_data, " missing loan entries. Considering uploading an updated funding circle loan book")
}
return(combined_loanbook)
}

