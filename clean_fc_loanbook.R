## Load packages
library(tidyverse)

## Clean and load fc loanbook
load_clean_loanbook <- function(loanbook_path,
                                ref_date = "date_la") {
  if (is.null(loanbook_path)) {
    ## path of loanbook 
    path  <- file.path("loanbook.csv")
  }else {
    path <- loanbook_path$datapath
  }
  
  ## Load data with miss spec as N/A
  loanbook <- read_csv(path, na = 'N/A')
  
  ## Clean loan status text
  loanbook <- loanbook %>% 
    separate(status,
             c("temp", "status"), 
             sep = ": ") %>% 
    select(-temp)
  
  ## Change variables into factors when ordering is important
  ## Make sure strings do not have free text
  loanbook <- loanbook %>% 
    mutate_at(.vars = c('status','credit_band',
                        'loan_purpose', 'sector', 
                        'business_type_name', 'region_name',
                        'whole_loan', 'repayment_type', 
                        'security_taken'),
              .funs = funs(factor))
  
  ## relevel credit band
  loanbook <- loanbook %>% 
    mutate(credit_band = credit_band %>%
             factor(levels = c('A+ (Very low risk)', 
                               'A (Low risk)', 
                               'B (Below average risk)', 
                               'C (Average risk)', 
                               'D', 
                               'E')
             ))
    
    ## Clean date of next repayment
    loanbook <- loanbook %>% 
      mutate(next_repayment = next_repayment %>% 
               replace(next_repayment %in% "", NA) %>% 
               date)
    
    ## Add repayments made
    loanbook <- loanbook %>% 
      mutate(repayments_made = term - payments_remaining) %>% 
      mutate(repayments_made = repayments_made %>% 
               factor(ordered = TRUE)
               )
  
    ## Add date of default - assuming it is date of acceptance plus number of repayments made
    loanbook <- loanbook %>% 
      mutate(date_of_default = loan_accepted_date %>% 
               replace(!status %in% "defaulted", NA)
      ) %>% 
      mutate(date_of_default = date_of_default  %m+%
               months(as.numeric(as.character(repayments_made))))
    
  ## set reference date - dropping data missing reference date
  if (ref_date %in% "date_la" || is.null(ref_date)) {
      loanbook <- loanbook %>% 
        mutate(ref_date = loan_accepted_date)
   }else if (ref_date %in% "date_d") {
      loanbook <- loanbook %>% 
        mutate(ref_date = date_of_default)
    }else if (ref_date %in% "date_np") {
      loanbook <- loanbook %>% 
        mutate(ref_date = next_repayment)
    }else if (ref_date %in% "date_fp") {
      loanbook <- loanbook %>% 
        mutate(ref_date = date_repaid)
    }
    
  loanbook <- loanbook %>% 
    filter(!is.na(ref_date))
  
  ## Change loan term to an ordinal factor
  loanbook <- loanbook %>% 
    mutate(term = term %>% factor(ordered = TRUE))
  
  ##Add variables
  loanbook <- loanbook %>% 
    mutate(grouped_loan_amount = case_when(loan_amount < 5e4 ~ "0-49,999",
                                           loan_amount < 1e5 ~ "50,000 - 99,999",
                                           loan_amount < 1.5e5 ~ "100,000 - 149,999",
                                           loan_amount < 2e5 ~ "150,000 - 199,999",
                                           loan_amount < 2.5e5 ~ "200,000 - 249,999",
                                           loan_amount < 3e5 ~ "250,000 - 299,999",
                                           loan_amount < 3.5e5 ~ "300,000 - 349,999",
                                           loan_amount < 4e5 ~ "350,000 - 399,999",
                                           loan_amount >= 4e5 ~"400,000+") %>% 
             factor(levels = c("0-49,999", "50,000 - 99,999", "100,000 - 149,999",
                               "150,000 - 199,999", "200,000 - 249,999", 
                               "250,000 - 299,999", "300,000 - 349,999",
                               "350,000 - 399,999", "400,000+"), ordered = TRUE),
           no_loans = 1, 
           loan_amount_by_facet = loan_amount,
           principal_remaining_by_facet = principal_remaining,
           principal_remaining_by_loan_amount = principal_remaining,
           principal_remaining_by_num_loans = ifelse(principal_remaining > 0, 1, 0),
           defaulted = principal_remaining %>% 
           replace(!(status %in% "defaulted"), 0),
           defaulted_by_loan_amount = defaulted,
           defaulted_by_facet = defaulted,
           defaulted_by_loan_amount = defaulted,
           defaulted_by_num_loans = ifelse(status %in% "defaulted", 1, 0),
           recoveries_by_facet = recoveries,
           recoveries_by_loan_amount = recoveries,
           recoveries_by_num_loans = ifelse(recoveries > 0, 1, 0),
           recoveries_by_defaulted = recoveries,
           unrecovered = defaulted - recoveries,
           unrecovered_by_facet = unrecovered,
           unrecovered_by_loan_amount = unrecovered,
           unrecovered_by_num_loans = ifelse(unrecovered > 0, 1, 0),
           unrecovered_by_defaulted = unrecovered,
           day = lubridate::day(ref_date) %>%  factor,
           day_of_week = lubridate::wday(ref_date, label = TRUE),
           week = lubridate::week(ref_date) %>% factor,
           month = lubridate::month(ref_date, label = TRUE),
           year = lubridate::year(ref_date) %>% factor
           )
  return(loanbook)
  
}
