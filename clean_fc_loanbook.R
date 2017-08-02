## Load packages
library(tidyverse)

## Clean and load fc loanbook
load_clean_loanbook <- function(loanbook_path) {
  if (is.null(loanbook_path)) {
    ## path of loanbook 
    path  <- file.path("loanbook.csv")
  }else {
    path <- loanbook_path$datapath
  }
  
  ## Load data with miss spec as N/A
  loanbook <- read_csv(path, na='N/A')
  
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
           
    ## Add repayments made
    loanbook <- loanbook %>% 
      mutate(repayments_made = term - payments_remaining) %>% 
      mutate(repayments_made = repayments_made %>% 
               factor(ordered = TRUE)
               )
  
  ## Change loan term to an ordinal factor
  loanbook <- loanbook %>% 
    mutate(term = term %>% factor(ordered = TRUE))
  
  ##Add variables
  loanbook <- loanbook %>% 
    mutate(year = lubridate::year(loan_accepted_date) %>% factor,
           defaulted = principal_remaining %>% 
           replace(!(status %in% "defaulted"), 0),
           loan_amount_by_facet = loan_amount,
           defaulted_by_loan_amount = defaulted,
           principal_remaining_by_loan_amount = principal_remaining)
  return(loanbook)
}
