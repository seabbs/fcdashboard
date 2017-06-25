## Load packages
library(tidyverse)

## path of loanbook 
path  <- file.path("loanbook.csv")

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
           factor(levels =c('A+ (Very low risk)', 
                            'A (Low risk)', 
                            'B (Below average risk)', 
                            'C (Average risk)', 
                            'D', 
                            'E')
                  )
  )

## Change loan term to an ordinal factor
loanbook <- loanbook %>% 
  mutate(term = term %>% factor(ordered = TRUE))
