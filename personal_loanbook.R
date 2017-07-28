library(tidyverse)
library(wrapr)
library(stringr)

#' - Third tab: - 
#'   - Comparing personal loanbook and fc loan book
#'   - For comparisions need to use proportions or will not be comparable - reuse graphs but with proportions if possible
#'   - Sub tab PCA your data set main dataset
#'   - Bad debt interest by number of loans and sample from loanbook, add option to sample based on your distribution

## Load and clean personal loanbook
loan_clean_personal_loanbook <- function(personal_loanbook_path) {
  
  if(is.null(personal_loanbook_path)) {
    if (file.exists("personal_loanbook.csv")) {
      path <- "personal_loanbook.csv"
    }else {
      path <- NULL
    }
  }else {
    path <- personal_loanbook_path$datapath
  }
  
  personal_loanbook <- read_csv(path)
  
  ##Change risk to ordered factor
  personal_loanbook <- personal_loanbook %>% 
    mutate(Risk = Risk %>% factor(
      levels = c("A+", "A", "B", "C", "D", "E")
    ))
  
  ##Munge and reformat rate so that it is ordered
  personal_loanbook <- personal_loanbook %>%
    mutate(Rate = str_replace_all(Rate, "%", "") %>% 
             as.numeric) %>% 
    mutate(Rate = Rate %>% 
             factor(levels = unique(Rate)[order(unique(Rate))],
                    ordered = TRUE)
           ) 
  return(personal_loanbook)
}

## Join FC and Personal Loanbooks
bind_loanbooks <- function(personal_loanbook, fc_loanbook, verbose= TRUE) {
  personal_loanbook <- personal_loanbook %>% 
  rename(id = `Loan ID`) %>%
    mutate(invested_in = "Yes")
combined_loanbook <- fc_loanbook %>% 
  full_join(personal_loanbook) %>% 
  mutate(invested_in = invested_in %>% 
           replace(is.na(invested_in), "No")) %>% 
  mutate(`Repayments made` = as.numeric(as.character(term)) - payments_remaining %>% 
           as.integer) %>% 
  mutate(`Percentage repaid` = round(`Repayments made` / as.numeric(as.character(term)) * 100))

if (verbose) {
  loans_without_data <- is.na(combined_loanbook$credit_band) %>% sum
  message("Loan books are bound with ", loans_without_data, " missing loan entries.")
  if (loans_without_data > 1) {
    message("Consider uploading an updated funding circle loan book as some of your loans are missing data")
  }
}
return(combined_loanbook)
}
## Overall summary stats for boxes
p_loanbook_overall_sum_info <- function(df,
                                        aplus_bad = 0.6,
                                        a_bad = 1.5, 
                                        b_bad = 2.3,
                                        c_bad = 3.3,
                                        d_bad = 5,
                                        e_bad = 8) {
  ##Transform rate for  calc
  df <- df %>%
    mutate(rate_prog = Rate %>% 
             str_split(pattern = "%") %>%
             map_chr(paste, collapse = "") %>% 
             as.numeric) %>% 
    mutate(bad_debt = case_when(Risk %in% "A+" ~ aplus_bad,
                                Risk %in% "A" ~ a_bad,
                                Risk %in% "B" ~ b_bad,
                                Risk %in% "C" ~ c_bad,
                                Risk %in% "D" ~ d_bad,
                                Risk %in% "E" ~ e_bad)) %>% 
    mutate(anul_adj_rate = 100 * (1 + (rate_prog - bad_debt - 1) / (100 * 12)) ^ 12 - 100) %>% 
    mutate(anul_rate_prog = 100 * (1 + rate_prog / (100 * 12)) ^ 12 - 100) %>% 
    mutate(rate_weight = `Principal remaining` / sum(`Principal remaining`))
  
  ##Summarise on sector
  sector_max <- df %>% 
    group_by(Sector) %>% 
    summarise(prin_remaining = sum(`Principal remaining`)) %>% 
    ungroup %>%
    pull(prin_remaining) %>% 
    max
  
  ## Total lent
  total_lent <- df$`Principal remaining` %>% sum

  ## Build table
  df %>% 
    mutate(crude_interest = anul_rate_prog * `Principal remaining`) %>% 
    mutate(adj_interest = anul_adj_rate * `Principal remaining`) %>% 
    summarise(
      `Amount lent` = sum(`Principal remaining`) %>% 
        paste0("£", .),
      `Amount late (%)` = `Principal remaining` %>%
        replace(!`Loan status` %in% "Late", 0) %>% 
        sum,
      `Amount defaulted (%)` = `Principal remaining` %>%
        replace(!`Loan status` %in% "Defaulted", 0) %>% 
        sum,
      `Number of loans invested in` = n(),
      `Number of loan parts` = sum(`Number of loan parts`),
      `Maximum lent in a single loan (%)` = max(`Principal remaining`),
      `Maximum lent to a single sector (%)` = sector_max,
      `Crude interest rate` = sum(anul_rate_prog * rate_weight) %>% 
        round(digits = 1) %>% 
        paste0("%"),
      `Adjusted interest rate*` = sum(anul_adj_rate * rate_weight) %>% 
        round(digits = 1) %>% 
        paste0("%")
      ) %>% 
    mutate_at(.vars = c("Amount late (%)",
                        "Amount defaulted (%)",
                        "Maximum lent in a single loan (%)",
                        "Maximum lent to a single sector (%)"),
              .funs = funs(paste0("£", ., " (", 
                                  round(. / total_lent * 100, digits = 1),
                                  "%)")))
}
## Summary table stratified by stratification variable
p_loanbook_sum_table <- function(df, strat) {
  ## Total amount lent
  total_amount_lent <- sum(df$`Principal remaining`)
  
  ##Summarise loanbook
  df_sum <- df %>% 
    group_by(.dots = strat) %>% 
    summarise(`Amount lent (£)` = sum(`Principal remaining`),
              `Number of loan parts` = sum(`Number of loan parts`),
              `Percentage of loanbook (%)` = round(`Amount lent (£)` / total_amount_lent * 100, digits = 1)
        ) %>% 
    mutate(`Amount lent (£)` = round(`Amount lent (£)`, digits = 0))

  return(df_sum)
}
