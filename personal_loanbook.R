library(tidyverse)
library(wrapr)
library(stringr)
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
    mutate(adj_rate = rate_prog - 1) %>% 
    mutate(bad_debt = case_when(Risk %in% "A+" ~ aplus_bad,
                                Risk %in% "A" ~ a_bad,
                                Risk %in% "B" ~ b_bad,
                                Risk %in% "C" ~ c_bad,
                                Risk %in% "D" ~ d_bad,
                                Risk %in% "E" ~ e_bad)) %>% 
    mutate(adj_rate = adj_rate - bad_debt) %>% 
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
    mutate(crude_interest = rate_prog * `Principal remaining`) %>% 
    mutate(adj_interest = adj_rate * `Principal remaining`) %>% 
    summarise(
      `Amount lent (£)` = sum(`Principal remaining`),
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
      `Crude interest rate` = sum(rate_prog * rate_weight) %>% 
        round(digits = 1) %>% 
        paste0("%"),
      `Adjusted interest rate*` = sum(adj_rate * rate_weight) %>% 
        round(digits = 1) %>% 
        paste0("%")
      ) %>% 
    mutate_at(.vars = c("Amount late (%)",
                        "Amount defaulted (%)",
                        "Maximum lent in a single loan (%)",
                        "Maximum lent to a single sector (%)"),
              .funs = funs(paste0(., " (", 
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

##Plot loan book summary
plot_p_loanbook_summary <- function(df, 
                                    yvar, 
                                    strat,
                                    plotly = TRUE) {
  p <- df %>% 
    ggplot(aes_string(x = strat, y = yvar, colour = strat)) + 
    geom_segment(aes_string(xend=strat, yend=0)) + 
    geom_point(size = 7) +
    geom_text(aes_string(label=yvar, y=yvar),
              vjust=0, size=2, colour = "white") + 
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45,hjust=1),
          legend.position = "none")
  
  
  if (plotly) {
    ggplotly(p)
  }else{
    p
  }
}
