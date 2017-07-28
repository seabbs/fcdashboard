#' Load packages
library(tidyverse)
library(plotly)
library(lubridate)
library(wrapr)


#' Summary statistics
summary_stats <- function(df) { 
  df %>% 
    summarise(amount_lent = sum(loan_amount, na.rm = TRUE),
              principal_repaid = sum(loan_amount - principal_remaining, na.rm = TRUE),
              recoveries = sum(recoveries, na.rm = TRUE),
              defaulted = principal_remaining %>%
                replace(!status %in% "defaulted", NA) %>% 
                sum(na.rm = TRUE)
    ) 
}

#' Plot amount lent
plot_by_date <- function(df, 
                         by = "loan_amount", 
                         strat = "credit_band",
                         plotly = TRUE,
                         round_date = "month") {
  wrapr::let(
    list(X = by, Y = strat), {
      df <- df %>% 
        mutate(`Loan Acceptance` = 
                 lubridate::round_date(loan_accepted_date, unit = round_date)) %>% 
        dplyr::group_by(`Loan Acceptance`, Y)  %>% 
        dplyr::summarise(X =  sum(X, na.rm = TRUE)/1e6) %>% 
        na.omit
      
      p <- df %>% 
        ggplot(aes(x = `Loan Acceptance`,
                   y = X, 
                   colour = Y)) +
        ylab(paste0(by, " (£, Millions)")) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  )
  
  
  
  if (plotly) {
    ggplotly(p) %>%
      plotly:: layout(autosize = TRUE)
  }else {
    p
  }
}

#' violin plot of data
plot_dist <- function(df, 
                      by = "loan_amount",
                      strat = "credit_band",
                      plotly = TRUE)
  wrapr::let(
    list(X = by, Y = strat), {
      df <- df %>% 
        mutate(X = X / 1e3)
      
      p <- df %>% 
        ggplot(aes(x = Y, y = X, fill = Y)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        ylab(paste0(by, " (£, Thousands)")) +
        theme_minimal() +
        theme(axis.text.x=element_text(angle=45,hjust=1),
              legend.position = "none")
      
      if (plotly) {
        ggplotly(p) %>%
          plotly:: layout(autosize = TRUE)
      }else {
        p
      }
    }
  )

#' scatter of 2 numeric variables
plot_scatter <- function(df, 
                         by = "loan_amount",
                         also_by = "term",
                         strat = "credit_band",
                         plotly = TRUE,
                         alpha = 0.4) {
  if(also_by %in% "term") {
    df <- df %>% 
      mutate(term = term %>% 
               as.character %>% 
               as.numeric)
  }
  
  if (by %in% "defaulted" || also_by %in% "defaulted") {
    df <- df %>% 
      filter(status %in% "defaulted") %>% 
      mutate(defaulted = principal_remaining)
  }
  
  wrapr::let(
    list(X = by, Y = strat, Z= also_by), {
      
      
      p <- df %>%
        mutate(X = X/1e3) %>% 
        ggplot(aes(x = X, y = Z, colour = Y)) +
        geom_count(alpha = alpha, show.legend = TRUE) +
        xlab(paste0(by, " (£, Thousands)")) +
        theme_minimal()
      
      if (plotly) {
        ggplotly(p) %>%
          plotly:: layout(autosize = TRUE)
      }else {
        p
      }
    }
  )
  
}


pca_on_loanbook <- function(df, no_pca = 2) {
  #' Perform pca using caret
  #' Drop variables with missing information/not true numeric variables
  filter_loanbook <-  df %>% 
    mutate(term = term %>% as.character %>% as.numeric) %>% 
    select(-id, 
           -next_repayment, 
           -year_incorporated, 
           -loan_guaranteed,
           -max_accepted_bid_rate,
           -min_accepted_bid_rate)
  
  numeric_loanbook <- filter_loanbook %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame
  
  process_loanbook <- numeric_loanbook %>% 
    preProcess(method = c("BoxCox", "center", "scale"))
  
  process_loanbook <- predict(process_loanbook, numeric_loanbook)
  
  pca_loanbook <- prcomp(process_loanbook, 
                         center = FALSE,
                         scale. = FALSE,
                         rank. = no_pca)
  return(list(pca_loanbook, filter_loanbook)) 
}

plot_pca <- function(df_list,
                     pc_1 = 1, 
                     pc_2 = 2,
                     strat, 
                     plotly = TRUE,
                     alpha = 0.2) {
  p <- autoplot(df_list[[1]], 
                x = pc_1,
                y = pc_2, 
                data = df_list[[2]],
                colour = strat,
                alpha = alpha,
                loadings = TRUE,
                loadings.colour = "black",
                loadings.label = TRUE,
                loadings.label.colour = "black",
                loadings.label.size = 3) + 
    theme_minimal()
  
  if (plotly) {
    ggplotly(p)
  }else{
    p
  }
}

#' Convert million
convert_million <- function(x, label = "M") {
  x <- round(x / 1e6, 1) 
  x <- format(x, big.mark = ",")
  x <- paste0(x, label)
}

#' Return with per
return_with_per <- function(df, num, denom) {
  num <- enquo(num)
  denom <- enquo(denom)
  
  paste0(df %>% 
           select(!!num) %>% 
           convert_million, " (", 
         df %>%
           mutate(per = (!!num) / (!!denom)) %>% 
           mutate(per = round(per * 100, digits = 1)) %>% 
           select(per), "%)")
}