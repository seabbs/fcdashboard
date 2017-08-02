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


## Summarise data
summarise_loanbook <- function(df = NULL,
                               xvar = NULL,
                               yvar = NULL, 
                               strat = NULL, 
                               facet = NULL) {
  
  ## Set up filtering
  strat_vect  <- strat
  if (!is.null(xvar)) {
    strat_vect <- c(strat_vect, xvar)
  }
  if (!facet %in% "no_facet") {
    strat_vect <- c(strat_vect, facet)
  }
  
  ##Group loanbook for summary
  group_df <- group_by(df, .dots = strat_vect)
  
  if (str_detect(yvar, "by_loan_amount")) {
    ##Summarise, normalising by loan amount in each group
    group_df <- group_df %>% 
      summarise_at(.vars = yvar,
                   .funs = funs(100 * sum(.)/sum(loan_amount)) 
      ) %>% 
      mutate_at(.vars = yvar, .funs = funs(round(., digits = 1)))
    
  }else if (str_detect(yvar, "by_defaulted")) {
    group_df <- group_df %>% 
      summarise_at(.vars = yvar,
                   .funs = funs(100 * sum(.)/sum(defaulted)) 
      ) %>% 
      mutate_at(.vars = yvar, .funs = funs(round(., digits = 1)))
    
  }else if (str_detect(yvar, "by_facet")) {
    ##Summarise, normalising by within facet total
    if (facet %in% "no_facet" && is.null(xvar)) {
        total_facet <- sum(df[[yvar]], na.rm = TRUE)
        
        group_df <- summarise_at(group_df, .vars = yvar,
                                 .funs = funs(100 * sum(.)/total_facet) 
        )
    }else{

      if (!is.null(xvar) && !facet %in% "no_facet") {
        facet_vect <- c(facet, xvar)
      }else if (is.null(xvar) && !facet %in% "no_facet") {
        facet_vect <- facet
      }else if (!is.null(xvar) && facet %in% "no_facet") {
        facet_vect <- xvar
      }
      
      total_facet <- df %>% 
        group_by(.dots = facet_vect) %>% 
        summarise_at(.vars = yvar, .funs = funs(sum(.))) %>% 
        ungroup %>% 
        rename_at(.vars =  yvar, .funs = (function(.) {"facet_sum"}))
      
      group_df <- group_df %>% 
        full_join(total_facet) %>% 
        ungroup %>% 
        group_by(.dots = strat_vect) %>% 
        summarise_at(.vars = yvar,
                     .funs = funs(100 * sum(., na.rm = TRUE)/facet_sum[1]) 
        )
    }
    
    group_df <- mutate_at(group_df, 
                          .vars = yvar, 
                          .funs = funs(round(., digits = 1)))
    
  }else if (str_detect(yvar, "no_loans")) {
    group_df <- summarise_at(group_df,
                             .vars = yvar,
                             .funs = funs(length(.)))
    }else{
    group_df <- group_df %>% 
      summarise_at(.vars = yvar, 
                   .funs = funs(sum(.)) 
      ) %>% 
      mutate_at(.vars = yvar, 
                .funs = funs(round(./1e6, digits = 1)))
  }
  
  ##Remove missing variables
  group_df <- na.omit(group_df)
  return(group_df)
}

##Plot loan book summary
plot_loanbook_summary <- function(df, 
                                  yvar, 
                                  strat,
                                  facet,
                                  scaled_to_mil = FALSE,
                                  plotly = TRUE) {
  
  if (facet %in% "no_facet") {
    size_point <- 7
  }else {
    size_point <- 2
  }
  
  p <- df %>%
    ggplot(aes_string(x = strat, y = yvar, colour = strat)) + 
    geom_segment(aes_string(xend = strat, yend = 0)) + 
    geom_point(size = size_point)
  
  if (facet %in% "no_facet") {
    p <- p +
      geom_text(aes_string(label = yvar, y = yvar),
                vjust = 0, size = 2, colour = "white")
  }
  
  if (scaled_to_mil) {
    if (str_detect(yvar, "by_loan_amount") | 
        str_detect(yvar, "by_facet") |
        str_detect(yvar, "by_defaulted")) {
      p <- p + ylab(paste0(yvar, " (%)"))
    }else if (str_detect(yvar, "no_loans")) {
      p <- p + ylab(paste0(yvar, " (no.)"))
      }else {
      p <- p +
        ylab(paste0(yvar, " (£, Millions)"))
    }
  }
  
  p <- p +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45,hjust = 1),
          legend.position = "none")
  
  if (!facet %in% "no_facet") {
    p <- p +
      facet_wrap(facet, scales = "fixed")
  }

  
    
  if (plotly) {
    ggplotly(p)
  }else{
    p
  }
}


#' Plot amount lent
plot_by_date <- function(df, 
                         by = "loan_amount", 
                         strat = "credit_band",
                         facet = "no_facet",
                         plotly = TRUE,
                         round_date = "month") {
  
      df <- df %>% 
        mutate(`Loan Acceptance` = 
                 lubridate::floor_date(loan_accepted_date, unit = round_date))
      
      df <- df %>%
        summarise_loanbook(xvar = "`Loan Acceptance`",
                           yvar = by, 
                           strat = strat, 
                           facet = facet)
      
      p <- df %>% 
        ggplot(aes_string(x = "`Loan Acceptance`",
                   y = by, 
                   colour = strat)) +
        geom_point() +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      if (str_detect(by, "by_loan_amount") || 
          str_detect(by, "by_facet") || 
          str_detect(by, "by_defaulted")) {
        p <- p + ylab(paste0(by, " (%)")) 
      }else if (str_detect(by, "no_loans")) {
        p <- p + ylab(paste0(by, " (no.)"))
        }else {
        p <- p + ylab(paste0(by, " (£, Millions)"))
      }
      
      if (!facet %in% "no_facet") {
        p <- p + facet_wrap(facet, scales = "fixed")
      }

  if (plotly) {
    ggplotly(p) %>%
      plotly::layout(autosize = TRUE)
  }else {
    p
  }
}

#' violin plot of data
plot_dist <- function(df, 
                      by = "loan_amount",
                      strat = "credit_band",
                      facet = "no_facet",
                      plotly = TRUE)
  wrapr::let(
    list(X = by, Y = strat), {
      if (str_detect(by, "by_loan_amount")) {
        df <- df %>% 
          mutate(X = 100 * X / loan_amount)
      }else if (str_detect(by, "by_defaulted")) {
        df <- df %>% 
          mutate(X = 100 * X / defaulted)
      }else {
        df <- df %>% 
          mutate(X = X / 1e3)
      }

      p <- df %>% 
        ggplot(aes(x = Y, y = X, fill = Y)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45,hjust = 1),
              legend.position = "none")
      
      if (str_detect(by, "by_loan_amount") | str_detect(by, "by_defaulted")) {
        p <- p +  ylab(paste0(by, " (%)"))
      }else{
       p <- p + ylab(paste0(by, " (£, Thousands)")) 
      }
      
      if (!facet %in% "no_facet") {
        p <- p + facet_wrap(facet, scales = "fixed")
      }
      
      if (!by %in% c("no_loans") && !str_detect(by, "by_facet")) {
        if (plotly) {
          ggplotly(p) %>%
            plotly::layout(autosize = TRUE)
        }else {
          p
        }
      }
    }
  )

#' scatter of 2 numeric variables
plot_scatter <- function(df, 
                         by = "loan_amount",
                         also_by = "term",
                         strat = "credit_band",
                         facet = "no_facet",
                         plotly = TRUE,
                         alpha = 0.4) {
  if (also_by %in% "term") {
    df <- df %>% 
      mutate(term = term %>% 
               as.character %>% 
               as.numeric)
  }
  
  wrapr::let(
    list(X = by, Y = strat, Z = also_by), {
      
   if (str_detect(by, "by_loan_amount")) {
     df <- df %>% 
       mutate(X = 100 * X / loan_amount)
   }else if (str_detect(by, "by_defaulted")) {
     df <- df %>% 
       mutate(X = 100 * X / defaulted)
   }else{
     df <- df %>%
       mutate(X = X/1e3)
   }

      p <- df %>% 
        ggplot(aes(x = X, y = Z, colour = Y)) +
        geom_count(alpha = alpha, show.legend = TRUE) +
        theme_minimal()
      
      if (str_detect(by, "by_loan_amount") | str_detect(by, "by_defaulted")) {
        p <- p + xlab(paste0(by, " (%)"))
      }else {
        p <- p + xlab(paste0(by, " (£, Thousands)"))
      }
      
      if (!facet %in% "no_facet") {
        p <- p + facet_wrap(facet, scales = "fixed")
      }
      
      if (!by %in% c("no_loans") && !str_detect(by, "by_facet")) {
        if (plotly) {
          ggplotly(p) %>%
            plotly::layout(autosize = TRUE)
        }else {
          p
        }
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