

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
#' - no additional informatio on how much loans are for sale for etc
