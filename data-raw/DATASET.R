## code to prepare `DATASET` dataset goes here

#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the prices and other attributes of almost 54,000
#' diamonds.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}


lm_patho <- read.csv("E:/HW 2020 FALL/computational statistics/homework-1/lm_patho.csv")

usethis::use_data(lm_patho, overwrite = TRUE)
