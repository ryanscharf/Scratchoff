library(tidyverse)
library(rvest)
library(jsonlite)

url <- 'https://www.flalottery.com/remainingPrizes'
xpath <- '//*[@id="scratch-offs"]/div/table'

games <- url %>%
  xml2::read_html() %>%
  html_node(xpath = xpath) %>%
  html_table()
