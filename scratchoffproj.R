library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)
library(lubridate)


url <- 'https://www.flalottery.com/remainingPrizes'
xpath <- '//*[@id="scratch-offs"]/div/table'

ending_games <- 'https://www.flalottery.com/endingGames' %>%
  xml2::read_html() %>%
  html_node(xpath = '//*[@id="scratch-offs"]/div/table') %>%
  html_table() %>% clean_names() %>%
  mutate(
    last_day_to_sell = parse_date(last_day_to_sell, format = '%B %d, %Y'),
    last_day_to_redeem = parse_date(last_day_to_redeem, format = '%B %d, %Y')
  ) %>% select(game_number, last_day_to_sell, last_day_to_redeem)

games <- url %>%
  xml2::read_html() %>%
  html_node(xpath = xpath) %>%
  html_table() %>% clean_names()


games_cleaned <- games %>%
  transmute(
    game_number,
    game_name,
    top_prize = parse_number(top_prize),
    ticket_cost = parse_number(ticket_cost),
    tot_prizes = parse_number(str_split(
      top_prizes_remaining, '\\sof\\s', simplify = T
    )[, 2]),
    prizes_remaining = parse_number(str_split(
      top_prizes_remaining, '\\sof\\s', simplify = T
    )[, 1]),
    reorder = str_detect(top_prizes_remaining, '\\*')
  ) %>% left_join(ending_games, 'game_number')


games_enh <-
  games_cleaned %>% mutate(
    value_remaining = top_prize * prizes_remaining,
    percent_remaining = prizes_remaining / tot_prizes,
    ending = (game_number %in% ending_games$game_number)
  )
