library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)
library(lubridate)
#library(taskscheduleR)


###notes
###set up taskscheduler
###pull data > make table > make html file (knitr?) > send to wordpress
##https://developer.wordpress.org/rest-api/
##https://opencontent.org/blog/archives/5779

url <- 'https://www.flalottery.com/remainingPrizes'
xpath <- '//*[@id="scratch-offs"]/div/table'

ending_games <- 'https://www.flalottery.com/endingGames' %>%
  xml2::read_html() %>%
  #html_node(xpath = '//*[@id="scratch-offs"]/div/table') %>%
  html_node(xpath = '//*[(@id = "scratch-offs")]') %>% html_nodes('table') %>%
  html_table() %>% .[[1]] %>% clean_names() %>%
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


game_url <-  'https://www.flalottery.com/scratch-offsGameDetails?gameNumber='
#game_tbl <- 
  
calc_ev <- function(cost, odds_denomintor, win){
  ev <- (win * 1/odds_denomintor) - (cost * (1-1/odds_denomintor))
  return(ev)
  }
  
  get_granular_info <- function(x){
    text <- xml2::read_html(paste0(game_url,x)) %>%
      html_nodes(xpath = '//p') %>% html_text()
    overall_odds <- text[grepl('Overall Odds', text)]
    overall_odds_d <- as.numeric(
      stringr::str_extract(
        overall_odds,
        '(?<=Overall Odds: 1-in-).*'
        )
      )
      
    num_printed_tickets <- ((1-(1/overall_odds_d)) * sum(test$total_prizes)) + sum(test$total_prizes)
    
    ticket_price <- text[grepl('Ticket Price', text)]
     ticket_price <- as.numeric(
       stringr::str_extract(
       ticket_price,
       '(?<=Ticket Price:\\s\\$).*(?=L)'
       )
     )
     
    xml2::read_html(paste0(game_url,x)) %>%
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "column2", " " ))]') %>% 
      html_nodes('table') %>% 
      html_table() %>% .[[1]] %>% clean_names() %>%
      mutate(
        prize_amount = parse_number(prize_amount),
        odds_denominator = parse_number(
          str_remove(
            odds_of_winning, 
            '1-in-')
          ),
        odds_of_winning = 1/odds_denominator
        )
  }