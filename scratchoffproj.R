library(tidyverse)
library(rvest)
library(jsonlite)
library(janitor)
library(lubridate)
library(RMySQL)
library(DBI)
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

###ending games not existing in games list?
games_cleaned <- games %>%
  transmute(
    game_number,
    game_name,
    top_prize = parse_number(top_prize),
    ticket_cost = parse_number(ticket_cost),
    top_prizes = parse_number(str_split(
      top_prizes_remaining, '\\sof\\s', simplify = T
    )[, 2]),
    top_prizes_remaining = parse_number(str_split(
      top_prizes_remaining, '\\sof\\s', simplify = T
    )[, 1]),
    reorder = str_detect(top_prizes_remaining, '\\*')
  ) %>% left_join(ending_games, by = 'game_number') %>% 
  group_by(game_number) %>% filter(top_prize == max(top_prize))


games_enh <-
  games_cleaned %>% mutate(
    #value_remaining = top_prize * prizes_remaining,
    top_percent_remaining = top_prizes_remaining / top_prizes,
    ending = (game_number %in% ending_games$game_number)
  )


game_url <-  'https://www.flalottery.com/scratch-offsGameDetails?gameNumber='
#game_tbl <- 

calc_ev <- function(cost, odds_denomintor, win){
  ev <- (win * 1/odds_denomintor) - (cost * (1-1/odds_denomintor))
  return(ev)
}


# get_granular_info <- function(x) {
#   page <- xml2::read_html(paste0(game_url, x)) 
#   
#   tbl <- page %>%
#     html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "column2", " " ))]') %>% 
#     html_nodes('table') %>% 
#     html_table() %>% .[[1]] %>% clean_names() %>%
#     mutate(
#       prize_amount = parse_number(prize_amount),
#       odds_denominator = parse_number(
#         str_remove(
#           odds_of_winning, 
#           '1-in-')
#       ),
#       odds_of_winning = 1/odds_denominator
#     )
#   
#   text <- page %>%
#     html_nodes(xpath = '//p') %>% 
#     html_text()
#   
#   overall_odds <- text[grepl('Overall Odds', text)]
#   overall_odds_d <- as.numeric(
#     stringr::str_extract(
#       overall_odds,
#       '(?<=Overall Odds: 1-in-).*'
#     )
#   )
#   
#   num_printed_tickets <-
#     tbl %>% 
#     slice(1) %>% 
#     summarize(total_tickets = total_prizes*odds_denominator) %>% 
#     pull(total_tickets)
#   
#   losing_tix <- num_printed_tickets - sum(tbl$total_prizes) 
#   # ((1 - (1 / overall_odds_d)) * sum(tbl$total_prizes)) + sum(tbl$total_prizes)
#   num_current_tickets_losing = (sum(tbl$prizes_remaining))/sum(tbl$total_prizes) * losing_tix
#   num_current_tickets_total = num_current_tickets_losing + sum(tbl$prizes_remaining)
#   p_0 <- 1 - sum(tbl$odds_of_winning)
#   
#   ticket_price <- text[grepl('Ticket Price', text)]
#   ticket_price <- as.numeric(
#     stringr::str_extract(
#       ticket_price,
#       '(?<=Ticket Price:\\s\\$).*(?=L)'
#     )
#   )
#   
#   tbl <- tbl %>% mutate(expected_return_orig = odds_of_winning * prize_amount,
#                         current_odds = prizes_remaining/num_current_tickets_losing,
#                         expected_return_current = current_odds * prize_amount)
#   
#   expected_values <- tbl %>% 
#     summarize(
#       expected_return_orig    = sum(expected_return_orig),
#       expected_return_current = sum(expected_return_current),
#       expected_value_orig     =  sum(expected_return_orig) - ticket_price,
#       expected_value_current  = sum(expected_return_current) - ticket_price
#     )
#   expected_value_o <- expected_values %>% pull(expected_value_orig)
#   expected_value_c <- expected_values %>% pull(expected_value_current)
#   
#   expected_value_c <-  tbl %>% 
#     summarize(
#       expected_return_current = 
#         ticket_price - sum(expected_return_current
#         )
#     ) %>%
#     pull(expected_return_current)
#   
#   return_obj <- bind_cols(game_number = x,
#                           expected_values,
#                           total_prizes = sum(tbl$total_prizes),
#                           remaining_prizes = sum(tbl$prizes_remaining))
# }
# 
# game_values <- purrr::map(games_enh$game_number, get_granular_info) %>% bind_rows()
# 
# games_enh <- games_enh %>% left_join(game_values) %>% 
#   filter(
#     !game_number %in% 
#       (count(games_enh, game_number) %>% filter(n>1) %>% pull(game_number)
#       )
#   )
# 
# output_table <-
#   games_enh %>% select(
#     game_name,
#     ticket_cost,
#     top_prize,
#     top_prizes,
#     top_prizes_remaining,
#     expected_value_orig,
#     expected_value_current
#   ) %>%
#   mutate(
#     expected_value_orig = as.numeric(prettyNum(expected_value_orig, digits = 3, format = 'f')),
#     expected_value_current = as.numeric(prettyNum(expected_value_current, digits = 3, format = 'f'))
#   )
# 
# games_enh <- games_enh %>% mutate(AOdate = today())

get_granular_info_prizes <- function(x) {
  page <- xml2::read_html(paste0(game_url, x)) 
  
  tbl <- page %>%
    html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "column2", " " ))]') %>% 
    html_nodes('table') %>% 
    html_table() %>% .[[1]] %>% clean_names() %>%
    mutate(game_number = x, 
           prize_amount = parse_number(prize_amount),
           odds_denominator = parse_number(
             str_remove(
               odds_of_winning, 
               '1-in-')
           ),
           odds_of_winning = 1/odds_denominator
    )
  
  text <- page %>%
    html_nodes(xpath = '//p') %>% 
    html_text()
  
  overall_odds <- text[grepl('Overall Odds', text)]
  overall_odds_d <- as.numeric(
    stringr::str_extract(
      overall_odds,
      '(?<=Overall Odds: 1-in-).*'
    )
  )
  
  num_printed_tickets <-
    tbl %>% 
    slice(1) %>% 
    summarize(total_tickets = total_prizes*odds_denominator) %>% 
    pull(total_tickets)
  
  losing_tix <- num_printed_tickets - sum(tbl$total_prizes) 
  # ((1 - (1 / overall_odds_d)) * sum(tbl$total_prizes)) + sum(tbl$total_prizes)
  num_current_tickets_losing = (sum(tbl$prizes_remaining))/sum(tbl$total_prizes) * losing_tix
  num_current_tickets_total = num_current_tickets_losing + sum(tbl$prizes_remaining)
  p_0 <- 1 - sum(tbl$odds_of_winning)
  
  ticket_price <- text[grepl('Ticket Price', text)]
  ticket_price <- as.numeric(
    stringr::str_extract(
      ticket_price,
      '(?<=Ticket Price:\\s\\$).*(?=L)'
    )
  )
  
  tbl <- tbl %>% mutate(expected_return_orig = odds_of_winning * prize_amount,
                        current_odds = prizes_remaining/num_current_tickets_losing,
                        expected_return_current = current_odds * prize_amount)
  
  # expected_values <- tbl %>%
  #   summarize(
  #     expected_return_orig    = sum(expected_return_orig),
  #     expected_return_current = sum(expected_return_current),
  #     expected_value_orig     =  sum(expected_return_orig) - ticket_price,
  #     expected_value_current  = sum(expected_return_current) - ticket_price
  #   )
  # expected_value_o <- expected_values %>% pull(expected_value_orig)
  # expected_value_c <- expected_values %>% pull(expected_value_current)
  # 
  # expected_value_c <-  tbl %>% 
  #   summarize(
  #     expected_return_current = 
  #       ticket_price - sum(expected_return_current
  #       )
  #   ) %>%
  #   pull(expected_return_current)
  # 
  # return_obj <- bind_cols(game_number = x,
  #                         expected_values,
  #                         total_prizes = sum(tbl$total_prizes),
  #                         remaining_prizes = sum(tbl$prizes_remaining))
  
  return_obj <- tbl
}

game_prizes <- purrr::map_df(games_enh$game_number, get_granular_info_prizes) %>% mutate(
  AOdate = today()
)

game_summaries <-
  game_prizes %>% ungroup() %>% 
  left_join(
    select(games_enh, game_number, ticket_cost)
  ) %>%
  group_by(game_number, AOdate) %>% 
  summarize(
    expected_return_orig    = sum(expected_return_orig),
    expected_return_current = sum(expected_return_current),
    expected_value_orig     =  sum(expected_return_orig) - unique(ticket_cost),
    expected_value_current  = sum(expected_return_current) - unique(ticket_cost),
    total_prizes = sum(total_prizes),
    prizes_remaining = sum(prizes_remaining)
  ) %>% ungroup()


game_info <- games_enh %>% 
  select(game_number,
         game_name,
         top_prize,
         ticket_cost,
         reorder,
         last_day_to_sell,
         last_day_to_redeem,
         ending
         ) %>% 
  mutate(AOdate = today())
