library(dplyr)
library(readr)
library(stringr)
library(jsonlite)
library(janitor)
library(lubridate)
library(RMariaDB)
library(DBI)
library(httr2)
library(purrr)
library(pool)

conn_args <- config::get("writeconnection")

ending_games <- tryCatch({
  response <- "https://apim-website-prod-eastus.azure-api.net/scratchgamesapp/getEndingGames" %>% 
    request() %>% 
    req_headers('X-Partner' = 'web') %>%
    req_perform() %>% 
    resp_body_json()
  
  # Check if the response is empty (an empty array)
  if (length(response) == 0) {
    # Return an empty dataframe with the expected columns
    data.frame(
      game_number = integer(),
      last_day_to_sell = as.Date(character()),
      last_day_to_redeem = as.Date(character()),
      stringsAsFactors = FALSE
    )
  } else {
    # Process as normal if there's data
    response %>%
      bind_rows() %>% 
      clean_names() %>% 
      mutate(
        game_number = id,
        last_day_to_sell = ymd(ymd_hms(last_day_to_sell)),
        last_day_to_redeem = ymd(ymd_hms(last_day_to_redeem))
      ) %>% 
      select(game_number, last_day_to_sell, last_day_to_redeem)
  }
}, error = function(e) {
  # Handle any errors by returning an empty dataframe
  message("Error fetching data: ", e$message)
  data.frame(
    game_number = character(),
    last_day_to_sell = as.Date(character()),
    last_day_to_redeem = as.Date(character()),
    stringsAsFactors = FALSE
  )
})

#ending games do not show up on https://floridalottery.com/games/scratch-offs/top-remaining-prizes
# but do show up in the API table

replace_string_null_with_na <- function(x) {
  if (any(x$TopPrizes == "null")) {
    x$TopPrizes <- NA
  }
  return(x)
}

games <- "https://apim-website-prod-eastus.azure-api.net/scratchgamesapp/getTopPrizesRemaining" %>%
  request() %>% 
  req_headers('X-Partner' = 'web') %>%
  req_perform() %>% 
  resp_body_json() %>%
  map(replace_string_null_with_na) %>% discard(~is.na(.x['TopPrizes'])) %>%
  #try to replace the 'null' TopPrizes with NA
  #bind_rows keeps dropping rows. IT converts NAs to NULLs
  # map_depth(1, replace_x) %>%
  bind_rows() %>%
  mutate(
    bind_rows(TopPrizes)
  ) %>% 
  clean_names() %>% 
  select(-top_prizes) %>%
  rename(game_number = id) 

games_cleaned <- games %>%
  transmute(
    game_number,
    game_name,
    top_prize = parse_number(top_prize),
    ticket_cost = ticket_price,
    reorder = if_else(str_detect(top_prizes_remaining, fixed('*')), T, F),
    total_top_prizes = str_extract(
      str_replace_all(
        str_replace_all(top_prizes_remaining, ',' ,''),
        fixed('*'), ''
      ),
      "(?<= of ).*"),
    total_top_prizes = parse_number(total_top_prizes),
    top_prizes_remaining = parse_number(top_prizes_remaining, "[:digits:]*"),
  ) %>% 
  left_join(ending_games, by = 'game_number') %>% 
  group_by(game_number) %>% 
  filter(top_prize == max(top_prize))

games_enh <-
  games_cleaned %>%
  mutate(
    top_percent_remaining = top_prizes_remaining / total_top_prizes,
    ending = (game_number %in% ending_games$game_number)
  )


game_url <-  'https://www.flalottery.com/scratch-offsGameDetails?gameNumber='

calc_ev <- function(cost, odds_denomintor, win){
  ev <- (win * 1/odds_denomintor) - (cost * (1-1/odds_denomintor))
  return(ev)
}

get_granular_info_prizes <- function(x) {
  
  game_dts <- "https://apim-website-prod-eastus.azure-api.net/scratchgamesapp/getscratchinfo" %>% 
    request() %>% 
    req_url_path_append(paste0('?id=',x)) %>%
    req_headers('X-Partner' = 'web',
                Host = 'apim-website-prod-eastus.azure-api.net') %>%
    req_perform() %>% 
    resp_body_json()
  
  tbl <- game_dts$OddsTiers %>%
    bind_rows() %>% 
    clean_names() %>%
    mutate(prize_amount = parse_number(prize_amount),
           odds_denominator = str_extract(winning_odds, '(?<=-in-)[:digit:]+'),
           odds_denominator = parse_number(odds_denominator),
           odds_of_winning = 1/odds_denominator,
    ) %>%
    select(-winning_odds)
  
  overall_odds_d <- game_dts$OverallOdds
  
  #youtube.com/watch?v=MnKtVlULf3E
  
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
  
  ticket_price <- game_dts$TicketPrice
  
  tbl <- tbl %>% mutate(expected_return_orig = odds_of_winning * prize_amount,
                        current_odds = prizes_remaining/num_current_tickets_total,
                        expected_return_current = current_odds * prize_amount,
                        num_current_tickets_losing,
                        num_current_tickets_total)
  
  return_obj <- tbl %>% mutate(game_number = x) %>%
    select(game_number, everything())
}

game_prizes <- purrr::map_df(games_enh$game_number, get_granular_info_prizes) %>% mutate(
  AOdate = today()
) %>% select(-prizes_paid)

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
    prizes_remaining = sum(prizes_remaining),
    num_current_tickets_losing = as.integer(unique(num_current_tickets_losing)),
    num_current_tickets_total = as.integer(unique(num_current_tickets_total))
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
  ) %>% ungroup() %>% 
  mutate(AOdate = today())

game_prizes <- game_prizes %>% select(-num_current_tickets_losing, -num_current_tickets_total)

con <- dbPool(RMariaDB::MariaDB(),
              host = conn_args$host, 
              port = conn_args$port, 
              username = conn_args$username,
              password = conn_args$password, 
              db = conn_args$db,
              minSize = conn_args$minSize,
              idleTimeout = conn_args$idleTimeout
)

dbWriteTable(con, 'game_prizes', game_prizes, append = T, overwrite = F, row.names = F)
cat(paste0("wrote ", nrow(game_prizes)," rows to game_prizes\n"))

dbWriteTable(con, 'game_summaries', game_summaries, append = T, overwrite = F, row.names = F)
cat(paste0("wrote ", nrow(game_summaries)," rows to game_summaries\n"))

dbWriteTable(con, 'game_info', game_info, append = T, overwrite = F, row.names = F)
cat(paste0("wrote ", nrow(game_info)," rows to game_info\n"))

