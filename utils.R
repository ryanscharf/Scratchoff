

# db connection and querying -------------------------------------------------
scratch_db <- function(conn){
  if(exists(
    deparse(substitute(conn))
    ) == F){

    con <- dbPool(RMariaDB::MariaDB(),
                  host = conn_args$host, 
                  port = conn_args$port, 
                  username = conn_args$username,
                  password = conn_args$password, 
                  db = conn_args$db,
                  minSize = conn_args$minSize,
                  idleTimeout = conn_args$idleTimeout
    )
  cat('connection didnt exist \n')
  con
  # } else if (dbIsValid(con) == F ) {
  #   cat('connection was dead \n')
  #   
  # con <- dbPool(RMariaDB::MariaDB(),
  #               host = conn_args$host, 
  #               port = conn_args$port, 
  #               username = conn_args$username,
  #               password = conn_args$password, 
  #               db = conn_args$db,
  #               minSize = conn_args$minSize,
  #               idleTimeout = conn_args$idleTimeout
  #)
  
  #   con
  } else {
    cat('connection was fine \n')
    conn 
    }
}

rquery <- function(conn = con, query){
  
  #conn <- scratch_db(conn)
  #conn <- con
  
  result <- dbGetQuery(conn, query)
  
  #dbDisconnect(conn)
  
  return(result)
}

# ev change ---------------------------------------------------------------

tf <- function(ad = aodate()){cat(ad)}

plot_ev_change <- function(game_number, # = selected_game_number(),
                           asofdate ,#= aodate(),
                           lag = 3,
                           con
                           ) {
  
  asofdate <- ymd(asofdate)
  
  lagdate <- asofdate %m-% months(lag, abbreviate = F)
  game_summary <-
    rquery(
      con,
      glue(
        "SELECT * FROM scratchoff.game_summaries
  where game_number = {game_number}
  and AOdate >= '{lagdate}'
  order by aodate desc"
      )
    )
  
  game_summary <-
    game_summary %>% mutate(AOdate = ymd(AOdate)) 
  
  game_summary <- game_summary%>%
    complete(AOdate = seq(
      min(
        game_summary$AOdate), 
        asofdate, 
        by = "1 day")) %>%
    mutate(
      missing_data = case_when(!is.na(game_number) ~ 'not_missing', T ~ 'missing'),
      expected_value_current = na.approx(expected_value_current),
      missing_data = factor(missing_data, levels = c('not_missing', 'missing'))
    )
  missing_dates <- game_summary %>% filter(missing_data == 'missing')
  
  missing_dates <- missing_dates %>%
    complete(AOdate = seq(
      min(missing_dates$AOdate) - 1,
      max(missing_dates$AOdate) + 1,
      by = "1 day"
    ))
  
  missing_dates <- game_summary %>% filter(AOdate %in% missing_dates$AOdate)
  
  not_missing_dates <- game_summary %>%
    filter(missing_data == 'not_missing') %>%
    complete(AOdate = seq(min(game_summary$AOdate),
                          ymd(asofdate),
                          by = "1 day"))
  
  ggplot() +
    geom_line(
      data = not_missing_dates,
      aes(x = AOdate,
          y = expected_value_current),
      na.rm = T,
      linetype = 'solid'
    ) +
    geom_line(data = missing_dates,
              aes(x = AOdate,
                  y = expected_value_current),
              linetype = 'dashed') +
    theme_few() +
    labs(x = 'Date', y = 'Expected Value') +
    ggtitle('Expected Value Over Time')
  
}


# prizes left -------------------------------------------------------------

plot_prizes_left <- function(df = prize_data()){
  df %>% select(game_number, prize_amount, total_prizes, prizes_remaining, AOdate) %>%
    #group_by(game_number, prize_amount, AOdate) %>%  
    pivot_longer(
      cols = c('total_prizes', 'prizes_remaining'),
      names_to = 'prize_split', 
      values_to = 'prize_value') %>%
    mutate(prize_split = factor(prize_split,
                                levels = c('total_prizes','prizes_remaining')
    )
    ) %>% 
    #ungroup() %>% 
    ggplot() +
    geom_col(
      aes(x = as.factor(prize_amount),
          y = prize_value * as.integer(prize_amount),
          fill = prize_split
      ), color = 'black'
    )+
    labs(x = 'Prize Amount', y = 'Remaining Value') +
    theme_few() + 
    scale_y_continuous(labels = scales::dollar_format()) + 
    ggtitle('Remaining Prizes') + 
    scale_fill_manual(values = c(NA, 'grey')) +
    theme(legend.position = 'none')
}


# game summary table ------------------------------------------------------


game_overview_table <- function(aodate, con = con, positive_ev = T){
  
  query <- glue("SELECT distinct
    a.game_number, a.aodate, b.game_name, a.expected_value_current, 
    a.expected_return_current/b.ticket_cost as percent_ev,
    b.ticket_cost, c.total_prizes as total_top_prizes,
c.prizes_remaining as top_prizes_remaining, b.last_day_to_sell

FROM
    scratchoff.game_summaries a
        LEFT JOIN
    scratchoff.game_info b ON a.game_number = b.game_number
    left join
    scratchoff.game_prizes c on a.game_number = c.game_number and
    c.prize_amount = b.top_prize 
WHERE
    cast(c.AOdate as date) = CAST('{aodate}' AS DATE) and 
    cast(a.AOdate as date) = CAST('{aodate}' AS DATE) #and 
    #expected_value_current > 0 
    
ORDER BY expected_value_current DESC")
  
  result <- rquery(con, query)
  result <- result %>% mutate(expected_value_current = 
                                parse_number(
                                  format(
                                    round(expected_value_current, 2),
                                    nsmall = 2)
                                )
  )
  
  if(positive_ev == T){
    result <- result %>% filter(expected_value_current > 0)
  }
  
  return(result)
}
