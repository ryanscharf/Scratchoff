# db connection and querying -------------------------------------------------
scratch_db <- function(conn) {
  if (
    exists(
      deparse(substitute(conn))
    ) ==
      F
  ) {
    con <- dbPool(
      RPostgres::Postgres(),
      host = conn_args$host,
      port = conn_args$port,
      user = conn_args$username,
      password = conn_args$password,
      dbname = conn_args$db,
      minSize = conn_args$minSize,
      idleTimeout = conn_args$idleTimeout
    )
    cat('connection didnt exist \n')
    con
  } else {
    cat('connection was fine \n')
    conn
  }
}

# ev change ---------------------------------------------------------------

tf <- function(ad = aodate()) {
  cat(ad)
}

plot_ev_change <- function(
  game_number, # = selected_game_number(),
  asofdate, #= aodate(),
  lag = 3,
  con
) {
  asofdate <- ymd(asofdate)

  lagdate <- asofdate %m-% months(lag, abbreviate = F)

  game_summary <-
    dbGetQuery(
      con,
      glue_sql(
        'SELECT * FROM game_summaries
         WHERE game_number = {game_number}
         AND "AOdate" >= {lagdate}
         ORDER BY "AOdate" DESC',
        .con = con
      )
    )

  game_summary <-
    game_summary %>% mutate(AOdate = ymd(AOdate))

  game_summary <- game_summary %>%
    complete(
      AOdate = seq(
        min(
          game_summary$AOdate
        ),
        asofdate,
        by = "1 day"
      )
    ) %>%
    mutate(
      missing_data = case_when(
        !is.na(game_number) ~ 'not_missing',
        T ~ 'missing'
      ),
      expected_value_current = na.approx(expected_value_current),
      missing_data = factor(missing_data, levels = c('not_missing', 'missing'))
    )
  missing_dates <- game_summary %>% filter(missing_data == 'missing')

  # Only try to complete if there are missing dates
  if (nrow(missing_dates) > 0) {
    missing_dates <- missing_dates %>%
      complete(
        AOdate = seq(
          min(missing_dates$AOdate) - 1,
          max(missing_dates$AOdate) + 1,
          by = "1 day"
        )
      )

    missing_dates <- game_summary %>% filter(AOdate %in% missing_dates$AOdate)
  } else {
    # No missing dates, create empty dataframe
    missing_dates <- game_summary %>% filter(FALSE)
  }

  not_missing_dates <- game_summary %>%
    filter(missing_data == 'not_missing') %>%
    complete(
      AOdate = seq(min(game_summary$AOdate), ymd(asofdate), by = "1 day")
    )

  ggplot() +
    geom_line(
      data = not_missing_dates,
      aes(x = AOdate, y = expected_value_current),
      na.rm = T,
      linetype = 'solid'
    ) +
    geom_line(
      data = missing_dates,
      aes(x = AOdate, y = expected_value_current),
      linetype = 'dashed'
    ) +
    theme_few() +
    labs(x = 'Date', y = 'Expected Value') +
    ggtitle('Expected Value Over Time')
}


# prizes left -------------------------------------------------------------

plot_prizes_left <- function(df = prize_data()) {
  df %>%
    select(
      game_number,
      prize_amount,
      total_prizes,
      prizes_remaining,
      AOdate
    ) %>%
    mutate(prizes_claimed = total_prizes - prizes_remaining) %>%
    pivot_longer(
      cols = c('prizes_claimed', 'prizes_remaining'),
      names_to = 'prize_split',
      values_to = 'prize_value'
    ) %>%
    mutate(
      prize_split = factor(
        prize_split,
        levels = c('prizes_claimed', 'prizes_remaining')
      )
    ) %>%
    ggplot() +
    geom_col(
      aes(
        x = as.factor(prize_amount),
        y = prize_value * as.integer(prize_amount),
        fill = prize_split
      ),
      color = 'black'
    ) +
    labs(x = 'Prize Amount', y = 'Remaining Value') +
    theme_few() +
    scale_y_continuous(labels = scales::dollar_format()) +
    ggtitle('Remaining Prizes') +
    scale_fill_manual(values = c(NA, 'grey'), na.value = NA) +
    theme(legend.position = 'none')
}


# game summary table ------------------------------------------------------

game_overview_table <- function(aodate, conn = con, positive_ev = T) {
  # Convert aodate to string if it's a Date object
  aodate_str <- as.character(aodate)

  query <- glue_sql(
    "SELECT 
      a.game_number, 
      a.\"AOdate\", 
      b.game_name, 
      a.expected_value_current, 
      a.expected_return_current/b.ticket_cost as percent_ev,
      b.ticket_cost, 
      c.total_prizes as total_top_prizes,
      c.prizes_remaining as top_prizes_remaining, 
      b.last_day_to_sell
    FROM
      game_summaries a
    inner JOIN
      game_info b ON a.game_number = b.game_number 
        AND a.\"AOdate\" = b.\"AOdate\"
    inner JOIN
      game_prizes c ON a.game_number = c.game_number 
        AND c.prize_amount = b.top_prize
        AND c.\"AOdate\" = a.\"AOdate\"
    WHERE
      CAST(c.\"AOdate\" AS DATE) = {aodate_str}
      AND CAST(b.\"AOdate\" AS DATE) = {aodate_str}
      AND CAST(a.\"AOdate\" AS DATE) = {aodate_str}
    ORDER BY expected_value_current DESC",
    .con = conn
  )

  result <- dbGetQuery(conn, query)
  result <- result %>%
    mutate(
      expected_value_current = parse_number(
        format(
          round(expected_value_current, 2),
          nsmall = 2
        )
      )
    )

  if (positive_ev == T) {
    result <- result %>% filter(expected_value_current > 0)
  }

  return(result %>% mutate(pic_url = get_scratcher_pics(game_number)))
}


# get scrachoff image url -------------------------------------------------

get_scratcher_pic <- function(game_number) {
  tryCatch(
    {
      b <- glue(
        'https://floridalottery.com/content/flalottery-web/us/en/games/scratch-offs/view.scratch-offs.{game_number}.json'
      ) %>%
        request() %>%
        req_headers('X-Partner' = 'web') %>%
        req_perform() %>%
        resp_body_json()

      pic <- paste0('https://floridalottery.com', b[['data']][['ticketFront']])

      # Wrap the image in an <a> tag to trigger JavaScript function
      pic_html <- glue(
        "<a href='#' onclick='showModal(\"{pic}\")'>
         <img src='{pic}' alt='Game {game_number}' width='100' />
       </a>"
      )

      return(pic_html)
    },
    error = function(e) {
      return("<span>Image not available</span>")
    }
  )
}


get_scratcher_pics <- function(game_numbers) {
  map_chr(game_numbers, get_scratcher_pic)
}
