library(shiny)
library(DBI)
library(pool)
library(RPostgres)
library(tidyverse)
library(httr2)
library(glue)
library(lubridate)
library(shinycssloaders)
library(DT)
library(ggthemes)
library(zoo)

source('utils.R')

conn_args <- config::get("dataconnection")

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

AOdates <- dbGetQuery(
  con,
  'SELECT DISTINCT "AOdate"
FROM game_info 
ORDER BY "AOdate" DESC
LIMIT 25'
)

# Convert to vector for selectInput
AOdates <- AOdates$AOdate

# the dashboard -----------------------------------------------------------

shinyApp(
  ui = fluidPage(
    #javascript magic
    tags$head(
      tags$script(HTML(
        "
  function showModal(imgSrc) {
    var modal = document.getElementById('imageModal');
    var modalImg = document.getElementById('modalImage');
    modal.style.display = 'block';
    modalImg.src = imgSrc;
  }

  function closeModal() {
    document.getElementById('imageModal').style.display = 'none';
  }

  // Close modal if clicking outside the image
  document.addEventListener('click', function(event) {
    var modal = document.getElementById('imageModal');
    var modalImg = document.getElementById('modalImage');
    if (event.target === modal) {
      closeModal();
    }
  });
"
      )),
      tags$style(HTML(
        "
  #imageModal {
    display: none;
    position: fixed;
    z-index: 1000;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(0,0,0,0.8);
    text-align: center;
  }

  #imageModal img {
    max-width: 80%;
    max-height: 80%;
    margin-top: 10%;
  }

  #imageModal .close {
    position: absolute;
    top: 20px;
    right: 35px;
    font-size: 40px;
    color: white;
    cursor: pointer;
  }
"
      ))
    ),

    # Modal div (Hidden by default)
    tags$div(
      id = "imageModal",
      tags$img(id = "modalImage")
    ),

    tabsetPanel(
      tabPanel(
        "Games Overview",
        fluidRow(mainPanel(
          withSpinner(DTOutput('game_overview_table'))
        ))
      ),
      tabPanel(
        "Game Information",
        fluidRow(
          column(
            3,
            selectInput('aodates', 'Date: ', AOdates),
            # selectInput(
            #   'game_names',
            #   'Game: ',
            #   avaliable_games)
            uiOutput('avaliable_games'),
            numericInput('lag', 'Time span (months): ', value = 3)
          ),
          mainPanel(
            withSpinner(plotOutput('prizes_left_graph')),
            withSpinner(plotOutput('ev_change_graph')),
            textOutput('test1')
          )
        )
      )
    )
  ),

  server = function(input, output) {
    aodate <- reactive({
      input$aodates
    })
    selected_game_number <- reactive({
      input$game_names %>% str_extract('.+(?= -)')
    })
    # selected_game_name <- reactive({ input$game_names %>% str_extract('(?<=- ).+') })
    selected_lag <- reactive({
      input$lag
    })
    game_info <- reactive({
      aodate_val <- aodate()
      dbGetQuery(
        con,
        glue_sql(
          'SELECT DISTINCT
        a.game_number, b.game_name, a.expected_value_current
      FROM 
        game_summaries a
      LEFT JOIN
        game_info b ON a.game_number = b.game_number 
          AND a."AOdate" = b."AOdate"
      WHERE
        CAST(a."AOdate" AS DATE) = CAST({aodate_val} AS DATE)
      ORDER BY expected_value_current DESC',
          .con = con
        )
      )
    })

    prize_data <- reactive({
      game_num <- selected_game_number()
      aodate_val <- aodate()

      dbGetQuery(
        con,
        glue_sql(
          'SELECT * FROM game_prizes 
       WHERE game_number = {game_num}
       AND "AOdate" = {aodate_val}',
          .con = con
        )
      )
    })

    # outputs -----------------------------------------------------------------
    output$game_info_table <-
      renderDT(
        DT::datatable({
          game_info()
        })
      )

    output$test1 <- renderText({
      paste0('aodate = ', aodate(), '\nsgnumber = ', selected_game_number())
    })

    output$avaliable_games <- renderUI({
      selectInput(
        'game_names',
        'Game: ',
        paste0(game_info()$game_number, ' - ', game_info()$game_name)
      )
    })

    output$prizes_left_graph <- renderPlot({
      plot_prizes_left(prize_data())
    })

    output$ev_change_graph <- renderPlot({
      asofdate <- aodate()
      game_number <- selected_game_number()[1]
      lag <- selected_lag()
      # browser
      tryCatch(
        {
          plot_ev_change(
            asofdate = asofdate,
            game_number = game_number,
            con,
            lag = lag
          )
        },
        error = function(e) {
          ""
        } # Leave this line as-is.
      )
    })

    output$game_overview_table = renderDT(
      DT::datatable(
        game_overview_table(aodate = aodate(), con = con, positive_ev = F),
        escape = FALSE,
        options = list(lengthChange = FALSE, paging = FALSE)
      ) %>%
        formatRound(columns = c('expected_value_current'), digits = 2) %>%
        formatPercentage(columns = c('percent_ev'), digits = 0)
    )
  }
)
