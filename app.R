library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(mongolite)
library(shiny)
library(DT)

## sandbox db url
mongo_url <- "mongodb+srv://bs:bs@cluster0.cgwmf.mongodb.net/steam_db?retryWrites=true&w=majority"
games_col <- mongo(collection = "steam_games", db = "steam_db", url = mongo_url)
tags_col  <- mongo(collection = "steam_tags", db = "steam_db", url = mongo_url)

ui <- 
dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "steamRecs"
  ),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    setBackgroundImage(
      src = "bg.jpg", shinydashboard = TRUE),
    tags$script(src = "enter_button.js"),
    fluidRow(
      box(
        solidHeader = TRUE,
        width = 3,
        fluidRow(
          column(width = 12,
                 textInput(inputId = "game_search",
                           label = "Find game"))),
        fluidRow(
          column(width = 2,
                 actionButton(inputId = "game_search_button",
                              label = "Find")),
          column(width = 2,
                 actionButton(inputId = "add_games",
                              label = "Add")),
          column(width = 2,
                 actionButton(inputId = "clear_button",
                              label = "Clear"))),
        fluidRow(
          column(
            width = 12,
            br(),
            checkboxGroupInput(inputId = "games_list",
                               label = "Select games",
                               choiceNames = NULL,
                               choiceValues = NULL,
                               inline = FALSE,
                               width = "100%")))
      ),
      box(
        solidHeader = TRUE,
        width = 3,
        fluidRow(
          column(width = 12,
                 p("Find games similar to:"), br(),
                 DTOutput(outputId = "u1_list"))
        )
      ),
      box(
        solidHeader = TRUE,
        width = 3,
        fluidRow(
          column(
            width = 12,
            p("Recommendations:"), br(),
            DTOutput(outputId = "recs")
          )
        )
      )
    )
  )
)

server <- function(session, input, output) {
  query_build_fn <- function(ids) {
    paste0(
      '{\"', ids[1], '\" : -1,\"', ids[2], '\": -1, \"',
      ids[3], '\": -1, \"', ids[4], '\": -1, \"', ids[5],
      '\": -1, \"', ids[6], '\" : -1}'
    )
  }
  
  reactVal <- reactiveValues()
  observeEvent(input$game_search_button, {
    if(nchar(input$game_search) >= 2) {
      reactVal$query_results <- games_col$find(paste0("{\"name\" : {\"$regex\": \"", input$game_search,"\", \"$options\" : \"i\"}}"), 
                                               fields = '{"name" : true, "appid" : true, "_id": false}',
                                               limit = 25) %>% tibble()
    }
    updateCheckboxGroupInput(
      session,
      inputId = "games_list",
      choiceNames = reactVal$query_results$name,
      choiceValues = reactVal$query_results$appid
    )
  })
  
  observeEvent(input$add_games, {
    if (!is.null(reactVal$appid_list)) {
      reactVal$appid_list <- unique(bind_rows(reactVal$appid_list, reactVal$query_results %>%
                                                filter(appid %in% input$games_list)))
    } else {
      reactVal$appid_list <- reactVal$query_results %>%
        filter(appid %in% input$games_list)
    }
  })
  
  observeEvent(input$clear_button, {
    updateCheckboxGroupInput(
      session,
      inputId = "games_list",
      selected = ""
    )
    reactVal$appid_list <- NULL
  })
  
  observe({
    output$u1_list <- renderDT({
      datatable(
        reactVal$appid_list,
        rownames  = FALSE,
        selection = "none",
        style     = "bootstrap",
        class      = "cell-border stripe hover",
        options    = list(
          ordering = FALSE,
          dom      = ""
        )
      )
    })
    
    input_ids <- try(
      tags_col$find(paste0('{ \"appid\": { \"$in\": [', paste0(reactVal$appid_list$appid %>% as.character(), collapse = ", "),'] }}')) %>% 
        mutate(appid = as.integer(appid)) %>% colSums() %>% data.frame() %>%
        rownames_to_column() %>% arrange(desc(.)) %>% na_if(0) %>% drop_na() %>% 
        filter(rowname != "appid") %>% plyr::rename(c("rowname" = "tag", "." = "score")) %>% 
        top_n(6) %>% pull(tag)
    )
    
    rec_ids <- try(
      tags_col$find(
        query = paste0('{\"appid\": { \"$nin\": [', paste0(reactVal$appid_list$appid %>% as.character(), collapse = ", "), ']}}'),
        sort = query_build_fn(input_ids),
        fields = '{"appid" : true}',
        limit = 10
      ) %>% pull(appid)
    )
    recs <- try(
      games_col$find(paste0('{ \"appid\": { \"$in\": [', paste0(rec_ids, collapse = ", "),'] }}'),
                     fields = '{"appid": true, "name": true, "release_date": true, "_id": false}') %>%
        tibble()
    )
      
    output$recs <- renderDT({
      validate(
        need(nrow(recs) > 0, message = FALSE)
      )
      datatable(
        recs,
        rownames  = FALSE,
        selection = "none",
        style     = "bootstrap",
        class      = "cell-border stripe hover",
        options    = list(
          ordering = FALSE,
          dom      = ""
        )
      )
    })
    if (length(input$games_list) > 0) {
      shinyjs::enable("add_games")
    } else {
      shinyjs::disable("add_games")
    }
  })
}

shinyApp(ui, server)