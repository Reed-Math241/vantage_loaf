# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(genius)
library(tidytext)
library(sentimentr)
library(magrittr)
library(ggthemes)
library(viridis)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Vantage loaf"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "tab_inputs", icon = icon("i-cursor")),
      menuItem("Behind the scenes", tabName = "tab_bts", icon = icon("search")),
      menuItem("Wordclouds", tabName = "tab_cloud", icon = icon("cloud")),
      menuItem("NRC sentiment analysis", tabName = "tab_nrc", icon = icon("smile")),
      menuItem("AFINN word analysis", tabName = "tab_afinn", icon = icon("stopwatch")),
      menuItem("Line-by-line sentiments", tabName = "tab_lyric", icon = icon("align-left"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_inputs",
        fluidRow(
          column(width = 4,
          box(width = NULL,
            h3("Welcome!"),
            h4("This dashboard will grab information from Genius based on your inputs to the right, and conduct sentiment analysis."),
            "You can grab information for multiple albums from a single artist, or multiple albums from different artists."
          ),
          box(width = NULL,
            title = "Author",
            "This dashboard was created by",
            tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
            "in May 2021."
          )),
          column(width = 4,
          box(width = NULL, solidHeader = TRUE, background = "black", status = "warning",
            title = "Artist",
            textInput("input_artist", label = "Input artist name(s):", value = "New Order, Beastie Boys"),
            "If you are just looking at one artist's works, just put their name once.
            If you are comparing albums from multiple artists, write them respective to their album as you list them below."
          ),
          box(width = NULL, solidHeader = TRUE, background = "black", status = "warning",
            title = "Albums",
            textInput("input_albums", label = "Input albums, separated by commas:", value = "Power Corruption and Lies, Licensed To Ill"),
            HTML("Capitalization does not matter, but spelling does.
            Exclude commas in album titles that contain them
            (for example, instead of inputting <i>For Emma, Forever Ago</i>, use <i>For Emma Forever Ago</i>).")
          ),
          actionButton(inputId = "action_grab",
                       label = "Get lyrics!",
                       icon = icon("hand-point-right"),
                       class = "btn-warning"),
          #submitButton("Get lyrics!", icon("hand-point-right")),
          "This process might take a minute or so, especially if there are many inputted albums.
          See the BTS tab on the left for progress."
        ))
      ),
      tabItem(
        tabName = "tab_bts",
        fluidRow(
          box(
            h2("List of albums"),
            tableOutput("artist_album"),
            tableOutput("lyrics")
            # tableOutput("albums"),
            # textOutput("list_albums"),
            # tableOutput("lyrics")
          )
        )
      )
    )
  )
)

server <- function(input, output){
  
  df_artist_album <- reactive({
    input$action_grab
    isolate(
    data.frame(
      artist = c(unlist(str_split(input$input_artist, ", "))),
      album = c(unlist(str_split(input$input_albums, ", ")))
    )
  )
  })
  
  output$artist_album <- renderTable({df_artist_album()})
  
  df_lyrics <- reactive({
    withProgress(message = "Gathering data",
    df_artist_album() %>% 
      add_genius(artist, album, type = "album")
    )
  })
  
  output$lyrics <- renderTable({df_lyrics()})
  
  # df_albums <- reactive({
  #   as.data.frame(c(unlist(str_split(input$input_albums, ", ")))) %>% 
  #     rename("album_title" = 1) %>% 
  #     rowid_to_column() %>%
  #     mutate(album = as.character(rowid))
  # })
  # output$albums <- renderTable({df_albums()})
  # 
  # list_albums <- reactive({c(unlist(str_split(input$input_albums, ", ")))})
  # 
  # df_lyrics <- reactive({
  #   list_albums() %>%
  #     map_dfr(.f = genius_album, artist = input$input_artist, .id = "album") %>%
  #     left_join(list_albums) %>%
  #     select(!album) %>%
  #     rename(album = albums_input) %>%
  #     mutate(album = fct_relevel(as.factor(album), list_albums()))
  # })
  # 
  # output$lyrics <- renderTable({df_lyrics()})
  
  tokenized_df <- df_lyrics() %>% 
    unnest_tokens(output = word, input = lyric, token = "words")
  # 
  # output$wordclouds <- tokenized_df() %>% 
  #   anti_join(stop_words, by = "word") %>%
  #   count(word, sort = TRUE) %>%
  #   filter(!word %in% {{drop_words}}) %>%
  #   with(wordcloud(word,
  #                  n,
  #                  min.freq = {{min_freq}},
  #                  random.order = {{random_order}},
  #                  colors = {{color_pal}}))
  
  
}

shinyApp(ui, server)