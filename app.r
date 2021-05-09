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
library(DT)
library(janitor)

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
          column(
            width = 4,
            box(
              width = NULL,
              h3("Welcome!"),
              h4("This dashboard will grab information from",
              tags$a(href="https://genius.com/", "Genius"),
              "based on provided inputs, and conduct sentiment analysis."),
              "You can grab information for multiple albums from a single artist, or multiple albums from different artists.",
              HTML("You can also filter out \"dropwords\" (words that might overwhelm word-by-word analysis that otherwise aren't stopwords).<br>"),
              "Due to the nature of the function used to grab the lyric data, some songs might not have their lyric data properly loaded,
              since Genius is not always consistent with URLs."
            ),
            box(
              width = NULL,
              title = "Author",
              "This dashboard was created by",
              tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
              "in May 2021 as a final project in Reed College Math 241 - Data Science."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Dropwords",
              textInput("input_dropwords", label = "Input dropwords, separated by commas:"),
              HTML("When you have finished inputting your dropwords, press the button below!
                   You do not need to press the \"Get lyrics\" button again.<br><br>"),
              actionButton(
                inputId = "action_dropwords",
                label = "Submit dropwords!",
                icon = icon("hand-point-right"),
                class = "btn-warning")
              # h3("Top tokens"),
              # "These are the highest count non-stopwords in the data. Should some be included in the list of dropwords?",
              # tableOutput("output_top_tokens")
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Artist",
              textInput("input_artist", label = "Input artist name(s):", value = "New Order, Beastie Boys"),
              "If you are just looking at one artist's works, you only need to put their name once.
              If you are comparing albums from multiple artists, write them respective to their album as you list them below."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Albums",
              textInput("input_albums", label = "Input albums, separated by commas:", value = "Power Corruption and Lies, Licensed To Ill"),
              HTML("Capitalization does not matter, but spelling does. Exclude commas in album titles that contain them
                   (for example, instead of inputting <i>For Emma, Forever Ago</i>, use <i>For Emma Forever Ago</i>).")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              HTML("Press the button below once artists and albums have been entered!
                   The data gathering process will take a few moments. See the BTS tab for progress.<br><br>"),
              actionButton(
                inputId = "action_grab",
                label = "Get lyrics!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_bts",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Artist(s) and albums",
              tableOutput("output_artist_album")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Inputted dropwords",
              textOutput("output_dropwords"),
              tags$hr(),
              "If you need to add more dropwords, return to the Inputs tab and remember to press the \"Submit Dropwords\" button."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Top tokens",
              "These are the top five non-stopwords in the data by album. Should some be included in the list of dropwords?",
              tableOutput("output_top_tokens")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "Dataframe of lyrics",
              dataTableOutput("output_lyrics")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output){
  
    
  df_artist_album <- eventReactive(input$action_grab, {
    data.frame(artist = c(unlist(str_split(input$input_artist, ", "))),
               album = c(unlist(str_split(input$input_albums, ", "))))
  })
  
  output$output_artist_album <- renderTable({df_artist_album()})
  
  df_lyrics <- eventReactive(input$action_grab, {
    withProgress(message = "Gathering data from Genius...",
                 detail = "This will be the longest step.",
                 df_artist_album() %>% 
                   add_genius(artist, album, type = "album")
    )
  })
  
  df_tokenized <- eventReactive(input$action_grab, {
    withProgress(message = "Tokenizing data",
                 df_lyrics() %>% 
                   unnest_tokens(output = word, input = lyric, token = "words")
    )
  })
  
  output$output_lyrics <- renderDataTable({
    datatable(style = "bootstrap",
              #class = 'table-bordered table-condensed',
              filter = list(position = "top", plain = TRUE),
              df_lyrics() %>% 
                select(artist, album, track_n, track_title, line, lyric) %>% 
                mutate(
                  artist = as.factor(artist),
                  album = as.factor(album),
                  track_title = as.factor(track_title)
                ) %>% 
                rename("No" = track_n) %>% 
                clean_names(case = "title")
    )
  })
  
  output$output_top_tokens <- renderTable({
    df_tokenized() %>% 
      anti_join(stop_words, by = "word") %>%
      drop_na(word) %>% 
      count(artist, album, word) %>% 
      group_by(artist, album) %>% 
      slice_max(order_by = n, n = 5)
  })
  
  
  
  # observeEvent(input$action_dropwords, {
  #   output$dropwords <- renderText(c(unlist(str_split(input$input_dropwords, ", "))))
  #   # df_tokenized <- reactive({df_tokenized() %>% 
  #   #   filter(!word %in% c(unlist(str_split(input$input_dropwords, ", "))))
  #   #   })
  #   })
  
  list_drop <- eventReactive(input$action_dropwords, {
    c(unlist(str_split(input$input_dropwords, ", ")))
  })
  
  output$output_dropwords <- renderText({list_drop()})
  
  
  
  
}

shinyApp(ui, server)