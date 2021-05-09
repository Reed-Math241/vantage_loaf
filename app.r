# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(genius)
library(tidytext)
library(textdata)
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
              "You can grab information for an album or albums from a single artist, or multiple albums from different artists.",
              HTML("You can also filter out \"dropwords\" (words that might overwhelm word-by-word analysis that otherwise aren't stopwords).<br>"),
              tags$hr(),
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
                   The data gathering process will take a few moments. Check the BTS tab for progress.<br><br>"),
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
      ),
      tabItem(
        tabName = "tab_cloud",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Wordclouds",
              "Text goes here."
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_nrc",
        fluidRow(
          column(
            width = 5,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Sentiment anlaysis via NRC",
              "This analysis utilizes the",
              tags$a(href="https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm", "NRC Lexicon"),
              "to analyze the emotional and sentimental associations with the provided tokens.",
              HTML("<br>Along with positive and negative sentiments, it analyzes for 
                   anger, fear, anticipation, trust, surprise, sadness, joy, and disgust.<br><br>"),
              actionButton(
                inputId = "action_nrc",
                label = "Conduct NRC!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "Highest frequency words of each sentiment",
              dataTableOutput("output_nrc_top")
            )
          ),
          column(
            width = 6,
            plotOutput("output_nrc", height = "800px")
          )
        )
      ),
      tabItem(
        tabName = "tab_afinn",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "AFINN analysis",
              "The",
              tags$a(href="https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x", "AFINN sentiment lexicon"),
              HTML("analyzes words using a scale of -5 (most negative sentiment) to +5 (most positive sentiment).<br>
                   The button below will plot the AFINN results averaged by album based on words' relative position in each song. 
                   This way, one can compare if various albums are generally negative or positive, and generally where songs are most positive or negative.<br><br>"),
              actionButton(
                inputId = "action_afinn",
                label = "Generate AFINN!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            )
          ),
          column(
            width = 6,
            plotOutput("output_afinn", height = "800px")
          )
        )
      ),
      tabItem(
        tabName = "tab_lyric",
        fluidRow(
          column(
            width = 10,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Sentence analysis",
              "Conduct line-by-line sentiment analysis via the",
              tags$a(href="https://github.com/trinker/sentimentr", tags$code("sentimentr")),
              "package. Lines of positive sentiment are highlighted green, while negative are highlighted pink.",
              HTML("Average sentiment value by song are also included as numeric values.<br><br>"),
              actionButton(
                inputId = "action_line",
                label = "Generate sentence analysis!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              ),
              HTML("<br><br>Results will open in a new tab."),
              htmlOutput("output_lines")
            ),
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "How profane",
              HTML("Aggregating profanity by song<br><br>"),
              actionButton(
                inputId = "action_profanity",
                label = "Calculate profanity!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              ),
              dataTableOutput("output_profanity")
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
  
  # list_artist <- eventReactive(input$action_grab, {
  #   c(unlist(str_split(input$input_artist, ", ")))
  # })
  # 
  # list_album <- eventReactive(input$action_grab, {
  #   c(unlist(str_split(input$input_album, ", ")))
  # })
  
  output$output_artist_album <- renderTable({df_artist_album()})
  
  df_lyrics <- eventReactive(input$action_grab, {
    withProgress(message = "Gathering data from Genius...",
                 detail = "This will be the longest step.",
                 df_artist_album() %>% 
                   add_genius(artist, album, type = "album") %>% 
                   mutate(
                     # album = fct_relevel(as.factor(album), list_album()),
                     # artist = fct_relevel(as.factor(artist), list_album()),
                     album = fct_inorder(as.factor(album)),
                     artist = fct_inorder(as.factor(artist)),
                     track_title = fct_reorder(as.factor(track_title), track_n)
                     )
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
                rename("No" = track_n) %>% 
                clean_names(case = "title")
    )
  })
  
  eventReactive(input$action_dropwords, {
    df_tokenized <- df_tokenized() %>%
      filter(!word %in% list_drop())
  })
  
  output$output_top_tokens <- renderTable({
    df_tokenized() %>% 
      anti_join(stop_words, by = "word") %>%
      drop_na(word) %>% 
      count(artist, album, word) %>% 
      group_by(artist, album) %>% 
      slice_max(order_by = n, n = 5)
  })
  
  list_drop <- eventReactive(input$action_dropwords, {
    c(unlist(str_split(input$input_dropwords, ", ")))
  })
  
  output$output_dropwords <- renderText({list_drop()})
  
  
  line_by_line <- eventReactive(input$action_line, {
    df_lyrics() %>% 
      mutate(lyric_line = get_sentences(lyric)) %$%
      sentiment_by(lyric, list(track_title, album)) %>% 
      highlight()
  })
  
  output$output_lines <- renderText({line_by_line()})
  
  df_profanity <- eventReactive(input$action_profanity, {
    df_lyrics() %>% 
      get_sentences() %$% 
      profanity_by(lyric, list(track_title, album, artist)) %>% 
      arrange(desc(profanity_count))
  })
  
  output$output_profanity <- renderDataTable({
    datatable(style = "bootstrap",
              filter = list(position = "top", plain = TRUE),
              df_profanity() %>% 
                clean_names(case = "title")
    )
  })
  
  nrc_order <- c("positive", "negative",
                 "anger", "fear", "anticipation",
                 "trust", "surprise",
                 "sadness", "joy", "disgust")
  
  df_nrc <- eventReactive(input$action_nrc, {
    df_tokenized() %>% 
      inner_join(get_sentiments("nrc")) %>% 
      mutate(sentiment = fct_relevel(as.factor(sentiment), nrc_order))
  })
  
  output$output_nrc <- renderPlot({
    df_nrc() %>% 
      count(album, artist, sentiment) %>% 
      mutate(cat = case_when(
        (sentiment == "positive" | sentiment == "negative") ~ "Sentiment",
        TRUE ~ "Emotion")
      ) %>% 
      ggplot(aes(x = n,
                 y = sentiment,
                 fill = album,
                 color = artist)) +
      #facet_grid(vars(cat), vars(artist), scales = "free_y", space = "free_y") +
      facet_grid(vars(cat), scales = "free_y", space = "free_y") +
      geom_col(position = position_dodge2(reverse = TRUE)) +
      geom_text(aes(label=n), position = position_dodge2(0.9, reverse = TRUE), hjust = 1.25, color = "black")+
      theme_hc(style = "darkunica") +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        axis.text = element_text(color = "gray", size = 12)
      ) +
      labs(
        title = paste("NRC Sentiment Analysis"),
        x = "Word count",
        y = "",
        fill = "Album:",
        color = "Artist:"
      ) +
      scale_color_brewer(palette = "Dark2") +
      scale_fill_brewer(palette = "Set3")
    })
  
  df_nrc_top <- eventReactive(input$action_nrc, {
    df_nrc() %>% 
      count(album, artist, sentiment, word) %>% 
      mutate(sentiment = fct_relevel(as.factor(sentiment), nrc_order)) %>% 
      mutate(cat = case_when(
        (sentiment == "positive" | sentiment == "negative") ~ "Sentiment",
        TRUE ~ "Emotion")
      ) %>% 
      group_by(album, artist, sentiment) %>% 
      slice_max(order_by = n, n = 1)
  })
  
  output$output_nrc_top <- renderDataTable({
    datatable(style = "bootstrap",
              #class = 'table-bordered table-condensed',
              filter = list(position = "top", plain = TRUE),
              df_nrc_top() %>% 
                mutate(
                  artist = as.factor(artist),
                  album = as.factor(album),
                  cat = as.factor(cat)
                ) %>% 
                rename(
                  "category" = cat,
                  "frequency" = n
                ) %>% 
                clean_names(case = "title")
    )
  })
  
  plot_afinn <- eventReactive(input$action_afinn, {
    df_tokenized() %>% 
      group_by(album, artist, track_title) %>%
      mutate(index = round(line / max(line) * 100)) %>%
      inner_join(get_sentiments("afinn")) %>%
      ungroup() %>%
      count(album, index = index, wt = value) %>%
      ggplot(aes(x = index, y = n, fill = n)) +
      facet_grid(rows = vars(album), scales = "free_y") +
      geom_col() +
      theme_few() +
      labs(
        x = "Relative line position (% through song)",
        y = "Sentiment value",
        fill = "Sentiment value: ",
        title = paste("AFINN sentiment analysis"),
        subtitle = "Net sentiment value by relative line position across all songs in an album.",
        caption = "Sentiment data provided by Finn Arup Nielsen"
      ) +
      geom_hline(yintercept = 0, color = "black") +
      theme(legend.position = "bottom") +
      scale_fill_viridis(option = "plasma")
  })
  
  output$output_afinn <- renderPlot({plot_afinn()})
  
}

shinyApp(ui, server)