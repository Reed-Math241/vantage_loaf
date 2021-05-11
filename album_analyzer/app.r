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
library(wordcloud)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Album Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "tab_welcome", icon = icon("music")),
      menuItem("Inputs", tabName = "tab_inputs", icon = icon("i-cursor")),
      menuItem("Wordclouds", tabName = "tab_cloud", icon = icon("cloud")),
      menuItem("NRC sentiment analysis", tabName = "tab_nrc", icon = icon("smile")),
      menuItem("AFINN word analysis", tabName = "tab_afinn", icon = icon("stopwatch")),
      menuItem("Sentence sentiments", tabName = "tab_lyric", icon = icon("align-left"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_welcome",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = h3("Welcome!"),
              h4("This dashboard will grab information from",
                 tags$a(href="https://genius.com/", "Genius"),
                 "based on provided inputs, and conduct sentiment analysis."),
              "You can grab information for an album or albums from a single artist, or multiple albums from different artists.",
              HTML("You can also filter out \"dropwords\" (words that might overwhelm word-by-word analysis that otherwise aren't stopwords).<br>"),
              tags$hr(),
              HTML("<b>Note:</b> This dashboard utilizes functions of the"),
              tags$a(href="https://cran.r-project.org/package=genius", tags$code("genius")),
              "package, which at times will not be successful in grabbing all an album's song lyrics due to inconsistencies in Genius' URLs
              or function bugs. Updates can be found on the package's",
              tags$a(href="https://github.com/JosiahParry/genius/blob/master/NEWS.md", "news"),
              "documentation."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "danger",
              title = "May 11 update",
              "Unfortunately as of the morning of 11 May 2021, the",
              tags$a(href="https://cran.r-project.org/package=genius", tags$code("genius")),
              "function that grabs lyric data based on any provided input is only partially functional:
              It will pull a track listing successfully, but reports empty lyric values for all songs.",
              tags$br(),
              HTML(paste0("This issue is also occurring on other warppers, including the API-based ",
                          tags$a(href="https://cran.r-project.org/package=geniusr", tags$code("geniusr")),
                          ".")),
              tags$hr(),
              "Fortunately, I had some saved lyric data of select albums, which can be loaded in the \"Inputs\" tab.",
              tags$br(),
              "All text, inputs, and outputs from when the dashboard was fully functional remain,
              but red boxes such as this have been added in order to properly load this data."
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "How to use this dashboard",
              "Go to the \"Inputs\" tab first to provide the artist(s) and album(s) of interest.
              Once the data has loaded, visit any of the other tabs
              to conduct various sentiment analysis tests on the lyrics,
              following any additional instructions on the respective pages."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Author",
              "This dashboard was created by",
              tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
              "(Reed College '22) in May 2021 as a final project for",
              tags$a(href="https://www.reed.edu/registrar/courses/index.html#math241", "Math 241"),
              "- Data Science.",
              tags$hr(),
              "Behind-the-scenes:",
              tags$a(href="https://github.com/Reed-Math241/vantage_loaf", "GitHub repository")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_inputs",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Required inputs",
              tags$div(
                tags$b("Some notes on inputs:"),
                tags$ul(
                  tags$li("Separate values using commas."),
                  tags$li("Capitalization does not matter, but spelling does."),
                  tags$li("Exclude special characters, punctuation, and logograms."),
                  tags$ul(
                    tags$li("Caveat: Some strings that contain an ampersand (&) might have their
                            Genius information stored with an \"and\", while others skip it entirely:",
                            tags$ul(
                              tags$li("e.g.",
                                      tags$a(href="https://en.wikipedia.org/wiki/Power,_Corruption_%26_Lies",
                                             tags$i("Power, Corruption & Lies")),
                                      "should be inputted as",
                                      tags$a(href="https://genius.com/albums/New-order/Power-corruption-and-lies",
                                             tags$i("Power Corruption and Lies")),
                                      "(also note the exclusion of the comma!)"
                              ),
                              tags$li("e.g.",
                                      tags$a(href="https://en.wikipedia.org/wiki/Speak_%26_Spell_(album)",
                                             tags$i("Speak & Spell")),
                                      "should be inputted as",
                                      tags$a(href="https://genius.com/albums/Depeche-mode/Speak-spell",
                                             tags$i("Speak Spell"))
                              )
                            )
                    ),
                    tags$li("Apostrophes will be automatically accounted for:",
                            tags$ul(
                              tags$li("Artist example:",
                                      tags$a(href="https://en.wikipedia.org/wiki/The_B-52%27s",
                                             "The B-52's"),
                                      "will become",
                                      tags$a(href="https://genius.com/artists/The-b-52s",
                                             "The B-52s")
                              ),
                              tags$li("Album example:",
                                      tags$a(href="https://en.wikipedia.org/wiki/The_Sky%27s_Gone_Out",
                                             tags$i("The Sky's Gone Out")),
                                      "will become",
                                      tags$a(href="https://genius.com/albums/Bauhaus/The-sky-s-gone-out",
                                             tags$i("The Sky s Gone Out"))
                              )
                            )
                    )
                  ),
                  tags$li("Valid examples have been included as default inputs.")
                )
              ),
              "Still unsure how artist or album should be named if it has special marks?
              Follow the pattern that the URL of the artist/album that the",
              tags$a(href="https://genius.com/", "Genius website"),
              "uses, replacing each \"-\" with a space.",
              tags$hr(),
              h4("Artists"),
              textInput("input_artists", label = "Input artist name(s):", value = "New Order, Depeche Mode, Depeche Mode"),
              "If analyzing one artist's works, their name is only required once.
              If comparing albums from multiple artists, write the names respective to their album as listed below.",
              tags$hr(),
              h4("Albums"),
              textInput("input_albums", label = "Input album title(s):", value = "Power Corruption and Lies, Violator, Speak Spell"),
              tags$hr(),
              HTML("Press the button below once artists and albums have been entered!
                   The data gathering process will take a few moments.
                   When complete, the tables will fill, and text analysis can be conducted in the other tabs.<br><br>"),
              actionButton(
                #inputId = "action_grab",
                # For fix: temporarily changing ID of action button
                inputId = "action_grab_to_fix",
                label = "Get lyrics!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Note: Table of songs with missing lyric data",
              "Since the",
              tags$a(href="https://cran.r-project.org/package=genius", tags$code("genius")),
              "function might fail to pull each song's lyric data (see the note on the welcome tab for more info),
              a data frame will be provided below of songs that are missing lyric data.
              This also includes tracks with have no lyrics, such as an instrumental song.
              If there are songs included in the list that you would like to analyze, consider re-running the \"Grab lyrics\" function.",
              tableOutput("output_missing")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "danger",
              title = "May 11 update",
              HTML("The data set will have six albums from three different artists,
                   which is considerably large when comparing sentiments.
                   However, all are included in order to maximize visual variability.<br>
                   Press the button below <b>twice</b> to load the pre-saved lyric data:<br><br>"),
              actionButton(
                inputId = "action_grab",
                label = "Load lyrics! (press twice)",
                icon = icon("hand-point-right"),
                class = "btn-danger"
              ),
              tags$hr(),
              "The original \"Get lyrics\" button will not work for the time being,
              but the box and inputs will remain for reference and to see how the dashboard would normally work.
              Please see the update on the \"Welcome\" tab for more information."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Table of provided inputs",
              "If this table is printed after submission but an error is reported in the lyric data frame,
              check to make sure the artist and albums are appropriately lined up
              and any special characters are properly addressed.",
              tableOutput("output_artist_album")
            ),
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "Data frame of lyrics",
              dataTableOutput("output_lyrics")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_cloud",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Filters and options",
              checkboxGroupInput("input_cloud_artist", label = "Artist(s) to include:"),
              checkboxGroupInput("input_cloud_album", label = "Album(s) to include:"),
              #sliderInput("input_freq", label = "Minimum word frequency:", min = 1, max = 50, value = 3),
              #For fix: manually setting max value to max token value of the set; usually automatic but bugging w loaded df
              sliderInput("input_freq", label = "Minimum word frequency:", min = 1, max = 61, value = 3),
              textInput("input_cloud_dropwords", label = "Optional - Dropword(s) to exclude, separated with commas:"),
              HTML("<i><b>CW:</b> Profanities are not automatically filtered,
                   and will be displayed if they have high enough frequency.</i><br><br>"),
              actionButton(
                inputId = "action_cloud_filter",
                label = "Generate wordcloud!",
                icon = icon("hand-point-right"),
                class = "btn-warning")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Top tokens",
              "These are the top five non-stopwords for each album based on the provided filters. Should more be included in the list of dropwords?",
              tableOutput("output_top_tokens")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Word cloud result",
              plotOutput("output_wordcloud", height = "600px")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "More about word clouds",
              HTML(paste0(
                "This analysis will generate a word cloud of non-",
                tags$a(href="https://en.wikipedia.org/wiki/Stop_word", "stopword"),
                " tokens based on the selected albums, utilizing the ",
                tags$a(href="https://CRAN.R-project.org/package=wordcloud", tags$code("wordcloud")),
                " package."
              ))
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_nrc",
        fluidRow(
          column(
            width = 6,
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
                label = "Conduct NRC analysis!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "Highest frequency words of each sentiment in each album",
              dataTableOutput("output_nrc_top")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "NRC analysis result",
              plotOutput("output_nrc", height = "800px")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_afinn",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "AFINN analysis",
              "The",
              tags$a(href="https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x", "AFINN sentiment lexicon"),
              HTML("analyzes words using a scale of -5 (most negative sentiment) to +5 (most positive sentiment).<br>
               The button below will plot the AFINN results averaged by album based on words' relative position in each song.
               This way, one can compare if various albums are generally negative or positive,
                   and at what timestamp songs tend to be most positive or negative.<br><br>"),
              actionButton(
                inputId = "action_afinn",
                label = "Conduct AFINN analysis!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "AFINN analysis result",
              plotOutput("output_afinn", height = "800px")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_lyric",
        fluidRow(
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Sentence analysis",
              "These analyses will conduct analysis by sentence (rather than by individual words) using the",
              tags$a(href="https://github.com/trinker/sentimentr", tags$code("sentimentr")),
              "package."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "How profane!",
              HTML("Aggregate profanity by song. This will <b>not</b> display the profane words, but rather numeric counts by song.<br><br>"),
              actionButton(
                inputId = "action_profanity",
                label = "Calculate profanity!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, status = "warning",
              title = "Table of profanity by song",
              dataTableOutput("output_profanity_table")
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Plot of profanity by song",
              plotOutput("output_profanity_plot")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "warning",
              title = "Highlighting lines",
              HTML("Lines of positive sentiment will be highlighted green, while negative will be highlighted pink.
                   Average sentiment value by song are also included as numeric values.
                   Results will open in a new window.
                   <br><i><b>Note:</b> This function currently works on local Shiny servers only.</i><br><br>"),
              actionButton(
                inputId = "action_line",
                label = "Conduct sentence analysis!",
                icon = icon("hand-point-right"),
                class = "btn-warning"
              ),
              htmlOutput("output_lines")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  #Temporary fix
  df_lyrics <- eventReactive(input$action_grab,{
    withProgress(message = "Loading data!",
                 read_csv("example_lyrics.csv") %>% 
                   mutate(
                     album = fct_inorder(as.factor(album)),
                     artist = fct_inorder(as.factor(artist)),
                     track_title = fct_reorder(as.factor(track_title), track_n)
                   )
                 )
  })
  
  observeEvent(input$action_grab, {
    artist_list <- df_lyrics() %>% 
      count(artist, album) %>% 
      mutate(artist = as.character(artist)) %>% 
      pull(artist) %>% 
      paste(collapse = ", ")
    updateTextInput(session, inputId = "input_artists", value = artist_list)
    
    album_list <- df_lyrics() %>% 
      mutate(album = as.character(album)) %$%
      unique(album) %>% 
      paste(collapse = ", ")
    updateTextInput(session, inputId = "input_albums", value = album_list)
  })
  #End of temporary fix
  
  #Start of tab_inputs section
  df_artist_album <- eventReactive(input$action_grab, {
    data.frame(artist = c(unlist(str_split(input$input_artists, ", "))),
               album = c(unlist(str_split(input$input_albums, ", ")))
    ) %>%
      mutate(artist = gsub(x = artist,
                           pattern = "'",
                           replacement = ""),
             album = gsub(x = album,
                          pattern = "'",
                          replacement = " ")
      )
  })
  
  output$output_artist_album <- renderTable({
    df_artist_album() %>% 
      clean_names(case = "title")
  })
  
  # df_lyrics <- eventReactive(input$action_grab, {
  #   withProgress(message = "Gathering data from Genius!",
  #                detail = "This will be the longest step.",
  #                df_artist_album() %>%
  #                  add_genius(artist, album, type = "album") %>%
  #                  mutate(
  #                    album = fct_inorder(as.factor(album)),
  #                    artist = fct_inorder(as.factor(artist)),
  #                    track_title = fct_reorder(as.factor(track_title), track_n)
  #                  )
  #   )
  # })
  
  df_tokenized <- reactive({
    withProgress(message = "Tokenizing data!",
                 detail = "Separating lyrics into individual words.",
                 df_lyrics() %>% 
                   unnest_tokens(output = word, input = lyric, token = "words")
    )
  })
  
  output$output_lyrics <- renderDataTable({
    datatable(style = "bootstrap",
              filter = list(position = "top", plain = TRUE),
              df_lyrics() %>% 
                select(artist, album, track_n, track_title, line, lyric) %>% 
                rename("track no" = track_n) %>% 
                clean_names(case = "title")
    )
  })
  
  output$output_missing <- renderTable({
    df_lyrics() %>%
      filter_at(vars(line, lyric), is.na) %>%
      select(artist, album, track_n, track_title) %>%
      rename("track no" = track_n) %>%
      clean_names(case = "title")
  })
  # End of tab_inputs section
  
  # Start of tab_cloud section
  observeEvent(input$action_grab, {
    artist_values <- unique(c(unlist(str_split(input$input_artists, ", "))))
    updateCheckboxGroupInput(session, inputId = "input_cloud_artist", choices = artist_values, selected = artist_values[1])
    album_values <- c(unlist(str_split(input$input_albums, ", ")))
    updateCheckboxGroupInput(session, inputId = "input_cloud_album", choices = album_values, selected = album_values[1])
  })
  
  list_cloud_dropwords <- eventReactive(input$action_cloud_filter, {
    c(unlist(str_split(input$input_cloud_dropwords, ", ")))
  })
  
  df_wordcloud <- eventReactive(input$action_cloud_filter,{
    df_tokenized() %>% 
      filter(
        !word %in% list_cloud_dropwords(),
        artist %in% input$input_cloud_artist,
        album %in% input$input_cloud_album
      ) %>% 
      anti_join(stop_words, by = "word") %>%
      drop_na(word) %>% 
      count(artist, album, word, sort = TRUE)
  })
  
  #Temporarily removed during fix.. reporting high values
  # observeEvent(input$action_grab, {
  #   df_top <- df_tokenized() %>% 
  #     anti_join(stop_words, by = "word") %>% 
  #     drop_na(word) %>% 
  #     count(artist, album, word, sort = TRUE)
  #   
  #   updateSliderInput(session, inputId = "input_freq", max = max(df_top$n))
  # })
  #end of temporary removal
  
  df_top_tokens <- eventReactive(input$action_cloud_filter, {
    df_wordcloud() %>% 
      group_by(artist, album) %>% 
      slice_max(order_by = n, n = 5)
  })
  
  output$output_top_tokens <- renderTable({
    df_top_tokens() %>% 
      arrange(desc(n)) %>% 
      rename("frequency" = "n") %>% 
      clean_names(case = "title")
  })
  
  plot_wordcloud <- eventReactive(input$action_cloud_filter, {
    df_wordcloud() %>% 
      with(wordcloud(word,
                     n,
                     min.freq = input$input_freq,
                     random.order = FALSE,
                     colors = inferno(20, direction = -1, end = .9)))
  })
  
  output$output_wordcloud <- renderPlot({plot_wordcloud()})
  # End of tab_cloud section
  
  # Start of tab_nrc section
  nrc_order <- c(
    "positive", "negative",
    "anger", "fear", "anticipation",
    "trust", "surprise",
    "sadness", "joy", "disgust"
  )
  
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
              class = 'table-bordered table-condensed',
              filter = list(position = "top", plain = TRUE),
              options = list(pageLength = 25),
              df_nrc_top() %>% 
                mutate(
                  artist = as.factor(artist),
                  album = as.factor(album),
                  cat = as.factor(cat)
                ) %>% 
                select(artist, album, cat, sentiment, n, word) %>% 
                clean_names(case = "title") %>% 
                rename(
                  "Category" = "Cat",
                  "Frequency" = "N"
                )
    )
  })
  # End of tab_nrc section
  
  # Start of tab_afinn section
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
  # End of tab_afinn section
  
  #Start of tab_lyric section
  line_by_line <- eventReactive(input$action_line, {
    df_lyrics() %>% 
      mutate(lyric_line = get_sentences(lyric)) %$%
      sentiment_by(lyric_line, list(artist, album, track_title)) %>% 
      highlight()
  })
  
  output$output_lines <- renderText({line_by_line()})
  
  df_profanity <- eventReactive(input$action_profanity, {
    df_lyrics() %>% 
      drop_na(lyric) %>% 
      get_sentences() %$% 
      profanity_by(lyric, list(track_title, track_n, album, artist))
  })
  
  output$output_profanity_table <- renderDataTable({
    datatable(style = "bootstrap",
              class = 'table-bordered table-condensed',
              filter = list(position = "top", plain = TRUE),
              df_profanity() %>% 
                arrange(desc(profanity_count)) %>% 
                mutate_at(c("sd", "ave_profanity"), round, digits = 3) %>% 
                select(artist, album, track_n, track_title,
                       word_count, profanity_count, sd, ave_profanity) %>% 
                rename(
                  "track no" = "track_n",
                  "standard deviation" = "sd",
                  "profanity rate" = "ave_profanity"
                ) %>% 
                clean_names(case = "title")
    )
  })
  
  output$output_profanity_plot <- renderPlot({
    df_profanity() %>% 
      ggplot(aes(x = track_n, y = profanity_count, color = artist)) +
      facet_wrap(~album, scales = "free_x") +
      geom_point() +
      geom_path() +
      theme_hc(style = "darkunica") +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        axis.text = element_text(color = "gray", size = 12),
        axis.text.x = element_blank()
      ) +
      ylim(0, NA) +
      labs(
        title = "Profanity count by song in each album",
        x = "Track",
        y = "Profanity count",
        color = "Artist:"
      )
  })
  # End of tab_lyrics section
  
}

shinyApp(ui, server)