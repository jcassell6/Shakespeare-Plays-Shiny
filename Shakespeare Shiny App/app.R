# library install
library(shiny)
library(shinythemes)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(tidytext)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# Stop words
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE)
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}


# Create the UI Layout
ui <- fluidPage(
  # task6: add in shinythemes function
  theme = shinytheme("journal"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "book", label = "Select a book:",
                  choices = books, selected = "merchant"),
      checkboxInput(inputId = "stopwords", label = "Stop Words:",
                    value = TRUE),
      actionButton(inputId = "run_app",
                   label = "Rerun"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId = "max_words", label = "Max # of words:",
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput(inputId = "largest_word_size", label = "Size of largest words:",
                  min = 1, max = 8, value = 4),
      sliderInput(inputId = "smallest_word_size", label = "Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId = "min_word_count", label = "Minimum words for Count Chart:",
                  min = 10, max = 100, value = 25),
      sliderInput(inputId = "word_size_count", label = "Words size for Count Chart:",
                  min = 8, max = 30, value = 14),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Word Cloud", plotOutput(outputId = "cloud", height = "600px")),
                  tabPanel("Word Counts", plotOutput(outputId = "freq", height = "600px"))
      )
    )
  )
)

server <- function(input, output) {
  freq <- eventReactive (
    input$run_app, withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book, input$stopwords) # ... = replace with the two inputs from Task 2
    })
  )
  output$cloud <-renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$largest_word_size, input$smallest_word_size),
          random.order = FALSE,
          max.words = input$max_words,
          colors=pal))
  })
  output$freq <-renderPlot({
    v <- freq()
    v %>%
      dplyr::filter(n > input$min_word_count) %>%
      ggplot(aes(x = reorder(word, n), y = n))+
      geom_col(width = 0.7) +
      coord_flip()+
      theme(text = element_text(size = input$word_size_count),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  })
}
shinyApp(ui = ui, server = server)

# https://jaime-cassell-uncc.shinyapps.io/Cassell_problem_set_3/

# Collaborated with Nikunja Shrestha   
