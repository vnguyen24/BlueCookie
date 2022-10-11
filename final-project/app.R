# Load necessary packages
library(shiny)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(wordcloud)
library(tidytext)

word_counts_shiny <- read_csv("data/word_counts_shiny.csv")


mypal <- brewer.pal(8, "Dark2")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 1 widgets:
# titles of the books
titles <- list("Philosopher's Stone" = "philosophers_stone", "Chamber of Secrets" = "chamber_of_secrets", "Prisoner of Azkaban" = "prisoner_of_azkaban",
            "Goblet of Fire" = "goblet_of_fire", "Order of the Phoenix" = "order_of_the_phoenix", "Half-Blood Prince" = "half_blood_prince",
            "Deathly Hallows" = "deathly_hallows")


############
#    ui    #
############
ui <- fixedPage(
  
  titlePanel("Harry Potter Word Frequency"),
    fixedRow(
        column(4,
          wellPanel(

          # text input to choose a word
           textInput(inputId = "choose_word", 
                  label = "Choose a word: ",
                  value = "wand"
             )
          )
       ),
       fixedRow(
       column(12,
      mainPanel(plotOutput(outputId = "word_counts")),
    )
  )
)
)
  
############
# server   #
############
server <- function(input, output) {

  
  # TAB 1: INTERACTIVE Word Frequency Plot 
  output$word_counts <- renderPlot({
   word_counts_shiny %>%
      filter(word %in% input$choose_word) %>%
      ggplot(aes(x = num, y = n)) +
      geom_col(aes(fill = book)) +
      geom_smooth(se = FALSE) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_blank()) +
      labs(title = "Frequency of a Given Word in the Harry Potter Series",
           fill = "Book", 
           x = "Chronological Chapters", y = "Number of Occurences")
 
  })
  
 
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)