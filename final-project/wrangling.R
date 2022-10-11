library(devtools)
library(kableExtra)
library(knitr)
library(tidyverse)      
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)

#devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
              "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
              "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

pop_names <- c("harry", "potter", "hermione", "ron", "dumbledore", "hagrid","snape", "malfoy", "voldemort", "vernon", "weasley", "dudley",
               "lupin", "sirius", "harry's", "umbridge", "mcgonagall", 
               "dobby", "aunt", "professor", "madam", "uncle", "madame", "rita")



##Each book is an array in which each value in the array is a chapter 
# Extracting words

series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(token = "words", output = word, input = text) %>%
    # we tokenize each chapter into words
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}
# set factor to keep books in order 
series$book <- factor(series$book, levels = rev(titles))


# wrangling for sentiment analysis
# remove stop words
sentiment <- series %>%
  anti_join(stop_words) 

lexicon <- get_sentiments("bing")

# join datasets and create the value of the chunk
sentiment_analysis <- sentiment %>%
  inner_join(lexicon, by = "word") %>%
  mutate(id = 1:n(), index = id %/% 100) %>%
  count(book, index = index, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = (positive - negative)/100, book = factor(book, levels = titles)) %>%
  arrange(index)

write_csv(sentiment_analysis, "final-project/data/sentiment_analysis.csv")

# wrangling for shiny app
series <- series %>%
  mutate(book_num = case_when( 
         book == "Philosopher's Stone" ~ 1,
         book == "Chamber of Secrets" ~ 2,
         book == "Prisoner of Azkaban" ~ 3,
         book == "Goblet of Fire" ~ 4,
         book == "Order of the Phoenix" ~ 5,
         book == "Half-Blood Prince" ~ 6,
         book == "Deathly Hallows" ~ 7))


`%notin%` <- Negate(`%in%`)

# generate word counts for each chapter in each book
word_counts_shiny <- series %>%
  unite(section, book_num, chapter, sep = ".")
  
# keep the books in chronological chapter order
word_counts_shiny <- word_counts_shiny %>%
  count(section, word) %>%
  separate(col = 1, into = c("book_num", "chapter"), convert = TRUE) %>%
  arrange(book_num, chapter) %>%
  mutate(id = 1:n()) %>%
  group_by(book_num,chapter) %>%
  mutate(num = max(id)) %>%
  mutate(book = case_when( 
    book_num == 1 ~ "Philosopher's Stone",
    book_num == 2 ~ "Chamber of Secrets",
    book_num == 3 ~ "Prisoner of Azkaban",
    book_num == 4 ~ "Goblet of Fire",
    book_num == 5 ~ "Order of the Phoenix",
    book_num == 6 ~ "Half-Blood Prince",
    book_num == 7 ~ "Deathly Hallows"))


write_csv(word_counts_shiny, "final-project/data/word_counts_shiny.csv")

# for ngrams dataset

# word cloud

# extracting bigrams
ngrams <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(token = "ngrams", output = bigram, n=2, input = text) %>%
    # we tokenize each chapter into bigrams
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  ngrams <- rbind(ngrams, temp)
}

# set factor to keep books in order
ngrams$book <- factor(series$book, levels = rev(titles))

# remove word if any of the words are stop words as well as popular names
ngrams_no_names <- ngrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% pop_names) %>%
  filter(!word2 %in% pop_names) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(book, bigram, sort = TRUE)

write_csv(ngrams_no_names, "final-project/data/no_names_bigrams.csv")




# wrangling for LDA

#NOTE: IF YOU WANT TO COMBINE THE 2 WRANGLING FILES, NOTE THAT THE 2 WORD_COUNTS ARE DIFFERENT.
#Create word_counts data frame, aggregating word count for each chapter for all books
word_counts <- series %>%
  unite(document, book, chapter) %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  filter(word %notin% c("harry", "potter", "hermione", "ron", "dumbledore", "hagrid",
                        "snape", "malfoy", "voldemort", "vernon", "weasley", "dudley",
                        "lupin", "sirius", "harry's", "umbridge", "mcgonagall", 
                        "dobby"))
write_csv(word_counts, "final-project/data/word_counts.csv")


#Create word_counts_book data frame, aggregating word count for each book
word_counts_book <- series %>%
  anti_join(stop_words) %>%
  count(book, word, sort = TRUE) %>%
  filter(word %notin% c("harry", "potter", "hermione", "ron", "dumbledore", "hagrid",
                        "snape", "malfoy", "voldemort", "vernon", "weasley", "dudley",
                        "lupin", "sirius", "harry's", "umbridge", "mcgonagall", 
                        "dobby"))

write_csv(word_counts_book, "final-project/data/word_counts_book.csv")

# Make data frame for Philosopher's Stone
philosopher <- word_counts_book %>%
  filter(str_detect(book, "Philosopher's Stone"))
write_csv(philosopher, "final-project/data/philosopher_word_counts.csv")

# Make data frame for Chamber of Secrets
chamber <- word_counts_book %>%
  filter(str_detect(book, "Chamber of Secrets"))
write_csv(chamber, "final-project/data/chamber_word_counts.csv")

# Make data frame for Prisoner of Azkaban
prisoner <- word_counts_book %>%
  filter(str_detect(book, "Prisoner of Azkaban"))
write_csv(prisoner, "final-project/data/prisoner_word_counts.csv")

# Make data frame for Goblet of Fire
goblet <- word_counts_book %>%
  filter(str_detect(book, "Goblet of Fire"))
write_csv(goblet, "final-project/data/goblet_word_counts.csv")

# Make data frame for Order of the Phoenix
order <- word_counts_book %>%
  filter(str_detect(book, "Order of the Phoenix"))
write_csv(order, "final-project/data/order_word_counts.csv")

# Make data frame for Half-Blood Prince
half <- word_counts_book %>%
  filter(str_detect(book, "Half-Blood Prince"))
write_csv(half, "final-project/data/half_word_counts.csv")

# Make data frame for Deathly Hallows
deathly <- word_counts_book %>%
  filter(str_detect(book, "Deathly Hallows"))
write_csv(deathly, "final-project/data/deathly_word_counts.csv")





