#### Pronoun Analysis File for Updating ####

library(tidytext)
library(udpipe)
library(dplyr)
library(tidyr)

## read in base data, pronoun data and verb classifications
data <- base_data_cleaned
data_pronoun_analysis <- data.table::fread("data_pronoun_analysis.csv")
data_pronoun_analysis$date <- as.Date(data_pronoun_analysis$date)
own_verbs <- data.table::fread("own_verbs.csv")
levin_verbs <- data.table::fread("levin_verbs.csv")

## get new dates for the analysis
dates_to_analyse <- unique(data$dates)[!unique(data$dates) %in% unique(data_pronoun_analysis$date)]

if (length(dates_to_analyse) != 0){
  
  
  ## get sub-data to perform pronoun analysis
  data <- data %>%
    filter(dates %in% dates_to_analyse) %>%
    select(dates, chart_rank, songs, artists, lyrics, genre)
  
  ## collapse all song texts for every week (by genre)
  data <- data %>%
    group_by(dates, genre) %>%
    summarize(text = toString(lyrics))
  
  ## make combination of genre and date (for some reason necessary)
  data <- unite(data, date_genre, c("dates", "genre"), sep = "_")
  
  ## get bigrams for the lyrics
  data <- data %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  ## set pronouns
  word_pronouns <- c("I", "i", "She", "she", "He", "he", "We", "we")
  
  ## check to keep pronouns only
  data <- data %>%
    tidyr::separate(bigram, c("Word1", "Word2"), sep = " ") %>%
    filter(Word1 %in% word_pronouns | Word2 %in% word_pronouns) %>%
    unite(bigram, c("Word1", "Word2"), sep = " ")
  
  ## prepare udpipe model
  ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")
  
  ## give IDs
  data$id <- 1:nrow(data)
  
  ## extract word information
  data_inf <- udpipe_annotate(ud_model, x = data$bigram, doc_id = (1:nrow(data)))
  data_inf <- data.frame(data_inf)
  
  ## keep only important columns
  data_inf <- data_inf %>%
    select(doc_id, sentence, token, lemma, upos, xpos)
  
  ## extract the verbs and pronouns
  verbs <- filter(data_inf, upos %in% c("VERB", "AUX"))
  verbs$doc_id <- as.integer(verbs$doc_id)
  pronouns <- filter(data_inf, upos == "PRON")
  pronouns$doc_id <- as.integer(pronouns$doc_id)
  
  ## keep only the rows in the bigram data that contain a verb
  data <- data[verbs$doc_id,]
  
  ## join pronouns and verbs to data
  data <- left_join(data, pronouns[,c("doc_id", "lemma")], by = c("id" = "doc_id")) %>%
    rename(pronoun = lemma)
  data <- left_join(data, verbs[,c("doc_id", "lemma")], by = c("id" = "doc_id")) %>%
    rename(verb = lemma)
  
  ## do a little cleaning inbetween
  data$pronoun <- tolower(data$pronoun)
  data <- data %>%
    filter(pronoun %in% word_pronouns)
  # get the date and genre column properly again
  data <- tidyr::separate(data = data,
                          col = date_genre, 
                          into = c("date", "genre"),
                          sep = "_")
  data$date <- as.Date(data$date)
  
  ## transform a little and then bind to original dataset again
  data <- data %>%
    select(-c("bigram", "id")) %>%
    separate(date, into = c("year", "month", "day"), sep = "-", remove = FALSE, convert = TRUE)
  
  ## rbind the new data to the old one
  data_pronoun_analysis <- rbind(data_pronoun_analysis, data)
  
  
  ## make 5 smaller datasets for the analysis
  we_data <- data_pronoun_analysis %>%
    filter(pronoun == "we") %>%
    group_by(year, verb, genre) %>%
    count() %>%
    arrange(desc(n)) %>%
    group_by(year)
  
  i_data <- data_pronoun_analysis %>%
    filter(pronoun == "i") %>%
    group_by(year, verb, genre) %>%
    count() %>%
    arrange(desc(n)) %>%
    group_by(year)
  
  she_data <- data_pronoun_analysis %>%
    filter(pronoun == "she") %>%
    group_by(year, verb, genre) %>%
    count() %>%
    arrange(desc(n)) %>%
    group_by(year)
  
  he_data <- data_pronoun_analysis %>%
    filter(pronoun == "he") %>%
    group_by(year, verb, genre) %>%
    count() %>%
    arrange(desc(n)) %>%
    group_by(year)
  
  ## merge the categories to he and she data
  # if new categorization comes: must be in format R("verb", "name_category")
  # then just simple left_join
  he_data <- left_join(he_data, own_verbs, by = c("verb"))
  he_data <- left_join(he_data, levin_verbs[,c("verb", "levin_category")], by = c("verb"))
  she_data <- left_join(she_data, own_verbs, by = c("verb"))
  she_data <- left_join(she_data, levin_verbs[,c("verb", "levin_category")], by = c("verb"))
  
  ## store the data as CSVs locally
  write.csv(data_pronoun_analysis, "data_pronoun_analysis.csv", row.names = FALSE)
  write.csv(we_data, "we_data.csv", row.names = FALSE)
  write.csv(i_data, "i_data.csv", row.names = FALSE)
  write.csv(she_data, "she_data.csv", row.names = FALSE)
  write.csv(he_data, "he_data.csv", row.names = FALSE)
  
  ## sent to Shiny App
  write.csv(data_pronoun_analysis, "/srv/shiny-server/DS_Project/data_pronoun_analysis.csv", row.names = FALSE)
  write.csv(we_data, "/srv/shiny-server/DS_Project/we_data.csv", row.names = FALSE)
  write.csv(i_data, "/srv/shiny-server/DS_Project/i_data.csv", row.names = FALSE)
  write.csv(she_data, "/srv/shiny-server/DS_Project/she_data.csv", row.names = FALSE)
  write.csv(he_data, "/srv/shiny-server/DS_Project/he_data.csv", row.names = FALSE)
  
  
  ## remove stuff
  try(rm(list = c("data_inf","pronouns", "ud_model", "verbs",
                  "we_data", "he_data", "she_data", "i_data","word_pronouns"))
  )
  
  gc()

}

try(rm(list = c("data_pronoun_analysis", "data", "dates_to_analyse", "own_verbs", "levin_verbs")))
gc()


