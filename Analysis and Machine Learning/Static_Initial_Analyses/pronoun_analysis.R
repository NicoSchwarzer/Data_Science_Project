
##############################################################
#### Processing for Pronoun Analysis on Initial Dataframe ####
#### (executed on the BW-Cluster) ####


## load packages
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(udpipe)

## read in data
data <- readr::read_csv("base_data_cleaned.csv")

## only keep columns of interest
data <- data %>%
  select(dates, chart_rank, songs, artists, lyrics, genre)


## collapse all song texts for every week (by genre)
week_lyrics_genre <- data %>%
  group_by(dates, genre) %>%
  summarize(text = toString(lyrics))

## since now the important information is retrieved, the big dataset can be dropped to save memory
rm(data); gc()

## descriptives
glimpse(week_lyrics_genre)
unique(week_lyrics_genre$genre)

## work on a subset for genre for now
#week_lyrics_genre_orig <- week_lyrics_genre
#week_lyrics_genre <- week_lyrics_genre[1:200,]


## make combination of genre and date (for some reason necessary)
week_lyrics_genre <- unite(week_lyrics_genre, date_genre, c("dates", "genre"), sep = "_")

## get n-grams for the lyrics
bi_grams_genre <- week_lyrics_genre %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

rm(week_lyrics_genre); gc()

### split data in 4 parts, then read them in again and do the maths
data_1 <- bi_grams_genre[1:floor(0.25*nrow(bi_grams_genre)),]
data_2 <- bi_grams_genre[ceiling(0.25*nrow(bi_grams_genre)):floor(0.5*nrow(bi_grams_genre)),]
data_3 <- bi_grams_genre[ceiling(0.5*nrow(bi_grams_genre)):floor(0.75*nrow(bi_grams_genre)),]
data_4 <- bi_grams_genre[ceiling(0.75*nrow(bi_grams_genre)):nrow(bi_grams_genre),]

write.csv(data_1, "data_1.csv", row.names = FALSE)
write.csv(data_2, "data_2.csv", row.names = FALSE)
write.csv(data_3, "data_3.csv", row.names = FALSE)
write.csv(data_4, "data_4.csv", row.names = FALSE)

rm(list = ls());gc()

## set pronouns
word_pronouns <- c("I", "i", "She", "she", "He", "he", "We", "we")

data_names <- c("data_1", "data_2", "data_3", "data_4")

for (i in 1:length(data_names)){

  data <- readr::read_csv(paste0(data_names[i], ".csv"))

  ## check to keep pronouns only
  data <- data %>%
    tidyr::separate(bigram, c("Word1", "Word2"), sep = " ") %>%
    filter(Word1 %in% word_pronouns | Word2 %in% word_pronouns) %>%
    unite(bigram, c("Word1", "Word2"), sep = " ")

  ## write to csv again
  name <- paste0(data_names[i], "_processed.csv")
  write.csv(data, name, row.names = FALSE)

  ## clean
  rm(data); gc()
}

rm(list=ls()); gc()

## get data names
data_names <- c("data_1", "data_2", "data_3", "data_4")

## get pronouns
word_pronouns <- c("I", "i", "She", "she", "He", "he", "We", "we")

## prepare udpipe model
ud_model <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")


for (i in 1:length(data_names)){
  
  
  data <- readr::read_csv(paste0(data_names[i], "_processed.csv"))
    
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
  head(verbs)
  
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
  
  ## save data
  name <- paste0(data_names[i], "_final.csv")
  write.csv(data, name, row.names = FALSE)
  
  ##
  rm(data); gc()
}




## read data back in
data_1 <- readr::read_csv("Data/data_1_final.csv")
data_2 <- readr::read_csv("Data/data_2_final.csv")
data_3 <- readr::read_csv("Data/data_3_final.csv")
data_4 <- readr::read_csv("Data/data_4_final.csv")

## bind together
data_pronoun_analysis <- rbind(data_1, data_2, data_3, data_4)
data_pronoun_analysis <- separate(data = data_pronoun_analysis,
                                  col = date,
                                  into = c("year", "month", "day"),
                                  sep = "-",
                                  convert = TRUE,
                                  remove = FALSE)

## drop columns not needed
data_pronoun_analysis <- data_pronoun_analysis %>%
  select(-c("id", "bigram"))

## save whole data
#write.csv(data_pronoun_analysis, "Data/data_pronoun_analysis.csv", row.names = FALSE)

## clean
rm(list = c("data_1", "data_2", "data_3", "data_4")); gc()






## make 5 smaller datasets for the analysis
we_data <- data_pronoun_analysis %>%
  filter(pronoun == "we") %>%
  group_by(year, verb) %>%
  count() %>%
  arrange(desc(n)) %>%
  group_by(year)

i_data <- data_pronoun_analysis %>%
  filter(pronoun == "i") %>%
  group_by(year, verb) %>%
  count() %>%
  arrange(desc(n)) %>%
  group_by(year)

she_data <- data_pronoun_analysis %>%
  filter(pronoun == "she") %>%
  group_by(year, verb) %>%
  count() %>%
  arrange(desc(n)) %>%
  group_by(year)

he_data <- data_pronoun_analysis %>%
  filter(pronoun == "he") %>%
  group_by(year, verb) %>%
  count() %>%
  arrange(desc(n)) %>%
  group_by(year)


write.csv(we_data, "C:/Users/leona/Documents/Data_Science_Project/Data/we_data.csv", row.names = FALSE)
write.csv(i_data, "C:/Users/leona/Documents/Data_Science_Project/Data/i_data.csv", row.names = FALSE)
write.csv(she_data, "C:/Users/leona/Documents/Data_Science_Project/Data/she_data.csv", row.names = FALSE)
write.csv(he_data, "C:/Users/leona/Documents/Data_Science_Project/Data/he_data.csv", row.names = FALSE)











