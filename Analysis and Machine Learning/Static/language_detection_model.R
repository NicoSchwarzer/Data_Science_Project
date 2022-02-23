#### Develop model for language recognition ####

## set working directory
setwd("C:/Users/leona/Documents/Data_Science_Project")

source("Code/functions_language_detection_model.R")


## load packages
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gutenbergr")) install.packages("gutenbergr")
if (!require("tokenizers")) install.packages("tokenizers")
if (!require("stringr")) install.packages("stringr")
if (!require("xgboost")) install.packages("xgboost")


## get books metadata
gutenberg_metadata <- gutenberg_metadata
glimpse(gutenberg_metadata)


## get spanish, english, french and german books
esbooks <- gutenberg_metadata %>%
  filter(language == "es") %>%
  filter(has_text = TRUE)

enbooks <- gutenberg_metadata %>%
  filter(language == "en") %>%
  filter(has_text = TRUE)

frbooks <- gutenberg_metadata %>%
  filter(language == "fr") %>%
  filter(has_text = TRUE)

debooks <- gutenberg_metadata %>%
  filter(language == "de") %>%
  filter(has_text = TRUE)

itbooks <- gutenberg_metadata %>%
  filter(language == "it") %>%
  filter(has_text = TRUE)


#### Get training data for french ####

## get the books which the model is trained on later
le_tour_de_monde_fr <- gutenberg_download(gutenberg_id = 800)
monte_christo1_fr <- gutenberg_download(gutenberg_id = 17989)

## get the whole text of book
jules_verne_txt <- paste0(le_tour_de_monde_fr$text, collapse = " ")
monte_christo1_fr_txt <- paste0(monte_christo1_fr$text, collapse = " ")

## tokenize words and word stems and unlist
monte_christo1_stems <- tok_stems(monte_christo1_fr_txt, stems = FALSE)
jules_verne_stems <- tok_stems(jules_verne_txt, stems = FALSE)

## combine all books and make vocabulary
french_vocab <- count_filter(list(monte_christo1_stems, jules_verne_stems),
                             n_filter = 7)

rm(list = c("le_tour_de_monde_fr", "monte_christo1_fr", "monte_christo1_stems", "jules_verne_stems"))


#### Get training data for spanish ####

## get the books which the model is trained on later
don_quijote_es <- gutenberg_download(gutenberg_id = 2000)
guardia_blanca_es <- gutenberg_download(gutenberg_id = 36453)

## get the whole text of book
don_quijote_txt <- paste0(don_quijote_es$text, collapse = " ")
guardia_blanca_txt <- paste0(guardia_blanca_es$text, collapse = " ")

## tokenize words and word stems and unlist
don_quijote_stems <- tok_stems(don_quijote_txt, stems = FALSE)
guardia_blanca_stems <- tok_stems(guardia_blanca_txt, stems = FALSE)

## combine all books and make vocabulary
spanish_vocab <- count_filter(list(don_quijote_stems, guardia_blanca_stems),
                              n_filter = 7)

rm(list = c("don_quijote_es", "guardia_blanca_es", "don_quijote_stems", "guardia_blanca_stems"))



#### Get training data for english ####

## get the books which the model is trained on later
pride_prejudice_en <- gutenberg_download(gutenberg_id = 1342)
alice_wonderland_en <- gutenberg_download(gutenberg_id = 11)
peter_pan_en <- gutenberg_download(gutenberg_id = 16)
moby_dick_en <- gutenberg_download(gutenberg_id = 15)
cia_factbook_en <- gutenberg_download(gutenberg_id = 14)
art_internet_en <- gutenberg_download(gutenberg_id = 34)
census_en <- gutenberg_download(gutenberg_id = 29)
internet_guide1 <- gutenberg_download(gutenberg_id = 39)
internet_guide2 <- gutenberg_download(gutenberg_id = 49)

## get the whole text of book
pride_prejudice_txt <- paste0(pride_prejudice_en$text, collapse = " ")
alice_wonderland_txt <- paste0(alice_wonderland_en$text, collapse = " ")
peter_pan_txt <- paste0(peter_pan_en$text, collapse = " ")
moby_dick_txt <- paste0(moby_dick_en$text, collapse = " ")
cia_factbook_txt <- paste0(cia_factbook_en$text, collapse = " ")
art_internet_txt <- paste0(art_internet_en$text, collapse = " ")
census_txt <- paste0(census_en$text, collapse = " ")
internet_guide1_txt <- paste0(internet_guide1$text, collapse = " ")
internet_guide2_txt <- paste0(internet_guide2$text, collapse = " ")

## tokenize words and unlist
pride_prejudice_stems <- tok_stems(pride_prejudice_txt, stems = FALSE)
alice_wonderland_stems <- tok_stems(alice_wonderland_txt, stems = FALSE)
peter_pan_stems <- tok_stems(peter_pan_txt, stems = FALSE)
moby_dick_stems <- tok_stems(moby_dick_txt, stems = FALSE)
cia_factbook_stems <- tok_stems(cia_factbook_txt, stems = FALSE)
art_internet_stems <- tok_stems(art_internet_txt, stems = FALSE)
census_stems <- tok_stems(census_txt, stems = FALSE)
internet_guide1_stems <- tok_stems(internet_guide1_txt, stems = FALSE)
internet_guide2_stems <- tok_stems(internet_guide2_txt, stems = FALSE)



## combine all books and make vocabulary
english_vocab <- count_filter(list(pride_prejudice_stems, alice_wonderland_stems, peter_pan_stems, moby_dick_stems,
                                   cia_factbook_stems, art_internet_stems, census_stems, internet_guide1_stems,
                                   internet_guide2_stems),
                              n_filter = 15)

num <- as.numeric(english_vocab$words)
english_vocab <- english_vocab[is.na(num),]


rm(list = c("pride_prejudice_en", "alice_wonderland_en", "peter_pan_en", "cia_factbook_en", "moby_dick_en",
            "cia_factbook_en", "art_internet_en", "census_en", "internet_guide1", "internet_guide2",
            "pride_prejudice_stems", "alice_wonderland_stems", "peter_pan_stems", "moby_dick_stems",
            "cia_factbook_stems", "art_internet_stems", "census_stems", "internet_guide1_stems", "internet_guide2_stems"))



#### Get training data for german ####

## get the books which the model is trained on later
faust_de <- gutenberg_download(gutenberg_id = 2229)
schatzinsel_de <- gutenberg_download(gutenberg_id = 49424)

## get the whole text of book
faust_txt <- paste0(faust_de$text, collapse = " ")
schatzinsel_txt <- paste0(schatzinsel_de$text, collapse = " ")

## tokenize words and word stems and unlist
faust_stems <- tok_stems(faust_txt, stems = FALSE)
faust_stems$words <- stringr::str_replace_all(faust_stems$words, "ä", "ae")
faust_stems$words <- stringr::str_replace_all(faust_stems$words, "ü", "ue")
faust_stems$words <- stringr::str_replace_all(faust_stems$words, "ö", "oe")
schatzinsel_stems <- tok_stems(schatzinsel_txt, stems = FALSE)
schatzinsel_stems$words <- stringr::str_replace_all(schatzinsel_stems$words, "ä", "ae")
schatzinsel_stems$words <- stringr::str_replace_all(schatzinsel_stems$words, "ü", "ue")
schatzinsel_stems$words <- stringr::str_replace_all(schatzinsel_stems$words, "ö", "oe")

## combine all books and make vocabulary
german_vocab <- count_filter(list(faust_stems, schatzinsel_stems),
                              n_filter = 7)

rm(list = c("faust_de", "schatzinsel_de", "faust_stems", "schatzinsel_stems"))


#### Get training data for italien ####

## get the books which the moitl is trained on later
inferno_it <- gutenberg_download(gutenberg_id = 1009)
racconti_it <- gutenberg_download(gutenberg_id = 49510)
carta_bollata <- gutenberg_download(gutenberg_id = 17896)

## get the whole text of book
inferno_txt <- paste0(inferno_it$text, collapse = " ")
racconti_txt <- paste0(racconti_it$text, collapse = " ")
carta_bollata_txt <- paste0(carta_bollata$text, collapse = " ")

## tokenize words and word stems and unlist
inferno_stems <- tok_stems(inferno_txt, stems = FALSE)
racconti_stems <- tok_stems(racconti_txt, stems = FALSE)
carta_bollata_stems <- tok_stems(carta_bollata_txt, stems = FALSE)

## combine all books and make vocabulary
italian_vocab <- count_filter(list(inferno_stems, racconti_stems, carta_bollata_stems),
                             n_filter = 10)

rm(list = c("inferno_it", "racconti_it", "carta_bollata", "carta_bollata_stems", "inferno_stems", "racconti_stems"))









#### Process the book data and create dataset for training ####

## after we have all the important words for 5 languages, set those all together and
## use parts of the books as training stuff

# get the whole vocabulary
word_variables <- c(english_vocab$words, french_vocab$words, german_vocab$words,
                    italian_vocab$words, spanish_vocab$words)
word_variables <- unique(word_variables)


word_variables_reduced <- c(english_vocab$words[1:200], french_vocab$words[1:200], german_vocab$words[1:200],
                            italian_vocab$words[1:200], spanish_vocab$words[1:200])
word_variables_reduced <- unique(word_variables_reduced)


# now "data-tize" the books, tokenize with word stems first again and unlist
faust_c <- count_paragraphs(text = faust_txt, word_variables = word_variables, language = "german")
schatzinsel_c <- count_paragraphs(text = schatzinsel_txt, word_variables = word_variables, language = "german")

allice_c <- count_paragraphs(text = alice_wonderland_txt, word_variables = word_variables, language = "english")
pride_prejudice_c <- count_paragraphs(text = pride_prejudice_txt, word_variables = word_variables, language = "english")
peter_pan_c <- count_paragraphs(text = peter_pan_txt, word_variables = word_variables, language = "english")
moby_dick_c <- count_paragraphs(text = moby_dick_txt, word_variables = word_variables, language = "english")
cia_factbook_c <- count_paragraphs(text = cia_factbook_txt, word_variables = word_variables, language = "english")
art_internet_c <- count_paragraphs(text = art_internet_txt, word_variables = word_variables, language = "english")
census_c <- count_paragraphs(text = census_txt, word_variables = word_variables, language = "english")
internet_guide1_c <- count_paragraphs(text = internet_guide1_txt, word_variables = word_variables, language = "english")
internet_guide2_c <- count_paragraphs(text = internet_guide2_txt, word_variables = word_variables, language = "english")

monte_christo_c <- count_paragraphs(text = monte_christo1_fr_txt, word_variables = word_variables, language = "french")
jules_verne_c <- count_paragraphs(text = jules_verne_txt, word_variables = word_variables, language = "french")

inferno_c <- count_paragraphs(text = inferno_txt, word_variables = word_variables, language = "italian")
racconti_c <- count_paragraphs(text = racconti_txt, word_variables = word_variables, language = "italian")
carta_bollata_c <- count_paragraphs(text = carta_bollata_txt, word_variables = word_variables, language = "italian")

don_quijote_c <- count_paragraphs(text = don_quijote_txt, word_variables = word_variables, language = "spanish")
guardia_blanca_c <- count_paragraphs(text = guardia_blanca_txt, word_variables = word_variables, language = "spanish")

## combine all text data to one big dataset
data <- rbind(faust_c, schatzinsel_c, allice_c, pride_prejudice_c, monte_christo_c, peter_pan_c,
              cia_factbook_c, art_internet_c, census_c, internet_guide1_c, internet_guide2_c,
              moby_dick_c, jules_verne_c, inferno_c, racconti_c, don_quijote_c, guardia_blanca_c,
              carta_bollata_c)



#### Do the same with reduced word varibales - vocabulary
# now "data-tize" the books, tokenize with word stems first again and unlist
faust_c_red <- count_paragraphs(text = faust_txt, word_variables = word_variables_reduced, language = "german")
schatzinsel_c_red <- count_paragraphs(text = schatzinsel_txt, word_variables = word_variables_reduced, language = "german")

allice_c_red <- count_paragraphs(text = alice_wonderland_txt, word_variables = word_variables_reduced, language = "english")
pride_prejudice_c_red <- count_paragraphs(text = pride_prejudice_txt, word_variables = word_variables_reduced, language = "english")
peter_pan_c_red <- count_paragraphs(text = peter_pan_txt, word_variables = word_variables_reduced, language = "english")
moby_dick_c_red <- count_paragraphs(text = moby_dick_txt, word_variables = word_variables_reduced, language = "english")
cia_factbook_c_red <- count_paragraphs(text = cia_factbook_txt, word_variables = word_variables_reduced, language = "english")
art_internet_c_red <- count_paragraphs(text = art_internet_txt, word_variables = word_variables_reduced, language = "english")
census_c_red <- count_paragraphs(text = census_txt, word_variables = word_variables_reduced, language = "english")
internet_guide1_c_red <- count_paragraphs(text = internet_guide1_txt, word_variables = word_variables_reduced, language = "english")
internet_guide2_c_red <- count_paragraphs(text = internet_guide2_txt, word_variables = word_variables_reduced, language = "english")

monte_christo_c_red <- count_paragraphs(text = monte_christo1_fr_txt, word_variables = word_variables_reduced, language = "french")
jules_verne_c_red <- count_paragraphs(text = jules_verne_txt, word_variables = word_variables_reduced, language = "french")

inferno_c_red <- count_paragraphs(text = inferno_txt, word_variables = word_variables_reduced, language = "italian")
racconti_c_red <- count_paragraphs(text = racconti_txt, word_variables = word_variables_reduced, language = "italian")
carta_bollata_c_red <- count_paragraphs(text = carta_bollata_txt, word_variables = word_variables_reduced, language = "italian")

don_quijote_c_red <- count_paragraphs(text = don_quijote_txt, word_variables = word_variables_reduced, language = "spanish")
guardia_blanca_c_red <- count_paragraphs(text = guardia_blanca_txt, word_variables = word_variables_reduced, language = "spanish")



## combine all text data to one big dataset
data_red <- rbind(faust_c_red, schatzinsel_c_red, allice_c_red, pride_prejudice_c_red, monte_christo_c_red, peter_pan_c_red,
              cia_factbook_c_red, art_internet_c_red, census_c_red, internet_guide1_c_red, internet_guide2_c_red,
              moby_dick_c_red, jules_verne_c_red, inferno_c_red, racconti_c_red, don_quijote_c_red, guardia_blanca_c_red,
              carta_bollata_c_red)


## save dataset
write.csv(data, file = "Data/text_model_data.csv",
          row.names = FALSE)

write.csv(data_red, file = "Data/text_model_data_reduced.csv",
          row.names = FALSE)

## save vocabulary
write.csv(word_variables, "Data/vocabulary.csv",
          row.names = FALSE)

write.csv(word_variables_reduced, "Data/vocabulary_reduced.csv",
          row.names = FALSE)








#### Model Training and evaluation
table(matrix(c(1,2,3,4,5), c("english", "french", "german", "italian", "spanish"),
             nrow = 2, byrow = TRUE))

# create train and test data
train_id <- sample(nrow(data_red), round(nrow(data_red)*0.9))

X_train <- data.matrix(data[train_id,-1])
X_test <- data.matrix(data[-train_id, -1])


Y <- data$LANGUAGE
Y <- as.integer(as.factor(Y))-1

y_train <- Y[train_id]
y_test <- Y[-train_id]


# train the model
xgb <- xgboost(data = X_train, label = y_train,
               nrounds = 35, max_depth = 15,
               objective = "multi:softprob",
               num_class = 5)

# predict the test set
dim(X_test)[1]*5
y_pred <- predict(xgb, X_test)
y_predsss <- matrix(y_pred, ncol = 5, byrow = TRUE)
which.max(y_pred[1:5])

predictions <- apply(y_predsss, 1, which.max)

# modify y_test a little


# check correct preditions
corr_pred <- (y_test == (predictions-1))
sum(corr_pred)/length(y_test)


# save model
xgb.save(xgb, "Models/xgb_language.model")


#### Model Training and evaluation - Reduced data
train_id <- sample(nrow(data_red), round(nrow(data_red)*0.75))

X_train <- data.matrix(data_red[train_id,-1])
y_train <- data_red$LANGUAGE[train_id]
y_train <- as.factor(y_train)

X_test <- data.matrix(data_red[-train_id, -1])
y_test <- data_red$LANGUAGE[-train_id]
y_test <- as.factor(y_test)


# train the model
xgb <- xgboost(data = X_train, label = y_train,
               nrounds = 35, max_depth = 15)

# predict the test set
y_pred <- predict(xgb, X_test)
y_pred <- round(y_pred)

# modify y_test a little
y_testn <- as.numeric(y_test)

# check correct preditions
corr_pred <- (y_testn == y_pred)
sum(corr_pred)/length(y_test)

# save model
xgb.save(xgb, "Models/xgb_language_reduced.model")


xgb.save(xgb_language_model, "Models/xgb_model_with_AMAZING_performance.model")
write.csv(vocab, "Data/vocabulary_for_GREAT_model.csv",
          row.names = FALSE)


## cleaning
rm(list = ls()); gc()




# read in data
text_data <- readr::read_csv("Data/text_model_data.csv")
text_data %>%
  group_by(LANGUAGE) %>%
  count()

data_red <- readr::read_csv("Data/text_model_data_reduced.csv")

lyrics_data <- readr::read_csv("Data/billboard_lyrics.csv")
glimpse(lyrics_data)
lyrics_data <- lyrics_data %>%
  select(song, artist, lyrics) %>%
  distinct() %>%
  filter(!is.na(lyrics))








vocab <- readr::read_csv("Data/vocabulary.csv")
vocab <- as.vector(vocab$x)

xgb_language_model <- xgb.load("Models/xgb_language.model")



#### Reduced ####
vocab_red <- readr::read_csv("Data/vocabulary_reduced.csv")
vocab_red <- as.vector(vocab_red$x)

xgb_language_model_red <- xgb.load("Models/xgb_language_reduced.model")
xgb_language_model <- xgb_language_model_red



# prepare inputs
train_id <- sample(nrow(text_data), round(nrow(text_data)*0.75))

X_train <- data.matrix(text_data[train_id,-1])
y_train <- text_data$LANGUAGE[train_id]
y_train <- as.factor(y_train)

X_test <- data.matrix(text_data[-train_id, -1])
y_test <- text_data$LANGUAGE[-train_id]
y_test <- as.factor(y_test)




#### Get Models and vocabulary ####
xgb_model <- xgb.load("Models/xgb_model_with_AMAZING_performance.model")
vocab <- readr::read_csv("Data/vocabulary_for_GREAT_model.csv")
vocab <- as.vector(vocab$x)


## make numeric/language identifier
num_lang_identifier <- data.frame(Num = c(1,2,3,4,5),
                                  lang = c("english", "french", "german", "italian", "spanish"))

## predict lyrics language
lang <- character(length = nrow(lyrics_data))

for (i in 1:nrow(lyrics_data)){
  
  lang[i] <- detect_language(lyrics = lyrics_data$lyrics[i],
                             vocab = vocab,
                             num_lang_identifier = num_lang_identifier,
                             model = xgb_model)
  
  if (i%%1000 == 0){
    print(i)
  }
  
}




## check and play
sum(lang != "english", na.rm = TRUE)
sum(is.na(lang))

table(lang)

lyrics_data$lang <- lang

t <- other_lang$lyrics[279]
t <- lyrics_data$lyrics[231]
lyrics <- t

detect_language(t, vocab = vocab_red, num_lang_identifier = num_lang_identifier)

other_lang <- lyrics_data %>%
  filter(lang != "english")


key2 <- "f1e4ba73-7f9d-d842-3d3b-4661878a038b:fx"


# R wrapper for Deepl API
devtools::install_github("zumbov2/deeplr")
source("Code/deepl_api_key.R")



write.csv(other_lang, "Data/not_english_songs.csv",
          row.names = FALSE)


## clean the lyrics a little bit so deepl can work with them
other_lang$lyrics <- stringr::str_replace_all(other_lang$lyrics, "'\'", "")

other_lang$lyrics_engl <- as.character(NA)

## actually translate the lyrics

for (i in 155:nrow(other_lang)){
  
  translated_lyrics <- deeplr::translate2(
    text = other_lang$lyrics[i],
    target_lang = "EN",
    auth_key = key2
  )
  
  other_lang$lyrics_engl[i] <- translated_lyrics
  
}


translated_lyrics <- tryCatch(deeplr::translate2(
  text = other_lang$lyrics[i],
  target_lang = "EN",
  auth_key = key))


deeplr::usage2(key)$










