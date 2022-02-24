
## function to detect language based on lyrics
detect_language <- function(lyrics, vocab, num_lang_identifier, model){
  
  # little bit of cleaning
  lyrics <- stringr::str_replace_all(lyrics, "\n", " ")
  
  # tokenize the lyrics and unlist
  lyrics_tok <- unlist(tokenizers::tokenize_words(lyrics))
  
  # initialize empty vector to count
  count_vector <- numeric(length = length(vocab))
  
  for (word in 1:length(vocab)){
    # count the word and store
    count_vector[word] <- sum(lyrics_tok == vocab[word])
  }
  
  # get relative appearance
  count_vector <- count_vector/length(lyrics_tok)
  
  # get data as input data matrix
  X <- t(data.matrix(count_vector))
  
  # predict the language
  pred <- predict(model, X)
  pred <- which.max(pred)
  
  # match language
  pred_lang <- as.character(num_lang_identifier$lang[num_lang_identifier$Num == pred])
  
  # return language
  return(pred_lang)
}


## make numeric/language identifier
num_lang_identifier <- data.frame(Num = c(1,2,3,4,5),
                                  lang = c("english", "french", "german", "italian", "spanish"))





deepl_key <- XX









