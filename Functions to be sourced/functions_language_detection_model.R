
tok_stems <- function(text, language = "english", stems = TRUE){
  
  gc()
  
  
  if (stems == TRUE){
    # tokenize
    output <- tokenize_word_stems(text,
                                  language = language)
  } else if (stems == FALSE){
    # tokenize
    output <- tokenize_words(text)
  }
  
  # make "nice" dataframe
  output <- data.frame(words = output[[1]],
                       stringsAsFactors = FALSE)
  
  return(output)
  
  gc()
  
}



count_filter <- function(tooks_list, str_len_filter = 1, n_filter = 0){
  
  gc()
  
  # get number of books
  no_books <- length(tooks_list)
  
  output <- data.frame()
  
  # loop through and rbind
  for (i in 1:no_books){
    output <- rbind(output, tooks_list[[i]])
  }
  
  
  # count and filter depending on filter argument
  output <- output %>%
    count(words, sort = TRUE)
  
  filter_arg <- stringr::str_length(output$words) > str_len_filter
  output <- output[filter_arg, ]
  
  
  # get only those words with certain amount of counts
  output <- output %>%
    filter(n > n_filter)
  
  
  return(output)
  
  gc()
}





## function to count and returns data frame with relative counted values

count_paragraphs <- function(text, word_variables, language, x = c(20, 50, 100, 300, 500), prob = c(0.1, 0.1, 0.15, 0.6, 0.05)){
  
  gc()
  
  # tokenize the text
  toks <- tokenize_words(text)
  
  # unlist and make nice
  d <- data.frame(words = toks[[1]],
                  stringsAsFactors = FALSE)
  
  para_length <- c(0, sample(x = x,
                             floor(nrow(d)/sum(x*prob)),
                             prob = prob, replace = TRUE))
      
  
  # initialize empty data frame to store the counts
  data_count <- data.frame(matrix(0, ncol = length(word_variables)+1,
                                  nrow = (length(para_length)-1)))
  
  # set up column names 
  colnames(data_count) <- c("LANGUAGE", word_variables)
  
  ## split the text (book) in paragraphs of 300 words, count the words from vocab
  ## and store relative appearance
  
  for (i in 1:(length(para_length)-1)){
    
    # extract the paragraph
    paragraph <- d$words[(sum(para_length[1:i])+1):((sum(para_length[1:i]))+para_length[i+1])]
    
    # initialize the vector in which the counts are stored
    count_vector <- numeric(ncol(data_count))
    
    # count every word in vocab for the text paragraph
    for (word in 1:ncol(data_count)){
      
      # store count in word vector
      count_vector[word] <- sum(paragraph == colnames(data_count)[word])
    }
    
    # store the (relative) count in the dataframe
    data_count[i,] <- count_vector/length(paragraph)
    
  }
  
  # add language to data_frame
  data_count$LANGUAGE <- language
  
  # return the counted values
  return(data_count)
  
  gc()
  
}









