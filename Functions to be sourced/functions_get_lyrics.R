
#########################
## Alte Mehode mit API ##
### API hat nun BUG #####
#########################

## function to return all possible combinations of a song/artist name
get_strcombinations <- function(str){
  
  # initialize str vector
  str_combination <- c(str)
  
  ## First Cleaning: Can Always be replaced
  a1 <- stringr::str_replace_all(str, "'", "")
  a1 <- stringr::str_replace_all(a1, "!", "")
  a1 <- stringr::str_replace_all(a1, "&amp;", "&")
  a1 <- stringr::str_replace_all(a1, ",", "")
  a1 <- stringr::str_replace_all(a1, "\\?", "")
  a1 <- stringr::str_replace_all(a1, "\\.\\.\\.", " ")
  a1 <- stringr::str_replace_all(a1, "\\.", "")
  a1 <- stringr::str_replace_all(a1, "\\*\\*", "uc")
  a1 <- stringr::str_replace_all(a1, "\\$", "s")
  a1 <- stringr::str_replace_all(a1, "\\[", "")
  a1 <- stringr::str_replace_all(a1, "\\]", "")
  a1 <- stringr::str_replace_all(a1, "\\:", "")
  a1 <- stringr::str_replace_all(a1, "\\#", "")
  
  # append first cleaning
  str_combination <- append(str_combination, a1)
  
  
  ## Second Cleaning: Get Only First str
  a2 <- stringr::str_replace_all(a1, " \\&.*", "")
  a2 <- stringr::str_replace_all(a2, " and.*", "")
  a2 <- stringr::str_replace_all(a2, " And.*", "")
  
  # append second cleaning
  str_combination <- append(str_combination, a2)
  
  
  ## Third Cleaning: Get Only First str without featuring
  a3 <- stringr::str_replace_all(a1, " Featuring.*", "")
  a3 <- stringr::str_replace_all(a3, " featuring.*", "")
  a3 <- stringr::str_replace_all(a3, " Feat.*", "")
  a3 <- stringr::str_replace_all(a3, " with.*", "")
  a3 <- stringr::str_replace_all(a3, " With.*", "")
  a3 <- stringr::str_replace_all(a3, "\\&", "and")
  
  # append third cleaning
  str_combination <- append(str_combination, a3)
  
  
  ## Fourth Cleaning: Get all str with removed &
  a4 <- stringr::str_replace_all(a1, "\\&", "and")
  
  # append fourth cleaning
  str_combination <- append(str_combination, a4)
  
  
  ## Fifth Cleaning: Just str without parentheses
  a5 <- stringr::str_replace_all(a4, " \\(.*", "")
  
  # append fifth cleaning
  str_combination <- append(str_combination, a5)
  
  
  ## Sixth Cleaning: Remove all parentheses
  a6 <- stringr::str_replace_all(a4, "\\(", "")
  a6 <- stringr::str_replace_all(a6, "\\)", "")
  
  
  
  # append sixth cleaning
  str_combination <- append(str_combination, a6)
  
  
  
  # return unique str combinations
  str_combination <- unique(str_combination)
  
  return(str_combination)
  
  
}



## function to get lyrics from Genius using the URL
get_lyrics_genius_url <- function(a, s){
  a2 <- str_replace_all(a, " ", "-")
  s2 <- str_replace_all(s, " ", "-")
  url <- paste0("https://genius.com/",
                a2, "-", s2, "-lyrics")
  # get lyrics
  lyrics <- try({geniusr::get_lyrics_url(url)}, silent = TRUE)
  
    # geniusr::get_ly
  
  return(lyrics)
}



## function to obtain lyrics given an artist name vector and a song name vector
get_lyrics <- function(artist_vector, song_vector, repeats = 5, print_comb = FALSE){
  
  # go through every possible combinations of artists and songs
  for (a in artist_vector){
    
    for (s in song_vector){
      
      
      # count the tries
      iteration <- 0
      
      # try to get lyrics a few times, escape if found
      while(iteration < repeats){
        # try get lyrics via URL
        lyrics <- get_lyrics_genius_url(a, s)
        
        # print(lyrics)
        
        # escape the loop if found or after x try
        if(is.null(nrow(lyrics))){
          iteration <- iteration + 1
        } else {
          iteration <- iteration + 1 + nrow(lyrics)
        }
      } # close the while loop
      
      if (print_comb == TRUE){
        print(c(a, s))
      }
      
      
      # if lyrics are found break out of the songs loop
      if(!is.null(nrow(lyrics))){
        # if something is found break out of the loop
        if(nrow(lyrics) > 0){
          break
        }
      }
      
      
    } # close loop through song
    
    # if something is found, break out through the artist loop
    if(!is.null(nrow(lyrics))){
      # if something is found break out of the loop
      if(nrow(lyrics) > 0){
        break
      }
    }
    
    
  } # close loop through artist
  
  
  if(!is.null(nrow(lyrics))){
    if(nrow(lyrics) > 0){
      # collapse lyrics
      lyrics <- paste(lyrics$line, collapse = " ")
      # if enabled, print successful combination
      if (print_comb == TRUE){
        print(paste0("Successfull Combination: ", a, " with ", s))
      }
      # return lyrics
      return(lyrics)
    }else {
      return(NA)
    }
  } else {
    return(NA)
  }
  
}  # close the function





####################################
## Alternative method mit Scrapen ##
####################################





get_lyrics_genius_url_al <- function(a, s){
  a2 <- str_replace_all(a, " ", "-")
  s2 <- str_replace_all(s, " ", "-")
  url <- paste0("https://genius.com/",
                a2, "-", s2, "-lyrics")
  
  # get lyrics via scraping 
  result_1 <- rvest::read_html(url)
  
  nodes <- xml_nodes(result_1, css = '.jYfhrf:nth-child(5) , .hHEDka+ .jYfhrf')
  
  len_nodes <- length(nodes)
  
  lyrics_str <- ""
  
  for (i in 1:len_nodes) {
    lyrics_text_i <- html_text(nodes[i])
    lyrics_str <- paste0(lyrics_str, " ", lyrics_text_i)
  }
  
  # removing [Verse], [Chorus], [Bridge], []
  
  lyrics_str <- gsub('Chorus', '', lyrics_str)
  lyrics_str <- gsub('Bridge ', '', lyrics_str)
  lyrics_str <- gsub('Verse 1', '', lyrics_str)
  lyrics_str <- gsub('Verse 2', '', lyrics_str)
  lyrics_str <- gsub('Verse 3', '', lyrics_str)
  lyrics_str <- gsub('Verse 4', '', lyrics_str)
  lyrics_str <- gsub('Verse 5', '', lyrics_str)
  lyrics_str <- gsub('Verse 6', '', lyrics_str)
  
  # removing all non-ahpa numerics Regex
  lyrics_str <- gsub('[^[:alnum:]]', " ", lyrics_str)
  
  # dealing with double empty spaces
  lyrics_str <- gsub('  ', " ", lyrics_str)
  
  # dealing with 's nad 'll
  lyrics_str <- gsub(' s ', "'s ", lyrics_str)
  lyrics_str <- gsub(' ll ', "'ll ", lyrics_str)
  lyrics_str <- gsub(' ve ', "'ve ", lyrics_str)
  lyrics_str <- gsub(' m ', "'m ", lyrics_str)
  
  
  
  # In the text now, commas re replaced by camel case. This is tackled here
  lyrics_str <- gsub("([A-Z])", ", \\1", lyrics_str)
  lyrics_str <- gsub("  , ", "", lyrics_str)
  
  #print(lyrics_str)
  
  
  # geniusr::get_ly
  
  return(lyrics_str)
}


# wrapping in a possibly statetement (purrr)
poss_lyrics <- possibly(get_lyrics_genius_url_al, otherwise = "NA")


## function no 2 for retrieving lyrics 

get_lyrics_alt <- function(artist_vector, song_vector, repeats = 5, print_comb = FALSE){
  
  # go through every possible combinations of artists and songs
  for (a in artist_vector){
    
    for (s in song_vector){
      
      
      # count the tries
      iteration <- 0
      
      # try to get lyrics a few times, escape if found
      while(iteration < repeats){
        # try get lyrics via URL
        lyrics <- poss_lyrics(a, s)
        
        # print(lyrics)
        
        # escape the loop if found or after x try
        if(is.na(lyrics)){
          iteration <- iteration + 1
        #} else {
        #  iteration <- iteration + 1 + nrow(lyrics)
        #}
      } # close the while loop
      
      if (print_comb == TRUE){
        print(c(a, s))
      }
      
      
      # if lyrics are found break out of the songs loop
      if(is.na(lyrics) == F){
          break
        }
      
      
      } # closes iteration loop 
      
    } # close loop through song
    
    # if something is found, break out through the artist loop
    if(is.na(lyrics) == F){
      break
    }
    
    } # close loop through artists       
    
  
  
      return(lyrics)


}  # close the function





