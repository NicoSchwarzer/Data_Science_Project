rm(list = ls())

## load packages
library(xml2)
library(rvest)

## set woring directory
setwd("C:/Users/leona/Documents/Data_Science_Project/Data")


## create date vector 
starting_date <- as.Date("1960-01-02")
d <- cumsum(rep(7, 3355))

dates <- starting_date + d
dates <- c(starting_date, dates)
dates <- dates[(dates < "2021-11-19")]   # cut of got a little too long

# enable for test-runs
#dates <- dates[(length(dates)-20):length(dates)]

## Set parameters for scraping

# set base url
base_url <- "https://www.billboard.com/charts/hot-100/"

# create empty data frame to store results
df <- data.frame(matrix(nrow = length(dates)*100,
                        ncol = 3))
colnames(df) <- c("week", "song", "artist")
# specify column types
df$week <- as.Date(df$week)
df$song <- as.character(df$song)
df$artist <- as.character(df$artist)


counter <- 1

for (i in 1:length(dates)){
  
  # get current URL
  url <- paste0(base_url, dates[i])
  
  # get website content
  hot100 <- xml2::read_html(url)
  
  # write website content as txt file
  xml2::write_xml(rvest::html_node(hot100, 'body'), "hot100.txt")
  
  # read in again as csv file 
  hot100csv <- read.delim("hot100.txt", sep = "|", header = FALSE)
  
  # extract artists
  artists <- hot100csv[which(hot100csv$V1 == "</h3>")+2,]
  
  # extract songs
  songs <- hot100csv[which(hot100csv$V1 == "</h3>")-1,]
  
  # put them together as dataframe
  help_df <- data.frame(songs, artists)
  
  # remove uneccessary rows and columns
  start_idx <- which(help_df[,1] == "Additional Awards")
  help_df <- help_df[(start_idx+1):(start_idx+100), c(1,4)]
  
  # optional: remove row names
  rownames(help_df) <- NULL
  
  # append to final dataframe df
  df$week[counter:(counter+99)] <- as.Date(as.character(dates[i]))
  df$song[counter:(counter+99)] <- as.vector(help_df[,1])
  df$artist[counter:(counter+99)] <- as.vector(help_df[,2])
  
  # increase counter
  counter <- counter + 100
  
  Sys.sleep(5.5)
  
}









