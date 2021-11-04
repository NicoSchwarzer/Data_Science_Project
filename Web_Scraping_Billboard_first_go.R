######################################
## Web Scraping Billboard first go  ##
######################################

setwd("C:\\Users\\Nico\\Documents\\Uni\\3. Sem\\DS Projekt\\Code_and_Data")


### This file serves to obtain artist and song information from the Billboard Top 100 for every week
### If this has not been done before!
### This is done by scraping the billboard website from 1960 til now. 


if (!require("rvest")) install.packages("rvest")
if (!require("lubridate")) install.packages("lubridate")
if (!require("writexl")) install.packages("writexl")
if (!require("tidyverse")) install.packages("tidyverse")


library(lubridate)
library(rvest)
library(writexl)
library(tidyverse)


### Set-Up of this script ### 

## 1) Meta-Data needed for Web Scraping 
## 2) Functions assisting in scraping and post-processing 
## 3) Creating a date vector of all saturdays from 1970 til now 
## 4) Scraping the site and storing all data in one large DF



################################
## Meta-Info for Web Scraping ##
################################


base_url <-  "https://www.billboard.com/charts/hot-100"

x_path_data <- '//*[contains(concat( " ", @class, " " ), concat( " ", "text--truncate", " " ))]'
x_path_date <- '//*[contains(concat( " ", @class, " " ), concat( " ", "button--link", " " ))]'



########################
## Relevant functions ##
########################

## function for getting last available date on Billboard   ## 
## important - this needs to be a saturday (is by default) ##

get_last_date <- function(url, x_path_date) {
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- html_elements(result_1, xpath= x_path_date)
  
  html_text <- rvest::html_text(xml_nodes_1)
  
  date_here <- substr(html_text, start = 18, stop = 33)
  
  # in correct day format
  date_format <- lubridate::mdy(date_here)
  
  return(date_format)
}



## function for scraping and returning list of chart data  ##
## the url must contain a date info, which shall be passed ##

get_charts_list <- function(url, path_1) {
  # function to scrape based URL and x_path using rvest package 
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- html_elements(result_1, xpath= path_1)
  
  # retrieving relevant text with charts 1-100 
  charts <- list(rvest::html_text(xml_nodes_1))[[1]][13:212]
  
  return(charts)
}



## function for creating a DF from scraped data ##
## input must be output of get_charts_list      ## 

get_table_from_scraped <- function(scraped_text) {
  
  # building a table from the html text
  
  songs <- rep("a", length(scraped_text)*0.5) # instantiating /w fitting data type, i.e. string
  artists <- rep("a", length(scraped_text)*0.5) # instantiating /w fitting data type, i.e. string
  
  s = 1 # to use as counter for songs
  a = 1 # to use as counter for artists
  
  for (i in 1:length(scraped_text)) {
    
    if (i%%2 != 0) {   # song if index %% 2 != 0
      (songs[s] <- scraped_text[i])
      (s = s+ 1) 
    } else { # song if index %% 1 == 0
      (artists[a] <- scraped_text[i])
      (a = a + 1) 
    }
  }

  all_info <- tibble(songs, artists)
  
  return(all_info)
  
}


################################
## Getting all relevant dates ##
################################

## One has to add a date to the base URL to scrape the charts of each week
## This needs to be the Saturday of each week 

## getting the current date (current week's saturday)


# the last available Saturday
last_date <- get_last_date(base_url, x_path_date)

# date vector starting from first Saturday in 1960
vec_all_dates <- seq(as.Date("1960-01-02"), last_date, by="days")

# every day for every two weaks at that week's Saturday
vec_7_days <- vec_all_dates[seq(1, length(vec_all_dates), 7)]


##############################
## Scraping the Information ##
##############################


## initializing empty vectors to be appended

songs <- c()
artists <- c()
dates <- c(vec_7_days[1]) # initialized not empty to ensure data type is passed correctly
dates <- dates[-1]





## running a big loop to scrape all data
a <- 1
len <-length(vec_7_days)


## using a while statement togther with sys.sleep to not overload the Page

while (a <= 2) {
  
  for (i in a:(a+9)) {
    
    # creating fitting url 
    print(i)
    
    date_char <- as.character(vec_14_days[i])
    url_final <- paste(base_url, "/", date_char, sep="")
    
    # getting data 
    scraped_data <- get_charts_list(url_final, x_path_data)
  
    # cleaning data 
    cleaned_data <- get_table_from_scraped(scraped_data)
    
    # appending vectors 
    songs <- c(songs, cleaned_data$songs)
    artists <- c(artists, cleaned_data$artists)
    dates_here <- rep(vec_7_days[i], nrow(cleaned_data))
    dates <- c(dates, dates_here)
    
  }  
    
  Sys.sleep(40)
  a = a + 10

}



# getting one large DF from the results
df_all_data_billboard <- data.frame(artists, songs, dates)

# saving to csv 
write.csv(all_data_billboard,"all_data_billboard_weeks.csv")

