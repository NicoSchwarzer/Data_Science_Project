##############################################
############# SENTIMENT SCORES ###############
## Checking how best to determine sentiment ##
## based on sentimentR and labelled dataset ##
##############################################


## based on the moody_df_lyrics Data crrated for this Algorithm and for the BERT Approaches 


if (!require("readr")) install.packages("readr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sentimentr")) install.packages("sentimentr")
if (!require("caret")) install.packages("caret")
if (!require("stringi")) install.packages("stringi")

library(readr)
library(tidyverse)
library(sentimentr)
library(caret)
library(stringi)

## Here thevalence shift algorithm is tested on those songs which appear oon the labelled Moddy Lyrics dataset as well as our data.
# A hyper-parameter search is conducted and based on the best parameters, the sentiment scores for all songs (until now) are retrieved! 

##############################
## Reading in relevant data ##
##############################

## Using the labelled Moody Lyrics Dataset 

## see: http://softeng.polito.it/erion/

moody_df <- read_csv("moody_df_lyrics.csv")



glimpse(moody_df)

ind <- sample(c(TRUE, FALSE), nrow(moody_df), replace=TRUE, prob=c(0.7, 0.3))
moody_df_train <- moody_df[ind, ]
moody_df_val <- moody_df[!ind, ]


##############
## Analysis ##
##############


## Using SentimentR's sentiment command for each sentence and then averaging over all sentences. 
## Sentences are identified by the get_sentences function. In case that "." are not present in the lyrics 
## (which is quite often the case), the sentences become quite long. This shoulf not be a problem - though!


## see: https://cran.r-project.org/web/packages/sentimentr/sentimentr.pdf
## also see: Jockers, M. L. (2017). Syuzhet: Extract sentiment and plot arcs from text. 

#Retrieved from https://github.com/mjockers/syuzhe


##############################################
## Grid-Search for optimal Hyper-Parameters ##
##############################################

## HyperParam Search for optimal Hyper-Parameters. Metric: F1 Score for those songs also
## in the Moody Lyrics dataset. Since the sentiment function returns a numeric value, a optimal treshold
## for mapping to the binaries is also required. 

## Parameters 
amplifier_weights <- seq(0.6, 1, by = 0.1) 
n_before <- seq(3,7, by = 1)
n_after  <- seq(3,7, by = 1)
tresholds <- seq(-0.3, 0.3, by=0.1)

f1 <- c()
a_w <- c()
n_bef <- c()
n_af <- c()
tre <- c()


for (a in amplifier_weights) {
  for (n in n_before) {
    for (na in n_after) {

    
      
      for (i in 1:nrow(moody_df_train)) {
     # getting mood predictions
      
      # encoding  
        moody_df_train$lyrics[i] <- stri_encode(moody_df_train$lyrics[i] , "", "UTF-8")
        
      # getting sentences 
      sent <- sentimentr::get_sentences(moody_df_train$lyrics[i])
        
      # removing all non-alpha-numeric chars from string - per sentence! 
      sent_rem <- str_replace_all(sent[1][[1]], "[^[:alnum:]]", " ")
      pred   <- sentimentr::sentiment(sentimentr::get_sentences(sent_rem), amplifier.weight = a, n.before = n, n.after = na)
      moody_df$mood_pred[i] <- mean(pred$sentiment)
      }
     
      
      for (t in tresholds) {
        
        print(paste0("Following combination : Amplifier weight is ", as.character(a), " n before is ", as.character(n), " n after is ", as.character(na), ". Treshold for assignment to positive is ", as.character(t), "."))
        
        # mapping to binaries
        moody_df_train$mood_pred2 <- "pos"
        moody_df_train$mood_pred2[moody_df_train$mood_pred < t  ] <- "neg"
        
        # getting f1 score
        moody_df_train$mood_pred2 <- as.factor(moody_df_train$mood_pred2)
        moody_df_train$mood <- as.factor(moody_df_train$mood)
        
        # account for case that all "pos"/"neg" are returned
        if (nrow(moody_df_train[moody_df_train$mood_pred2 == "pos",])  != 0) {
          if (nrow(moody_df_train[moody_df_train$mood_pred2 == "neg",])  != 0) {
          con_matrix <- caret::confusionMatrix(moody_df_train$mood, moody_df_train$mood_pred2)
          precision <- con_matrix$table[2,2] /  ( con_matrix$table[2,1] + con_matrix$table[2,2] )
          recall <-  con_matrix$table[2,2] / ( con_matrix$table[1,2] +  con_matrix$table[2,2] )
          f1_here <- 2 * (precision * recall) / (precision + recall)
                    
          # appending vectors
          f1 <- c(f1, f1_here)
          a_w <- c(a_w, a)
          n_bef <- c(n_bef, n)
          n_af <- c(n_af, na)
          tre <- c(tre, t)
          
        }
      }
     }
    }
  }
}


## Getting the best hyperparameters

index_best_f1 <- which.max(f1)   
aw_best <- a_w[index_best_f1]
n_bef_best <- n_bef[index_best_f1]
n_af_best <- n_af[index_best_f1]
tre_best <- tre[index_best_f1]

print(paste0("Best Amplifier weight is: ", as.character(aw_best), " best n before is ", as.character(n_bef_best), " best n after is ", as.character(n_af_best), " best treshold is ", as.character(tre_best) ))



#aw_best <- 1
#n_bef_best <- 6
#n_af_best <- 3 
#tre_best <- -0.3


## testing on val set ##


## prediciting 

for (i in 1:nrow(moody_df_val)) {
  # getting mood predictions
  
  # encoding  
  moody_df_val$lyrics[i] <- stri_encode(moody_df_val$lyrics[i] , "", "UTF-8")
  
  # getting sentences 
  sent <- sentimentr::get_sentences(moody_df_val$lyrics[i])
  
  # removing all non-alpha-numeric chars from string - per sentence! 
  sent_rem <- str_replace_all(sent[1][[1]], "[^[:alnum:]]", " ")
  pred   <- sentimentr::sentiment(sentimentr::get_sentences(sent_rem), amplifier.weight = a, n.before = n, n.after = na)
  moody_df$mood_pred[i] <- mean(pred$sentiment)
}


## evaluating
moody_df_val$mood_pred2 <- "pos"
moody_df_val$mood_pred2[moody_df_val$mood_pred < t  ] <- "neg"
  
# getting f1 score
moody_df_val$mood_pred2 <- as.factor(moody_df_val$mood_pred2)
moody_df_val$mood <- as.factor(moody_df_val$mood)
  

con_matrix <- caret::confusionMatrix(moody_df_val$mood, moody_df_val$mood_pred2)
precision <- con_matrix$table[2,2] /  ( con_matrix$table[2,1] + con_matrix$table[2,2] )
recall <-  con_matrix$table[2,2] / ( con_matrix$table[1,2] +  con_matrix$table[2,2] )
f1_here <- 2 * (precision * recall) / (precision + recall)
      


print(paste0("The f1 Score with optimal HyperParametrs on the Val set is ", str(f1_here), "!"))

## f1 of 0.89 ( !!!! )



####################################
## Getting all lyrical sentiments ## 
####################################


base_data_cleaned <- read_csv("base_data_cleaned.csv")


base_data_cleaned_reduced <- base_data_cleaned[,c("genre", "combination", "lyrics")]
df_uniques <- distinct(base_data_cleaned_reduced)


df_uniques$sent <- NA


for (i in 1:nrow(df_uniques)) {
  
  
  # overall length 
  len_lyrics <- length(str_split(df_uniques$lyrics[i], " ")[[1]])
  
  if (is.na(len_lyrics) == F)  { # to ensure functionality
    if ( len_lyrics < 5000) { # for reasons of computational burden
      
      
      df_uniques$lyrics[i] <- stri_encode(df_uniques$lyrics[i] , "", "UTF-8")
  
      # getting sentences 
      
  sent <- sentimentr::get_sentences(df_uniques$lyrics[i])
  
  # removing all non-alpha-numeric chars from string - per sentence! 
  sent_rem <- str_replace_all(sent[1][[1]], "[^[:alnum:]]", " ")
  pred   <- sentimentr::sentiment(sentimentr::get_sentences(sent_rem)    , amplifier.weight = aw_best, n.before = n_bef_best, n.after = n_af_best)
  df_uniques$sent[i] <- mean(pred$sentiment)

  }
}
    
  print(i)
}



## re-mapping to all available data ##

base_data_cleaned <- read_csv("base_data_cleaned.csv")

## to base_data_cleaned 
nrow(base_data_cleaned)


base_data_cleaned <- dplyr::left_join(base_data_cleaned, df_uniques[, c("combination", "sent") ], by = "combination")

write_csv(base_data_cleaned, "base_data_cleaned.csv")



## to base_data_raw 
base_data_raw <- read_csv("base_data_raw.csv")
base_data_raw <- dplyr::left_join(base_data_raw, df_uniques[, c("combination", "sent") ], by = "combination")
rm(base_data_raw)






