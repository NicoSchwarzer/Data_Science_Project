##############################################
############# SENTIMENT SCORES ###############
## Checking how best to determine sentiment ##
## based on sentimentR and labelled dataset ##
##############################################


if (!require("readr")) install.packages("readr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sentimentr")) install.packages("sentimentr")
if (!require("caret")) install.packages("caret")

library(readr)
library(tidyverse)
library(sentimentr)
library(caret)



##############################
## Reading in relevant data ##
##############################

## Using the labelled Moody Lyrics Dataset 

## see: http://softeng.polito.it/erion/

moody_df <- read_csv("MoodyLyrics.csv")

#glimpse(moody_df)

moody_df$combination <- paste0(moody_df$artist,"  ", moody_df$title)

base_data_cleaned <- read_csv("base_data_cleaned.csv")
## important now - > len refers to overall lyrics length while len1 refers to to reduced length!
xx <- base_data_cleaned[, c("lyrics", "combination", "combination_1", "combination_2")]
df_uniques <- distinct(xx)
rm(xx)



moody_df_a <- moody_df %>% dplyr::inner_join(df_uniques, by = c("combination" = "combination"))
moody_df_b <- moody_df %>% dplyr::inner_join(df_uniques, by = c("combination" = "combination_1"))
moody_df_c <- moody_df %>% dplyr::inner_join(df_uniques, by = c("combination" = "combination_2"))

xx <- rbind(moody_df_a[, c("mood", "lyrics")], moody_df_b[, c("mood", "lyrics")], moody_df_c[, c("mood", "lyrics")])
nrow(xx) # 1613                                                                                           


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
      
      
      
      for (i in 1:nrow(xx)) {
        # getting mood predictions
        # removing all non-alpha-numeric chars from string! 
        xx$lyrics[i] <- str_replace_all(df_uniques$lyrics[i], "[^[:alnum:]]", " ")
        pred   <- sentimentr::sentiment(sentimentr::get_sentences(xx$lyrics[i]), amplifier.weight = a, n.before = n, n.after = na)
        xx$mood_pred[i] <- mean(pred$sentiment)
      }
      
      
      for (t in tresholds) {
        
        print(paste0("Following combination : Amplifier weight is ", as.character(a), " n before is ", as.character(n), " n after is ", as.character(na), ". Treshold for assignment to positive is ", as.character(t), "."))
        
        # mapping to binaries
        xx$mood_pred2 <- "pos"
        xx$mood_pred2[xx$mood_pred < t  ] <- "neg"
        
        # getting f1 score
        xx$mood_pred2 <- as.factor(xx$mood_pred2)
        xx$mood <- as.factor(xx$mood)
        
        # account for case that all "pos"/"neg" are returned
        if (nrow(xx[xx$mood_pred2 == "pos",])  != 0) {
          if (nrow(xx[xx$mood_pred2 == "neg",])  != 0) {
            con_matrix <- caret::confusionMatrix(xx$mood, xx$mood_pred2)
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

## Importantly, since no actual training of the algorithms is done (no weight optimization), there is no need for splitting into a test and train set!

index_best_f1 <- which.max(f1)   
aw_best <- a_w[index_best_f1]
n_bef_best <- n_bef[index_best_f1]
n_af_best <- n_af[index_best_f1]
tre_best <- tre[index_best_f1]

print(paste0("Best Amplifier weight is: ", as.character(aw_best), " best n before is ", as.character(n_bef_best), " best n after is ", as.character(n_af_best), " best treshold is ", as.character(tre_best) ))


## f1 of 0.91 !!!
#aw_best <- 1
#n_bef_best <- 6
#n_af_best <- 3 
#tre_best <- -0.3


####################################
## Getting all lyrical sentiments ## 
####################################


df_uniques$sent <- NA
df_uniques$best_sent <- NA
df_uniques$worst_sent <- NA


for (i in 1:nrow(df_uniques)) {
  
  # removing all non-alpha-numeric chars from string! 
  a <- str_replace_all(df_uniques$lyrics[i], "[^[:alnum:]]", " ")
  if (is.na(nchar(a)) == F)  {
    if (nchar(a) <= 3500 )  { # to ensure proper functionality!
      sent <- sentimentr::sentiment(sentimentr::get_sentences(a) , amplifier.weight = aw_best, n.before = n_bef_best, n.after = n_af_best)
      df_uniques$sent[i] <- mean(sent$sentiment)
      
    }
  }
  
  print(i)
}


## re-mapping to all available data ##



## to base_data_cleaned 
nrow(base_data_cleaned)


base_data_cleaned <- dplyr::left_join(base_data_cleaned, df_uniques[, c("combination", "best_sent", "worst_sent") ], by = "combination")

write_csv(base_data_cleaned, "base_data_cleaned.csv")



## to base_data_raw 
base_data_raw <- read_csv("base_data_raw.csv")
base_data_raw <- dplyr::left_join(base_data_raw, df_uniques[, c("combination", "sent") ], by = "combination")




#####################
library(lubridate)
library(plotly)


aa  <-base_data_cleaned %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  summarise(Scores = mean(sent, na.rm = T) ) %>%
  filter(genre != "unknown genre")


bb  <-base_data_cleaned %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "month")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Scores = mean(sent, na.rm = T) )

bb



plotData <-  aa 

ggplotly(
  ggplot(data= plotData , aes(x=Date,  y= Scores) ) +
    geom_line( aes(colour = genre)) + 
    xlab("Time") + 
    ylab("Mean Sentiment score") + 
    ggtitle("Mean Sentiment Score per song")+
    stat_smooth(method=lm, colour = "black") + 
    labs(  subtitle="Overall trend in black") + 
    scale_color_brewer(palette = "Blues") + 
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 13, hjust = 0.5),
          legend.text = element_text(size=12), 
          legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
) 

### YES :D 

ggplot(data= bb , aes(x=Date,  y= Scores) ) +
  geom_line() + 
  xlab("Time") + 
  ylab("Mean Sentiment score") + 
  geom_vline(xintercept =   bb$Date[723], color = "red") +
  geom_vline(xintercept =   bb$Date[739], color = "blue") +
  geom_vline(xintercept =   bb$Date[497], color = "red")  + 
  
  ## financial crisis
  geom_vline(xintercept =   bb$Date[574], color = "red")  + 
  geom_vline(xintercept =   bb$Date[592], color = "red")  +
  annotate("rect", xmin = bb$Date[574], xmax = bb$Date[592], ymin = -Inf, ymax = Inf,
           alpha = .4) + 
  
  geom_vline(xintercept =   bb$Date[186], color = "red")  + 
  geom_vline(xintercept =   bb$Date[33], color = "red") +
  
  geom_vline(xintercept =   bb$Date[253], color = "red") + 
  geom_vline(xintercept =   bb$Date[277], color = "red") + 
  annotate("rect", xmin = bb$Date[253], xmax = bb$Date[277], ymin = -Inf, ymax = Inf,
           alpha = .4) + 
  ## end of soviet union 
  geom_vline(xintercept =   bb$Date[358], color = "red") + 
  geom_vline(xintercept =   bb$Date[385], color = "red") +   annotate("rect", xmin = bb$Date[358], xmax = bb$Date[385], ymin = -Inf, ymax = Inf,
                                                                      alpha = .4) + 
  ## iraq war No2 Start 
  geom_vline(xintercept =   bb$Date[519], color = "red")  + 
  geom_text(aes(x=bb$Date[519], label="Start of Iraq War", y=0.6), colour="blue", angle=90, vjust = 0.2, text=element_text(size=11))


################################

base_data_cleaned$sent2 <- 0 
base_data_cleaned$sent2[base_data_cleaned$sent > 0.3] <- 1 

## binary case! 

aa  <-base_data_cleaned %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  summarise(Scores = mean(sent2, na.rm = T) ) %>%
  filter(genre != "unknown genre")





bb  <-base_data_cleaned %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "month")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Scores = mean(sent2, na.rm = T) )

bb








505 # 9/11 
588 # econ crisis 
723 # covid (#1)
727
185 # end of vietnam war 





bb$Date

bb[, bb$Date == "2001-01-01"]



rm(base_data_cleaned)
rm(base_data_raw)
rm(xx)
rm(df_uniques)
rm(amplifier_weights)
rm(n_before)
rm(n_after)
rm(tresholds)
rm(f1)
rm(a_w)
rm(n_bef)
rm(n_af)
rm(tre)











###################



a <- quanteda::dfm(base_data_cleaned$lyrics[1:20], remove_punct = TRUE)

xx <- quanteda.textstats::textstat_simil(a)

xx[2000,]



### diversity 


quanteda.textstats::textstat_lexdiv(a)


## correspondance analysis 


tmod_ca <- quanteda::textmodel_ca(a)

require(quanteda)
install.packages("seededlda")

require(seededlda)

seededlda::topics(a)

# load dictionary containing seed words
dict_topic <- dictionary(file = "../dictionary/topics.yml")
print(dict_topic)

head(dat_ca)



#### topic models with seeded LDA 




#### idea

## use PCA 
## then clustering technqies and display results 


corpus = tm::Corpus(tm::VectorSource(df_uniques$lyrics)) # or ly! 
tdm <- tm::DocumentTermMatrix(corpus)

tdm.tfidf <- tm::weightTfIdf(tdm)


##

tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
# Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

