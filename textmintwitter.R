library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
tweetcred <- OAuthFactory$new(consumerKey='9pvy4UUvhF5dXdqJIepBugZ34', # Consumer Key (API Key)
                         consumerSecret='BRmuoSewdk4ZCEdvtsJADRWcXwpxJLlZYFle8zZ6f9LRGHAZ4L', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token', accessURL='https://api.twitter.com/oauth/access_token',
                        authURL='https://api.twitter.com/oauth/authorize')

save(tweetcred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("9pvy4UUvhF5dXdqJIepBugZ34", 
                    "BRmuoSewdk4ZCEdvtsJADRWcXwpxJLlZYFle8zZ6f9LRGHAZ4L",
                    "1304705444212961283-NqnG84howpXP8uYtHPdK3m64pOOzUY", 
                    "2h4ZJmkTGaSD3MKA27FKbskvbTPzuki5l98SmTN7wLv7X")  


kangana_list <- searchTwitter("@Kangana", n=1000)
library(dplyr)

kangana <- bind_rows(lapply(kangana_list, as.data.frame))
View(kangana)
library(lubridate)

kangana$date <- day(kangana$created)
kangana$hour <- hour(kangana$created)
library(ggplot2)

ggplot(kangana, aes(x = date)) + 
  geom_density()
ggplot(kangana, aes(x = hour)) + 
  geom_density()
kangana$text <- gsub("@[[:alpha:]]*","", kangana$text)
library(tm)
#sentimental analysis

#install.packages("sentimentr")
library(sentimentr)

corpus<- as.factor(kangana)
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
### Cleaning Data###
corpus <- tm_map(corpus,tolower)
#inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
#inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
#inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
#inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
#inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
#inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('jayabachchan','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'jayabachchan', replacement = 'celebrity')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10]
w <- rowSums(tdm)  # no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
#install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 1.0, shape = 'rectangle', rotateRatio = 1.0, minSize = 1)
letterCloud(w,word = 'Am',frequency(5), size=1)

#######Sentiment Analysis  ######

reviews <- as.character(kangana)
class(reviews)
score <- get_nrc_sentiment(reviews)
head(score)
reviews[4]
get_nrc_sentiment('happy')  ### as happy comes more frequently
barplot(colSums(score), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for twitter Reviews')

