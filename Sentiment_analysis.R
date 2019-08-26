#Steps Involved
#Using Flickr API we get comments of the most interesting photos in Flickr
#the data is written to a csv file 'Comment_flickr.csv'
#We process the data using tm package in R
#Processing include: converting to lower case,removePunctuation,removeNumbers,remove stopwords,stripWhitespace
#Then we get the most occuring words using the wordcloud
#For sentiment analysis we use the syuzhet package and the nrc lexicon in r 


install.packages("installr"); library(installr) # install+load installr

updateR() # updating R.

install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

rm(list=ls())

interestingPics = GET(url = "https://api.flickr.com/services/rest/?method=flickr.interestingness.getList&api_key=0a1bac1babf9d30116a621d799167e2e&format=json&nojsoncallback=1&auth_token=72157691216024373-2665fe7e518150ec&api_sig=2a5cfb821403244501d86d51cd65f116")
interestingPics =content(interestingPics)$photos$photo
photoIds = c()
for (i in 1:length(interestingPics)) {
  photoIds[i] <- interestingPics[i][[1]]$id
}

photoid =c()
comment = c()
myData = data.frame(photoId = photoid, comments = comment)

for (photoId in photoIds){
  commentData = GET(url = paste("https://api.flickr.com/services/rest/?method=flickr.photos.comments.getList&api_key=ad6788401343506016a8d4b83be76f96&format=json&nojsoncallback=1", "&photo_id=", photoId, sep = ""))
  commentData = content(commentData)$comments$comment
  if (length(commentData) > 0){
    for (i in 1: length(commentData)){
      photoid[i] <- photoId
      comment[i] <- commentData[i][[1]]$"_content"
  }
  }
  tempData = data.frame(photoId = photoid, comments = comment)
  myData = rbind(tempData, myData)
  photoid =c()
  comment = c()
}

View(myData)
write.csv(myData,"Comment_flickr.csv")


install.packages("pacman")
pacman::p_load(tm)

install.packages("slam")
length(which(!complete.cases(myData)))
install.packages("tm")
library(tm)
corpus<-iconv(myData$comments)
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
clean_set<-tm_map(corpus,removeWords,stopwords('english'))
length(clean_set)
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
clean_set <- tm_map(clean_set, content_transformer(removeURL))

clean_set<-tm_map(clean_set,stripWhitespace)


tdm<-TermDocumentMatrix(clean_set)
tdm<-as.matrix(tdm)
tdm[1:10,1:20]


w<-rowSums(tdm)
w<-subset(w,w>=50)
barplot(w,las=2,col=rainbow(50))



library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

install.packages("syuzhet")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
comments<-iconv(myData$comments)
s<-get_nrc_sentiment(comments)


barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Flickr Comment')
