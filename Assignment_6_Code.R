#First, update R to make sure that you're using the latest version.


#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("base64enc")
#install.packages("httr")
#install.packages("httpuv")
#install.packages("curl")
#install.packages("devtools")
#install.packages("bit")
#
#install.packages("RTextTools")
#install.packages("topicmodels")

library(twitteR)
library(ROAuth)
library(base64enc)
library(httr)
library(httpuv)
library(curl)
library(devtools)
library(bit)
library(RTextTools)
library(topicmodels)
library(RCurl) 

#setwd("C:/Users/eugeneby/Dropbox/Documents/Work and School/Teaching/CPLN 671 - Statistics and Data Mining/Data/Lecture 24")

#    1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
httr::oauth_endpoints("twitter")

#    2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410"
#    Get your own consumer key and consumer secret code, as well as the 
#    access token and access secret key and replace the info below

#PW: penn2020





ConsumerKey <- "vyrJ42qGmhMX0VbzXSFzUzS3Y"
ConsumerSecret <- "ovibs84go0bfXrOwxaTow1dQJEYbCC4sKf6ekIDeOmhseHcaR2"
AccessToken <- "3940172242-cjqsawzzdvaXt7d4rSSyYNHBQJKWflbUHHr2bqw"
AccessSecret <- "k20LXhXdb9X3lXDDH73nofavZ9KfcRD0Mc8kB2eFdD2iM"

#    Type 1 and hit Enter if 1 appears on the screen
twitteR::setup_twitter_oauth(ConsumerKey,ConsumerSecret,AccessToken,AccessSecret)


#    Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


#    Q: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2014-March/000547.html
#    My twitteR is github version 1.1.7. Have you any suggestions about how to
#    get more than 100000 search results?

#    A: You can't. It is a limitation of the API that it will only let you go back 
#    a handful of days.

covid <- searchTwitter("covid", lang="en", n=10000)

upenn <- searchTwitter("upenn", lang="en", n=1000)
winteriscoming <- searchTwitter("winteriscoming", lang="en", n=100000)
omicron <- searchTwitter("omicron", lang="en", n=100000)


install.packages("tm")
#install.packages("RTextTools")
install.packages("SnowballC")

library(tm)
#library(RTextTools)
library(SnowballC)

#     Saving twitter feed as a data frame
df <- do.call("rbind", lapply(omicron, as.data.frame))
df <- do.call("rbind", lapply(covid, as.data.frame))

covid.m <- as.matrix(covid)
covid.df <- as.data.frame(covid.m)
df <- covid.df

#     If twitteR is not working, we have a file with winteriscoming tweets saved (i.e., we save septa1 data
#     frame as a .csv file and then reimport it and call it df)
#write.csv(df, file="WinterIsComing.csv")
#df <-read.csv(file="WinterIsComing.csv")



#     Corpora are collections of documents containing (natural language) text. 
#     Used in packages which employ the infrastructure provided by package tm. 
#     Each tweet is saved as a document
myCorpus <- Corpus(VectorSource(df$text));
myCorpus

#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Let's do some additional cleaning
#     Defining toSpace function
#     Removing special characters
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
#Ignore the "transformation drops documents" warning messages.
myCorpus <- tm_map(myCorpus, toSpace, "@")
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "]")
myCorpus <- tm_map(myCorpus, toSpace, "$")
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Removing numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Removing punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Removing non-ascii characters
myCorpus <- tm_map(myCorpus, function(x) iconv(x, "latin1", "ASCII", sub=""))
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Removing English stop words
stopwords("english")
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#     Removing our own stop words
myCorpus <- tm_map(myCorpus, removeWords,c("By", "the", "may", "ttp", "qhb...", "https", "http...", "for", "tco"))
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#Removing whitespace characters
#When rendered, a whitespace character does not correspond to a visible
#mark, but typically does occupy an area on a page
myCorpus <- tm_map(myCorpus, stripWhitespace)
#     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])

#Stemming (i.e., removing common word endings like "es", "ed", "ing" etc.)
#We might not want to do that -- Now, instead of having "Winteriscoming", 
#we have "winteriscom"
 #     Let's look at the first entry in the corpus
strwrap(myCorpus[[1]])
myCorpus



#Creating a Document Term Matrix (DTM)
#DTM is a mathematical matrix that describes the frequency of terms that occur in a collection of
#documents. In a DTM, rows correspond to documents (tweets) in thecollection and columns
#correspond to terms. Source: https://en.wikipedia.org/wiki/Document-term_matrix

#For example, see: http://d3j5m2z8950src.cloudfront.net/sites/default/files/TM16.jpg
dtm <- DocumentTermMatrix(myCorpus)
dtm
dim(dtm)


freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
freq

#install.packages("qdap")
#install.packages("qdapDictionaries")
#install.packages("dplyr")
#install.packages("RColorBrewer")
#install.packages(RColorBrewer)

install.packages("wordcloud")
library(wordcloud)

wordcloud(names(freq), freq, min.freq=20, colors=brewer.pal(6, "Dark2"))






#myCorpus <- Corpus(VectorSource(df$text));
#myCorpus





#    Remove the columns replyToSID, replyToUID and replyToSN from data frame df
df$replyToSID <- NULL
df$replyToUID <- NULL
df$replyToSN <- NULL
head(df)
dim(df)

sum(is.na(df$longitude))

#    Let's look at the number of observations which have missing variable values
#    There are a few hundred observations left which have NON-missing values for 
#    latitude and longitude.
df.geocoded=na.omit(df)
dim(df.geocoded)


#mean(df.geocoded$latitude)
#mean(df.geocoded$longitude)

install.packages("ggmap")
install.packages("ggplot2")
install.packages("rworldmap")
install.packages("mapproj")
install.packages("maps")
install.packages("sp")
install.packages("sf")
library(ggmap)
library(ggplot2)
library(rworldmap)
library(mapproj)
library(maps)
library(sp)
library(sf)

write.csv(df.geocoded, file = "df_geocoded.csv")
geocoded.points <- read.csv("df_geocoded.csv")


#Now let's plot the tweets on a world map!
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  geom_point(data=geocoded.points, aes(x=longitude, y=latitude), inherit.aes=FALSE)


#You can color the points differently, or compute density, etc.
#REMEMBER that this is not necessarily a representative sample of tweets.




