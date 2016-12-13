#####################################################
#  dalla seconda settimana
load("vecchiaworkspace.Rdata")
old <- df_now
#####################################################
# scarica tweet referendum
library(twitteR)
setup_twitter_oauth(consumer_key = "yourCK", consumer_secret = "yourCS",
                    access_token="yourAT",
                    access_secret="yourAS")
searchQuery <- c("#iovotosi", "#iovotono")
#####################################################
# esporta e immagazina tutto in una lista
multidf<-list()
for (i in 1:length(searchQuery)){
  tweets<-searchTwitteR(searchQuery[i], n=30000, lang = "it", since = "2016-11-23", until =  "2016-12-03")
  df <- do.call("rbind", lapply(tweets, as.data.frame))
  df$searchQuery<-searchQuery[i]
  multidf[[i]]<-df
}
#####################################################
# merge dei data.frame nella lista
mymergedata<-do.call("rbind", multidf)
# rimuovere eventuali duplicat
df_now <- mymergedata[!duplicated(mymergedata[,-17]),]
#####################################################
# merge data.frame vecchi e nuovi tweet
#####################################################
tweets <- rbind(old, df_now)
df_now <- tweets[!duplicated(tweets[,-17]),]
#####################################################
# barplot per searchquery
#####################################################
t <- as.data.frame(table(df_now$searchQuery), stringsAsFactors = F)
barplot(t$Freq, names.arg = t$Var1, main = "chi tweetta maggiormente", ylab = "Tweets", col = c("red", "blue"))
#####################################################
# lineplot per giorno
#####################################################
dayly <- as.data.frame(table(as.Date(df_now$created, format = "%yyyy-%mm-%dd"),
                             df_now$searchQuery))
names(dayly) <- c("date", "query", "freq")
dayly$date <- as.Date(dayly$date)
library(ggplot2)
ggplot(data = dayly, aes(x = date, y = freq, colour = query)) +
  geom_line(size = 1) +
  theme_bw()
#####################################################
# piechart
#####################################################
pie(t$Freq, labels = c("#iovotono", "#iovotosi"), radius = 1, col = c("red", "blue"), clockwise = T)
#####################################################
# piechart per user singolo
#####################################################
tU <- df_now[!duplicated(df_now$screenName),]
tU <- as.data.frame(table(tU$searchQuery), stringsAsFactors = F)
pie(tU$Freq, labels = c("#iovotono", "#iovotosi"), radius = 1, col = c("red", "blue"), clockwise = T)
#####################################################
#####################################################
library(stringr)
######################################
# estrazione hashtag
hashtag.regex <- perl("(?<=^|\\s)#\\S+")
hashtag<-str_extract_all(df_now$text, hashtag.regex)
hashtag[hashtag=="character(0)"] <-"NA"
hashtag2 <- unlist(hashtag)
# preprocessing degli hashtag
library(tm)
# remove URLs
hashtag3 <- gsub('http\\S+\\s*', '', hashtag2)
# remove emoji
hashtag3 <- gsub("(\\\\[[:alnum:]_]*)", '', hashtag3)
hashtag3 <- str_replace_all(hashtag3,"[^[:graph:]]", " ")
# rimozione punteggiatura
hashtag3 <- removePunctuation(hashtag3)
# rimozione numeri
hashtag3 <- removePunctuation(hashtag3)
# tutto minuscolo
hashtag3 <- tolower(hashtag3)
#remove stopword
# stopword in italiano più aggiunte al dizionario
stopW <- c(stopwords("italian"), "rt", "po", "no", "qui", "me", "quando", "invece", "così",
           "già", "cè", "cazzo")
hashtag3 <- removeWords(x = hashtag3, words = stopW)
# rimozione spazi superflui inizio e fine stringa
hashtag3 <- trimws(hashtag3, which = "both")
# rimozione spazi doppi
hashtag3 <- stripWhitespace(hashtag3)
# costruzione della wordcloud
word <- table(hashtag3)
library(wordcloud)
par(bg = "black")
wordcloud(words = names(word), freq = word, min.freq = 55, max.words = 50, random.order = F,
          colors = heat.colors(10), scale = c(5,.8))