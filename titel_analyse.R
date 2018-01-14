library(tm)
library(ggplot2)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

artikel_list3 <-  read.csv2("bdw_artikel.csv")
#tm::DataframeSource need textid and text as column names
df_title <- data.frame(doc_id=row.names(artikel_list3),
                       text=artikel_list3$Titel)
mycorpus <- Corpus(DataframeSource(df_title))

mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, removeWords, c(stopwords("german")))


tdm <- DocumentTermMatrix(mycorpus)
print(tdm)
m <- as.matrix(tdm)
v <- sort(colSums(m),decreasing=TRUE)

words <- names(v)
d <- data.frame(word=words, freq=v)
display.brewer.all()
pal2 <- brewer.pal(3,"Dark2")
wordcloud(d$word,d$freq, scale=c(3,.2), min.freq=45, max.words=150, random.color=TRUE,colors = pal2)
wordcloud(d$word,d$freq, scale=c(8,.2), max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
