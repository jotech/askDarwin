library(tm)
library(ggplot2)   
library(wordcloud)
library(RColorBrewer)

# https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html


docs <- VCorpus(DirSource("~/uni/askDarwin/dat/"))   
summary(docs)

## Preprocessing      
docs <- tm_map(docs,removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, removeWords, c("page"))   
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

### Stage the Data      
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs) 

### Explore data      
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   

#  removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # 
tdms <- removeSparseTerms(tdm, 0.1) #

# Word Frequency   
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)   
findFreqTerms(dtm, lowfreq=50)
wf <- data.frame(word=names(freq), freq=freq)   
head(wf,20)  
ggplot(subset(wf, freq>250), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Relationships Between Terms
findAssocs(dtms, "species", corlimit=0.95)
findAssocs(tdms, c("origin","species"), corlimit=1)

# wordcloud
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   
