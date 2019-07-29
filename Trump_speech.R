install.packages("tm")
library("tm", lib.loc="C:/R/R-3.5.3/library") # perform text mining
setwd("C:/Users/john doe/Desktop/python/data") # set directory
install.packages("wordcloud") # to create multiple word cloud 
install.packages("wordcloud2") # to create multiple word cloud  
install.packages("RColorBrewer") # to create colorful word cloud 
install.packages("Snowballc") # to perform stemming on text
library("Snowballc")
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
docs=VCorpus(DirSource("C:/Users/john doe/Desktop/python/data")) # to create an instance which is stored on volatile memory
summary(docs) 
inspect(docs[1]) # metadata 7 indicates text document
inspect(docs)
writeLines(as.character(docs[1])) # prints document
docs <- tm_map(docs, removePunctuation) # removes punctuation
for (j in seq(docs)){
  docs[[j]] <- gsub("/", " ", docs[[j]]) # removes rest of punctuation, regular expression in quotes
  docs[[j]] <- gsub("@", " ", docs[[j]]) # removes rest of punctuation
  docs[[j]] <- gsub("\\|", " ", docs[[j]]) # removes pipe symbol form text
  docs[[j]] <- gsub("\u2028", " ", docs[[j]]) # removes ascii character that are either hidden on special char and had to be removed
}
writeLines(as.character(docs[1]))
docs <- tm_map(docs, tolower) # changes text to lower
docs <- tm_map(docs, removeNumbers) # changes numbers to null
docs <- tm_map(docs, removeWords, stopwords("english")) # remove common english stop words
docs <- tm_map(docs, stripWhitespace) # removes whitespaces( tab spaces )
docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[1])) # prints document
dtm <- DocumentTermMatrix(docs) # creates term document matrix
dtm # sparsity shows less correlation b/w words
tdm <- TermDocumentMatrix(docs) # creates document term matrix
tdm
freq <- colSums(as.matrix(dtm)) # calcs freq of word
length(freq)
ord <- order(freq)
m <- as.matrix(dtm)
dim(m) # 11*3694 means 11 docs by 3694 terms
dtms <- removeSparseTerms(dtm, 0.2) # removes sparse terms, makes matrix 20% empty space, max
dtms
freq <- colSums(as.matrix(dtm)) # shows freq. of top 20 words (dmt/dmts?)
head(table(freq), 20)
freq
wf <- data.frame(word = names(freq), freq = freq) # to create data frame for freq 
head(wf)
library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45 , hjust = 1)) # creates 
p
findAssocs(dtm, c("country", "america"), corlimit = 0.85) # specifying correlation limit to 0.85 (dmt/dmts?)
findAssocs(dtms, c("think"), corlimit = 0.70)  # specifying correlation limit to 0.70 (dmt/dmts?)
set.seed(142)
wordcloud(names(freq), freq, min.freq = 25)
wordcloud2(names(freq), minSize = 0, gridSize =  0, fontFamily = 'Pristina', fontWeight = 'bold', color = 'random-dark', backgroundColor = "white", minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65, widgetsize = NULL, figPath = NULL, hoverFunction = NULL)

