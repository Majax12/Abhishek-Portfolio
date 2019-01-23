library(NLP)
library(tm)
library(plyr)
library(RSiteCatalyst)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringr)
install.packages("slam", repos = "https://cran.cnr.berkeley.edu/")
library(openNLP)
library(qdap)

library(RWeka)
library(rJava)

library(ngram)
library(qdap)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(Rgraphviz)
library(slam)

setwd("D:\\Abhishek\\TAPE2")
getwd()

#####*Getting the file*
df <- read.csv("Dataf.csv", header = TRUE, sep = ",")

#####*creating dataframe where Workgroup=='UHC M AND R CONSUMER PORTALS'

data_for_tfidf <- subset(df,df$Category =="INCIDENT")
data_for_tfidf <- subset(data_for_tfidf,data_for_tfidf$YEAR>= 2015)
#rm(data)
#####Getting only Brief.Description column for newdata
summary_one_workgroup <- data.frame(data_for_tfidf$Summary)


  
####converting to character variable
summary_one_workgroup$newdata.Brief.Description <- as.character(data_for_tfidf$Summary)
class(summary_one_workgroup$newdata.Brief.Description)
data_text <- Corpus(VectorSource(summary_one_workgroup$newdata.Brief.Description))
data_text <- tm_map(data_text,content_transformer(removeNumbers))
data_text <- tm_map(data_text,stripWhitespace)
data_text <- tm_map(data_text,content_transformer(tolower))
data_text <- tm_map(data_text,removePunctuation)
data_text <- tm_map(data_text, removeWords, stopwords("english"))

#stopwords creation
#stpwrds <- c(stopwords("english"))
#data_text <- tm_map(data_text, removeWords, stpwrds)

#rm(data)


#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

#trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

#tetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

#pentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

#mention whether it is for unigram, bigram, trigram, tetragram and pentagram

#tdm <- TermDocumentMatrix(data_text,control = list(tokenize = BigramTokenizer))
tdm <- TermDocumentMatrix(data_text)
#dtm <- DocumentTermMatrix(data_text)

new1 <- removeSparseTerms(Defects_dtm, 0.99) #removing sparcity
Defects_dtm <- DocumentTermMatrix(data_text,control=list(weighting=weightTfIdf)) 
new2 <- as.matrix(new1)
new2 <- data.frame(new2)

write.csv(new2,"Test.csv")
#new21 <- as.numeric(new2)
term.freq <- rowSums(as.matrix(new1))
term.freq <- subset(term.freq, term.freq >= 100)
df <- data.frame(term = names(term.freq), freq = term.freq)
sort.df = df[order(-term.freq),]#sorted in descending order

#write.table(sort.df, file = "D:/Abhik/UHG/R/Data/unigram.csv", sep = ",", col.names = NA,qmethod = "double") #update file name

####Wordcloud

m <- as.matrix(term.freq)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)

# colors

pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20,
          random.order = F, colors = pal)

