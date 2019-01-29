library(NLP)
library(tm)
library(plyr)
library(RSiteCatalyst)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(stringr)
install.packages("tm", repos = "https://cran.cnr.berkeley.edu/")
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

#----------------------------------------------------------------------------------------
setwd("E:\\Scripts\\Data\\WordCloud")
getwd()


#####Getting the file

df <- read.csv("Tweets.csv", header = T, sep = ",")

#----------------------------------------------------------------------------------------
#####*Extract Specific Datapoints to get the world cloud

data_for_tfidf <- df$Tweet

#####Convert to a dataframe

tfidf_dataframe <- data.frame(data_for_tfidf)



####converting to character variable / List

tfidf_dataframe$Tweet <- lapply(data_for_tfidf, as.character)

class(tfidf_dataframe$Tweet)


data_text <- Corpus(VectorSource(tfidf_dataframe$Tweet))
data_text <- tm_map(data_text, content_transformer(removeNumbers))
data_text <- tm_map(data_text, stripWhitespace)
data_text <- tm_map(data_text, content_transformer(tolower))
data_text <- tm_map(data_text, removePunctuation)
data_text <- tm_map(data_text, removeWords, stopwords("english"))

#----------------------------------------------------------------------------------------

#Create and Remove stopwords creation and Choose tokenizer [Optional]


#stpwrds <- c(stopwords("english"))
#data_text <- tm_map(data_text, removeWords, stpwrds)

#----------------------------------------------------------------------------------------
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

#trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

#tetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

#pentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

#mention whether it is for unigram, bigram, trigram, tetragram and pentagram


#----------------------------------------------------------------------------------------

#tdm <- TermDocumentMatrix(data_text,control = list(tokenize = BigramTokenizer))

tdm <- TermDocumentMatrix(data_text)
new1 <- removeSparseTerms(tdm, 0.99) #removing sparcity

term.freq <- rowSums(as.matrix(new1))
term.freq <- subset(term.freq, term.freq >= 100)
df3 <- data.frame(term = names(term.freq), freq = term.freq)
sort.df = df3[order(-term.freq), ]#sorted in descending order


#----------------------------------------------------------------------------------------
####Wordcloud

m <- as.matrix(term.freq)
# calculate the frequency of words and sort it by frequency

word.freq <- sort(rowSums(m), decreasing = T)

# Add colors

pal <- brewer.pal(9, "BuGn")[-(1:4)]

#plot word cloud

wordcloud(
  words = names(word.freq),
  freq = word.freq,
  min.freq = 20,
  random.order = F,
  colors = pal
)
