packages <- c("tm", "quanteda", "installr", "stylo", "NLP", "openNLP", "qdap", "RWeka")

## install.packages(packages)

library(installr)
library(tm)
library(quanteda)
library(NLP)
library(stylo)
library(qdap)
library(openNLP)
library(RWeka)
library(ggplot2)

## setwd("C:\\Users\\Anusha\\Desktop\\Coursera_Assgn\\capstone project\\en_US")

## establish file connections and read news, blog and twitter data
filenamesUS <- list.files(pattern = "/*.txt", full.names = TRUE)

conBlog <- file(filenamesUS[1], "rb")
conNews <- file(filenamesUS[2], "rb")
conTwit <- file(filenamesUS[3], "rb")

usBlog <- readLines(conBlog,encoding="latin1", n=1000)
usNews <- readLines(conNews,encoding="latin1", n=1000) 
usTwit <- readLines(conTwit,encoding="latin1", n=1000) 
usProf <- readLines(filenamesUS[4],encoding="latin1") 

close(conBlog)
close(conNews)
close(conTwit)

## Basic summaries 
lBlog <- length(usBlog)
lNews <- length(usNews)
lTwit <- length(usTwit)

hWordCntBlog <- max(nchar(usBlog), na.rm = TRUE)
lWordCntBlog <- min(nchar(usBlog), na.rm = TRUE)
hWordCntNews <- max(nchar(usNews), na.rm = TRUE)
lWordCntNews <- min(nchar(usNews), na.rm = TRUE)
hWordCntTwit <- max(nchar(usTwit), na.rm = TRUE)
lWordCntTwit <- min(nchar(usTwit), na.rm = TRUE)

usBlog <- unlist(strsplit(usBlog, split=", "))
usNews <- unlist(strsplit(usNews, split=", "))
usTwit <- unlist(strsplit(usTwit, split=", "))

## combine sample from each data set
allText <- paste(usBlog, usNews, usTwit)
allText2 <- grep("allText", iconv(allText, "latin1", "ASCII", sub="allText"))
allText <- allText[-allText2]
allText <- paste(allText, collapse = ", ")
byLineText <- sent_detect(allText, language = "en", model = NULL)

## Creat corpus for the combined text and clean data

sourceText <- VectorSource(byLineText)
corpusText <- VCorpus(sourceText)
corpusText <- tm_map(corpusText, removeNumbers)
corpusText <- tm_map(corpusText, stripWhitespace)
corpusText <- tm_map(corpusText, content_transformer(tolower))
corpusText <- tm_map(corpusText, removePunctuation)
corpusText <- tm_map(corpusText, PlainTextDocument)
getRidOff <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpusText <- tm_map(corpusText, getRidOff, "@[[:alnum:]]*")
corpusText <- tm_map(corpusText, getRidOff, "http[[:alnum:]]*")
corpusText <- tm_map(corpusText, getRidOff, "http[[:alnum:]]*")
corpusText <- tm_map(corpusText, getRidOff, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpusText <- tm_map(corpusText, getRidOff, "")
corpusText <- tm_map(corpusText, removeWords, usProf)
corpusText <- tm_map(corpusText, removeNumbers)

## Tokenize cleaned data, create TDM to form n-grams and extract top 20 word-frequency combinations 
## 1-gram words
oneGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
oneTdm <- TermDocumentMatrix(corpusText, control = list(tokenize = oneGramTokenizer))
oneFreq <- rowSums(as.matrix(oneTdm))
oneFreq <- sort(oneFreq, decreasing = TRUE)
oneDf <- data.frame("Words"=names(oneFreq), "Freq"=oneFreq)
oneForGraph <- oneDf[1:20,]

## 2-gram words
biGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
twoTdm <- TermDocumentMatrix(corpusText, control = list(tokenize = biGramTokenizer))
twoFreq <- rowSums(as.matrix(twoTdm))
twoFreq <- sort(twoFreq, decreasing = TRUE)
twoDf <- data.frame("Words"=names(twoFreq), "Freq"=twoFreq)
twoForGraph <- twoDf[1:20,]

## 3-gram words
triGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
threeTdm <- TermDocumentMatrix(corpusText, control = list(tokenize = triGramTokenizer))
threeFreq <- rowSums(as.matrix(threeTdm))
threeFreq <- sort(threeFreq, decreasing = TRUE)
threeDf <- data.frame("Words"=names(threeFreq), "Freq"= threeFreq)
threeForGraph <- threeDf[1:20,]

## 4-gram words
quadGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min=4,max=4))
fourTdm <- TermDocumentMatrix(corpusText, control = list(tokenize = quadGramTokenizer))
fourFreq <- rowSums(as.matrix(fourTdm))
fourFreq <- sort(fourFreq, decreasing = TRUE)
fourDf <- data.frame("Words"=names(fourFreq), "Freq"= fourFreq)
fourForGraph <- fourDf[1:20,]

## Write n-grams created to csv to use for the predcition model
write.csv(oneDf, paste0(getwd(),"./oneGram.csv"), row.names =FALSE)
write.csv(twoDf, paste0(getwd(),"./biGram.csv"), row.names =FALSE)
write.csv(threeDf, paste0(getwd(),"./triGram.csv"), row.names =FALSE)
write.csv(fourDf, paste0(getwd(),"./quadGram.csv"), row.names =FALSE)

## Plot top 20 word-frequncy for all n-grams 
ggplot(oneForGraph, aes(x=Words,y=Freq)) + 
    geom_bar(stat="Identity", fill="Red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(twoForGraph, aes(x=Words,y=Freq)) + 
    geom_bar(stat="Identity", fill="Blue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(threeForGraph, aes(x=Words,y=Freq)) + 
    geom_bar(stat="Identity", fill="Green") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(fourForGraph, aes(x=Words,y=Freq)) + 
    geom_bar(stat="Identity", fill="Pink") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Tokenize combined text and plot word frequencies

textToken1 <- tokenize(toLower(allText), removePunct = TRUE,
                       removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 1)
textToken2 <- tokenize(toLower(allText), removePunct = TRUE,
                       removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 2)
textToken3 <- tokenize(toLower(allText), removePunct = TRUE,
                       removeNumbers = TRUE, removeTwitter = TRUE, ngrams = 3)

dfmToken1 <- dfm(textToken1)
dim(dfmToken1)
plot(dfmToken1, min.freq = 1000, random.order = FALSE)

dfmToken2 <- dfm(textToken2)
dim(dfmToken2)
plot(dfmToken2, min.freq = 500, random.order = FALSE)

dfmToken3 <- dfm(textToken3)
dim(dfmToken3)
plot(dfmToken3, min.freq = 100, random.order = FALSE)
