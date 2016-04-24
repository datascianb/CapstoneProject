library(shiny)
library(R.utils)
library(installr)
library(tm)
library(quanteda)
library(NLP)
library(stylo)
library(qdap)
library(openNLP)
library(RWeka)
library(ggplot2)

## read n-gram data frames
uniDf <- read.csv("oneGram.csv") 
biDf <- read.csv("biGram.csv")
triDf <- read.csv("triGram.csv")
quadDf <- read.csv("quadGram.csv")
usProf <- readLines("./profanity.txt",encoding="latin1")

## function to clean required input phrase/ word, return cleaned input 
## cleaning is done in the same way as data sets for blog, news and twitter
## returns cleaned input
cleanUserInput <-function(phrase) {
    phrase <- tolower(phrase) 
    phCorpus <- VCorpus(VectorSource(phrase))
    phCorpus <- tm_map(phCorpus, removeNumbers)
    phCorpus <- tm_map(phCorpus, stripWhitespace)
    phCorpus <- tm_map(phCorpus, removePunctuation)
    phCorpus <- tm_map(phCorpus, removeWords, usProf)
    phrase <- as.character(phCorpus[[1]])
    phrase <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", phrase)
    if(nchar(phrase) > 0){
        return(phrase)
    } else{
        return("")
    }
}

## function to predict next word for a required input  
## prediction model based on katz back-off modelGuy at
predictNextWord <- function(userIp){
    ## call clean input
    userIp <- cleanUserInput(userIp)
    
    userIp <- unlist(strsplit(userIp, split=" "))
    
    ipLength <- length(userIp)
    
    predFlag <- FALSE
    predWord <- as.character(NULL);
    
    ## if input length greater or equal to 3, 
    ## extract last 3 words of input phrase and 
    ##check against first three words of 4-gram table created for word with highes frequency
    if(ipLength >= 3 & !predFlag){ 
        
        useIp <- paste0(userIp[(ipLength-2):ipLength], collapse=" ")
        searchIp <- paste("^",useIp, sep = "")
        quadGram <- quadDf[grep(searchIp, quadDf$Words), ]
        
        if(length(quadGram[ , 1]) >= 1){
            predWord <- as.character(quadGram[1,1])
            predFlag <- TRUE
            modelUsed <- "4-gram used to predict next word!"
        }
        quadGram <- NULL;
    } else if(ipLength >= 2 & !predFlag){
        ## if input length greater or equal to 2, 
        ## extract last 2 words of input phrase and 
        ##check against first two words of 3-gram table created for word with highes frequency
        useIp <- paste0(userIp[(ipLength-1):ipLength], collapse=" ")
        searchIp <- paste("^",useIp, sep = "")
        triGram <- triDf[grep(searchIp, triDf$Words), ]
        
        if (length(triGram[, 1]) >= 1){
            predWord <- as.character(triGram[1,1])
            predFlag <- TRUE
            modelUsed <- "3-gram used to predict next word!"
        }
        triGram <- NULL;
    } else if(ipLength >= 1 & !predFlag){
        ## if input length greater or equal to 1, 
        ## extract last word of input phrase and 
        ##check against first word of 2-gram table created for word with highes frequency
        useIp <- userIp[ipLength]
        searchIp <- paste0("^",useIp, sep = "")
        biGram <- biDf[grep(searchIp, biDf$Words), ]
        
        if (length(biGram[, 1]) >= 1){
            predWord <- as.character(biGram[1,1])
            predFlag <- TRUE
            modelUsed <- "2-gram used to predict next word!"
        }
        biGram <- NULL
    } else if(ipLength > 0 & !predFlag){
        ## if input length greater 0, 
        ## extract word with highest frequency
        predWord <- as.character(uniDf$Words[1])
        predFlag <- TRUE
        modelUsed <- "Word with Highest Probability of Occurance!"
    } else{
        ## in all other cases, 
        ## return null with explanation
        predWord <- ""
        predFlag <- FALSE
        modelUsed <- "I do not have enough information to predict the next word :("
    }
    
    ## return output for next predicted word
    
    if(length(predWord) == 0){
        predWord <- ""
        modelUsed <- "I do not have enough information to predict the next word :("
        outDf <- data.frame(predWord, modelUsed)
        return(outDf)
    } else{
        if(nchar(predWord) > 1){
            predWord <- tail(strsplit(predWord, split=" ")[[1]], 1)
        }
        
        if (ipLength > 0){
            outDf <- data.frame(predWord, modelUsed)
            return(outDf)
        } else {
            predWord <- ""
            modelUsed <- "I do not have enough information to predict the next word!"
            outDf <- data.frame(predWord, modelUsed)
            return(outDf)
        }
    }
}

## test examples

phrase1 <- "I ate"
predictNextWord(phrase1)

phrase2 <- "how many times"
predictNextWord(phrase2)

phrase3 <- "in the year"
predictNextWord(phrase3)

phrase4 <- "15th"
predictNextWord(phrase4)

phrase5 <- "thereafter he wasn't home"
predictNextWord(phrase5)

phrase6 <- "grace period"
predictNextWord(phrase6)

