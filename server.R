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
    predWord2 <- as.character(NULL);
    predWord3 <- as.character(NULL);
    
    ## if input length greater or equal to 3, 
    ## extract last 3 words of input phrase and 
    ##check against first three words of 4-gram table created for word with highes frequency
    if(ipLength >= 3 & !predFlag){ 
        
        useIp <- paste0(userIp[(ipLength-2):ipLength], collapse=" ")
        searchIp <- paste("^",useIp, sep = "")
        quadGram <- quadDf[grep(searchIp, quadDf$Words), ]
        
        if(length(quadGram[ , 1]) >= 1){
            predWord <- as.character(quadGram[1,1])
            predWord2 <- as.character(quadGram[2,1])
            predWord3 <- as.character(quadGram[3,1])
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
            predWord2 <- as.character(triGram[2,1])
            predWord3 <- as.character(triGram[3,1])
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
            predWord2 <- as.character(biGram[2,1])
            predWord3 <- as.character(biGram[3,1])
            predFlag <- TRUE
            modelUsed <- "2-gram used to predict next word!"
        }
        biGram <- NULL
    } else if(ipLength > 0 & !predFlag){
        ## if input length greater 0, 
        ## extract word with highest frequency
        predWord <- as.character(uniDf$Words[1])
        predWord2 <- as.character(uniDf$Words[2])
        predWord3 <- as.character(uniDf$Words[3])
        predFlag <- TRUE
        modelUsed <- "Word with Highest Probability of Occurance!"
    } else{
        ## in all other cases, 
        ## return null with explanation
        predWord <- ""
        predWord2 <- ""
        predWord3 <- ""
        predFlag <- FALSE
        modelUsed <- "I do not have enough information to predict the next word :("
    }
    
    ## return output for next predicted word
    
    if(length(predWord) == 0){
        predWord <- ""
        predWord2 <- ""
        predWord3 <- ""
        modelUsed <- "I do not have enough information to predict the next word :("
        outDf <- data.frame(predWord, predWord2, predWord3, modelUsed)
        return(outDf)
    } else{
        if(nchar(predWord) > 1){
            predWord <- tail(strsplit(predWord, split=" ")[[1]], 1)
        }
        if(length(predWord2) == 0){
            predWord2 <- ""
        } else if(nchar(predWord2) > 1){
            predWord2 <- tail(strsplit(predWord2, split=" ")[[1]], 1)
        }
        if(length(predWord3) == 0){
            predWord3 <- ""
        } else if(nchar(predWord3) > 1){
            predWord3 <- tail(strsplit(predWord3, split=" ")[[1]], 1)
        }
        
        if (ipLength > 0){
            outDf <- data.frame(predWord, predWord2, predWord3, modelUsed)
            return(outDf)
        } else {
            predWord <- ""
            predWord2 <- ""
            predWord3 <- ""
            modelUsed <- "I do not have enough information to predict the next word!"
            outDf <- data.frame(predWord, predWord2, predWord3, modelUsed)
            return(outDf)
        }
    }
}

nextWordPredict <- ""
shinyServer(
    function(input, output) {
        output$userIp <- renderPrint({input$userIp})
        output$pWordMethod <- renderPrint({userInp <- cleanUserInput(input$userIp);
                                           getOut <- predictNextWord(userInp);
                                           input$action;
                                           nextWordPredict <- as.character(getOut[1,2]);                                  
                                           cat(as.character(getOut[1,1]));
                                           cat("\n");
                                           cat("Other top option(s) for next word - ", as.character(getOut[1,2]));
                                           cat("\n");
                                           cat("Other top option(s) for next word - ", as.character(getOut[1,3]));
                                           cat("\n");
                                           cat("Model- ", as.character(getOut[1,4]))})
    }
)

