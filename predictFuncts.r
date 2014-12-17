#
# Functions used in training and prediction
#
library(tm)

#
# getNgram - Retrieves most frequent n-grams from n-1 words
#
getNgram <- function(words, top=10, skip=FALSE){
    #
    # Takes a vector of words and returns the most frequent ngrams
    #
                                                          # identify proper dataframe
    n <- length(words) + 1                                # ngram order
    dftype <- 'g'                                         # ngram data frame
    if (skip)                                             # or skip data frame
        dftype <- 's'
    
    df <- paste0('n', n, dftype, 'n_df')                  # build df name
    df <- get(df)                                         # and get object

    searchKey <- nCodeNgram(words)                        # create secondary key
    res <- df[df$Skey == searchKey,]                      # go get it
    top <- min(top, nrow(res))                            # limit results to those available
    
    return(res[order(res$Count, decreasing = T)[1:top],]) # return ordered
}

#
# guessWord - Evaluates next word in sentence
#
guessWord <- function(sentence) {
    #
    # Default settings
    cntrl <- list(convertTolower=c(TRUE),
                  convertToASCII=TRUE,
                  removePunct=TRUE,
                  removeNumbers=TRUE,
                  removeStopWords=c(FALSE, NULL))
    vocab = WRDS;
    num_results = 3;
    skip_num_results = 2;
    
    # End of Settings 
    
    # Sentence cleansing
    words <- cleanSent(x = sentence, control = cntrl)
    
    # Replace unknown terms and encode
    # words <- repUnk(x = words, vocab = vocab)
    words <- ngram2(words, 1, encode=F)         # get a vector of words
    n <- length(words)
    
    m <- 2
    allres <- data.frame()
    for (i in 1:m) {
        if (n <=  i-1)
            break
        res <- getNgram(words[(n-i+1):n])
        res <- cbind(N = i, W = res[,4+i], C = res[,2])
        allres <- rbind(allres, res)
    }    
        
    # remove stopwords
    
    cntrl <- list(convertTolower=c(TRUE),
                  convertToASCII=TRUE,
                  removePunct=TRUE,
                  removeNumbers=TRUE,
                  removeStopWords=c(TRUE, 'myStopWords'))
    
    # Sentence cleansing
    words <- cleanSent(x = sentence, control = cntrl)

    m = 3
    for (i in 1:m){
        if (n <=  i-1)
            break
        res <- getNgram(words[n-i+1], skip=T)
        res <- cbind(N = paste0('2s',i), W = res[,5], C = res[,2])
        allres <- rbind(allres, res)        
    }
    
    # Report Results
    return(allres)
}

#
# trainer - Chooses a word at random from a sentence and
#           calls the predict function. Saves results in 
#           training data frame.
#
trainer <- function(x, tdf) {
    #
    # Takes a character vector and applies the predict function to all entries.
    # Creates or update training data frame.
    #
    
    MAX_NW = 10;    # max number of words to save in training data set
    sents = guessed = n1g = n2g = n3g = n2s1 = n2s2 = n2s3 = c()    # storing vectors  
    
    # Create new data frame if not existing or not specified
    if ((missing(tdf) | !((deparse(substitute(tdf)) %in% ls(envir = .GlobalEnv))))){
        tdf <- setTrain.DF()
        cat('>>> Training Data Set created. \n')
    }
        
    for (i in 1:length(x)){
        
        words <- ngram2(x[i], 1)                                  # get words
        
        if (length(words) < 2)                                    # not shorties
            next
        
        toGuess <- sample(2:(length(words) - 1), 1)               # choose a random word
        wordToGuess <- words[toGuess]                             # Word to predict
        strt <- (toGuess - 1) - MAX_NW + 1                        # save sentence from here 
        
        if (strt < 1)                                            
            strt <- 1                                             # if sentence too short
        
        sent <- paste(words[strt:(toGuess - 1)], collapse = ' ')  # save sentence
        res <- guessWord(sent)                                    # pass sentence to predictor

        # save results
        res <- cbind(Sent = sent, Word = toGuess) ...<------- up to here...
        allres <- rbind(allres, res)
  }
    
    # update data frame and go back
    return(rbind(tdf, data.frame(sents, guessed, n1g, n2g, n3g, n2s1, n2s2, n2s3, 
                                 stringsAsFactors=F))) 
}
#
setTrain.DF <- function(Sent='', Word='',  
                        n2g='', n3g='', n2s1='', n2s2='', n2s3=''){
    #
    # Creates training data frames
    #
    df <- data.frame(Sent = Sent, 
                     Word = Word,
                     n2g = n2g,
                     n3g = n3g,
                     n2s1 = n2s1,
                     n2s2 = n2s2,
                     n2s3 = n2s3,
                     row.names = NULL,
                     stringsAsFactors = FALSE)
    if (Sent == '')
        return(df[-1,])
    return(df)
}
#
# getResults - Display results for a particular ngram clearly
#              (not yet working)
#
getResults <- function(tdf, ngramType) {
    # This function does not work (yet)
    ngres <- paste0(deparse(substitute(tdf)), '$', ngramType)
    words <- unlist(strsplit(get(ngres), '-'))[1:3]
    Values <- unlist(strsplit(get(ngres), '-'))[4:6]
    return(words)
}

