#
library(tm)
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
    words <- repUnk(x = words, vocab = vocab)
    ncWords <- nCode(words)
    num_words <- length(ncWords)
    if (num_words == 0)
       ncWords <- nCode('<UNK>')   
    
    # Try 3gram on previous two words
    r3 <- NULL
    if (num_words >= 2) {
        n = 3;
        res <- tryNgram(n = n, words = ncWords[(num_words - 1):num_words])
        r3 <- c(n, res$W3[1:num_results], res$Count[1:num_results])
    }
    
    # Try 2gram on previous word
    r2 <- NULL
    if (num_words >= 1){
        n = 2;
        res <- tryNgram(n = n, words =  ncWords[length(ncWords)])
        r2 <- c(n, res$W2[1:num_results], res$Count[1:num_results])        
    }

    # Try skip digrams on previous 3 to 1 words
    n = 2;
    r21 = r22 = r23 <- 0

    if (num_words > 1) {
        res <- tryNgram(n = n, words =  ncWords[length(ncWords)-1], df=n2s_df)
        r21 <- c(paste0(n,'s1'), res$W2[1:skip_num_results], res$Count[1:skip_num_results])        
    }
    if (num_words > 2) {
        res <- tryNgram(n = n, words =  ncWords[length(ncWords)-2], df=n2s_df)
        r22 <- c(paste0(n,'s2'), res$W2[1:skip_num_results], res$Count[1:skip_num_results])        
    }
    if (num_words > 3) {
        res <- tryNgram(n = n, words =  ncWords[length(ncWords)-3], df=n2s_df)
        r23 <- c(paste0(n,'s3'), res$W2[1:skip_num_results], res$Count[1:skip_num_results])        
    }
    
    # remove stopwords
    
    # Try skip digrams on previous 3 to 1 words
    
    # Report Results
    
    # Testing output below
    return(list(words, ncWords, r2, r3, r21, r22, r23))
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
    sents = guessed = n2g = n3g = n2s1 = n2s2 = n2s3 = c()        # storing vectors  
    
    # Create new data frame if not existing or not specified
    if ((missing(tdf) | !((deparse(substitute(tdf)) %in% ls(envir = .GlobalEnv))))){
        tdf <- setTrain.DF()
        cat('>>> Training Data Set created. \n')
    }
        
    for (i in 1:length(x)){
        words <- ngram(x[i], 1)                                   # get words
        if (length(words) < 2)                                    # not small ones
            next
        toGuess <- sample(2:(length(words) - 1), 1)               # choose a random word
        wordToGuess <- words[toGuess]                             # Word to predict
        strt <- (toGuess - 1) - MAX_NW + 1                        # save sentence from here 
        if (strt < 1)                                            
            strt <- 1                                             # if sentence too short
        sent <- paste(words[strt:(toGuess - 1)], collapse = ' ')  # save sentence
        res <- guessWord(sent)                                    # pass sentence to predictor

        # save results
        sents <- append(sents, sent)
        guessed <- append(guessed,wordToGuess)
        n2g <- append(n2g, paste(res[[3]][2:length(res[[3]])], collapse='-'))
        n3g <- append(n3g, paste(res[[4]][2:length(res[[4]])], collapse='-'))
        n2s1 <- append(n2s1, paste(res[[5]][2:length(res[[5]])], collapse='-'))
        n2s2 <- append(n2s2, paste(res[[6]][2:length(res[[6]])], collapse='-'))
        n2s3 <- append(n2s3, paste(res[[7]][2:length(res[[7]])], collapse='-'))
    }
    
    # update data frame and go back
    return(rbind(tdf, data.frame(sents, guessed, n2g, n3g, n2s1, n2s2, n2s3, 
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

