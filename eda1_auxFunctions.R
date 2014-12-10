#
# EDA 1 Aux functions
#

#'
#' toascii - Removes any non ascii character from text 
#'
toascii <- function(x, encoding="UTF-8") {
    # removes non ascii characters from char vector
    iconv(x, from=encoding, to="ASCII", sub="")
}
#'
#' map - Applies function to elements of a vector  
#'
map <- function(x, fun, progress=T, ...) {
    # Applies function to element
    cat(">>> Applying function to a text with", length(x), "records. \n")
    y <- rep(0, length(x))
    for (i in 1:length(x)) {
        y[i] <- fun(x[i], ...)
        if (!progress)
            next
        if (i %% 1000 == 0)
            cat(".")
        if (i %% 50000 == 0) {
            segm <- (i %/% 50000) * 50 
            cat(segm, "K", sep="")
        }   
    }
    if (progress)
        cat("EoF \n")
    y
}
#'
#' step - applies function in steps
#'
step <- function(x, fun, steps=1, ...){
    # executes a function in steps
    steps <- as.integer(steps)
    cat(">>> Processing object in", steps, "steps. \n")
    recs <- length(x) %/% steps
    lst_y <- list()
    for (i in 1:steps){
        cat(">>> Step ", i,".\n", sep="")
        init <- 1 + (i-1) * recs
        if (i == steps)
            recs = recs + length(x) %% steps
        y <- fun(x[init:(init + recs - 1)], ...)
        lst_y <- append(lst_y, list(y))
        cat(">>>", recs, "records processed in this step. \n")
    }
    lst_y    
}
#'
#' ngram - Recursive n-gram generator
#'
ngram <- function(x, n=2, split=" ", sep="::", 
                  startMark='<s>', stopMark='</s>'){
    # Takes a vector of strings, size of n-grams to generate, split char and separator
    # Returns a vector of n-grams
    
    # recursive ngram generator
    ngrm <- function(words, n, sep, ngrams){
        if (length(words) < n)                    # no more n-grams, end of story
            return(unlist(ngrams))
        ngrams <- append(ngrams, list(paste(words[1:n], collapse=sep))) # append n-gram
        return(ngrm(words[2:(length(words))], n, sep, ngrams)) # again, without first word 
    }    
    
    # wrapper function
    if(split == " ")                              # if split by whitespaces
        split <- "\\s"                            # use it as split char but    
    x <- gsub(paste0(split,"+"), " ", x)          # make sure there's only one between words
    x <- gsub("^\\s+", "", x)                     # and none as first character
    words <- unlist(strsplit(x,split = split))    # create vector of words
    if (n > 1) {
        words <- append(startMark, words)         # add start... 
        words <- append(words, stopMark)          # and stop markers            
    }
    ngrams <- list()                              # list of ngrams
    if (n < 2)                                    # just return input words
        return(words)                             # or empty vector
    return(ngrm(words, n, sep, ngrams))           # not a trivial case. call generator.    
}
#'
#' ngramN - Recursive n-gram generator for encoded words
#'
ngramN <- function(x, n=2, sep="::", startMark=0, stopMark=0){
    # Takes a vector of words encoded as numbers, size of n-grams to generate, 
    # split char and separator and returns a vector of encoded n-grams
    
    # recursive ngram generator
    ngrm <- function(words, n, sep, ngrams){
        if (length(words) < n)                    # no more n-grams, end of story
            return(ngrams)                        # list replaced by vector
        ngrams <- append(ngrams, paste(words[1:n], collapse=sep)) # append n-gram
        return(ngrm(words[2:(length(words))], n, sep, ngrams)) # again, without first word 
    }    
    
    # wrapper function
    words <- x
    if (n > 1) {
        words <- append(startMark, x)             # add start... 
        words <- append(words, stopMark)          # and stop markers            
    }
    ngrams <- c()                                 # list replaced by vector 
    if (n < 2)                                    # just return input words
        return(words)                             # or empty vector
    return(ngrm(words, n, sep, ngrams))           # not a trivial case. call generator.    
}
#
#'
#' top - Returns the required percent of most used words in a 
#'       term frequency vector. 
#'
top <- function(x, prop){
    x[order(x, decreasing=T)][1:(as.integer((prop * length(x))))]
}
#'
#' cdf - Computes a cumulative distribution function from x
#'
cdf <- function(x){
    x <- x[order(x)]
    y <- rep(0, length(x))
    for (i in 1:length(x)){
        y[i] <- sum(x[1:i])
    }
    return(y/sum(x))
}
#'
#' qxdist - computes quintile where acum probability > p
#'          using x's empirical cumulative distribution
#'
qxdist <- function(x, p, lower.tail=TRUE){
    # returns quantile of a distribution where P <= value
    if (lower.tail) {
        q <- max(which(cdf(x) <= p))
    } else {
        q <- min(which(cdf(x) >= p))
    }
    return(q / length(x))
}
#
# is.english - Check if words are english 
# Note. Requires rJava and wordnet libraries
# 
is.english <- function(words) {
    # Checks if words in wordnet
    # lookup function 
    lookup <- function(word) {
        # looks up a word in wordnet
        pos <- c("ADJECTIVE", "ADVERB", "NOUN", "VERB")
        term <- NULL
        lemma <- NULL
        idx <- 1
        while (is.null(term)){
            filter <- getTermFilter("ExactMatchFilter", word, TRUE)
            term <- getIndexTerms(pos[idx], 25, filter)
            idx <- idx + 1
            if (idx > length(pos))
                break
        }    
        return(term)
    }
    # main
    ans <- rep(FALSE, length(words))
    for (i in 1:length(words)){
        ans[i] <- !is.null(lookup(words[i]))
    }
    ans
}
#
# clock -  Measures a function call elapsed time
#
clock <- function(f, ...){
    # clock measures a function call elapsed time
    # return time and function results in a list
    start_time <- Sys.time()
    result <- f(...)
    elapsed <- Sys.time() - start_time
    return(list(elapsed, result))
}
#
# getCorpusInfo - Prints basic info of a tm corpus
#
getCorpusInfo <- function(crp){
    # Prints basic info of tm corpus
    summary(crp)
    for (i in 1:length(crp)) {
        if (i == 1)
            cat(">>> Corpus contents \n")  
        id <- ID(crp[[i]])
        dts <- as.character(DateTimeStamp(crp[[i]]))
        lang <- Language(crp[[i]])
        cat('>>>', id, '-', dts, '-', lang, '-', length(crp[[i]]), 'lines. \n') 
    }
}
#'
#' dict -  Creates a dictionary of elements and counts
#'
dict <- function(x){
    x <- x[order(x)]
    y <- rep(0, length(unique(x)))
    j <- 1
    names(y)[1] <- x[1]
    for (i in 1:length(x)){
        if (x[i] != names(y)[j]){
            j = j + 1
            names(y)[j] <- x[i]
        }
        y[j] <- y[j] + 1
    }
    y
}
#'
#' dict2 -  Creates a dictionary of elements and counts using 
#'          package hash created by Christopher Brown.
#'
library(hash)
dict2 <- function(x, ht){
    # Takes a character vector of words and optionally a dict (hash table)
    # Returns a new or updated dict of term frequencies.
    
    if(missing(ht)) {
        ht <- hash(unique(x), 0)             # init hash table
    } else {                                 # or identify new entries 
        newones <-!(has.key(unique(x), ht))  
        if (sum(newones) > 0)
            ht[x[newones]] <- 0              # set new entries count to zero
    }
    
    for (i in 1:length(x)){
        ht[x[i]] <- ht[[x[i]]] + 1            # increment counter
    }
    ht
}
# dict.lkup - Retrieves the nth most frequently used terms in a dict
#
dict.lkup <- function(word, dict, n=1){
    # Retrieves the nth most frequent terms in a dict
    # if n = 0, returns the most frequente term
    if (!((n >= 0) & (n <= 1)))
        n <- 1
    key <- paste0("^", word, "::")
    matches <- grep(key, names(dict))
    if (length(matches) == 0)
        return(NULL)
    elems <- dict[matches]
    elems <- elems[order(elems, decreasing=T)]
    if (n == 0)
        return(elems[1])
    return(elems[1:as.integer(length(elems) * n)])
}
#
chop <- function(x, n){
    # divides an object in  n parts without separating elements 
    # of equal value (for vectors only)
    
    # recursive chopping
    chp <- function(x, n, y){
        # chops a vector recursively
        # cat(">>> x:", x, "n:", n, "y:", unlist(y), "\n")
        
        if ((n == 1) | (length(x) <= n))
            return (append(y, list(x)))
        
        cutsize <- as.integer(length(x) / n)
        x1 <- x[1:cutsize]
        x2 <- x[cutsize + 1:(length(x) - cutsize)]
        if (!class(x) == 'list') {        
            x1 <- append(x1, x2[x2==x1[length(x1)]])
            x2 <- x2[x2 != x1[length(x1)]]
            y <- append(y, list(x1))
        } else {
            y <- append(y, list(x1))
        }
            
        if (length(x2) == 0)
            return(y)
        
        return(chp(x2, n-1, y))
    }
    
    # wrapper
    # main
    if (!class(x) == 'list')
        x <- x[order(x)]    
    return(chp(x, n, list()))
}
#
# ramStatus - Gets status of used ram by objects
#
ramStatus <- function(size=0, class='all'){
    # returns a df listing objects' names and sizes in decreasing order 

    inmemory <- ls(envir= .GlobalEnv)
    outlen <- length(inmemory)
    objsizes <- rep(0, outlen)
    objnames <- rep(NA, outlen)
    objclasses <- rep('', outlen)
    
    for (i in 1:length(inmemory)){
        objnames[i] <- inmemory[i]
        objsizes[i] <- round(as.numeric(object.size(get(inmemory[i])) / 1024 ^ 2), 2)
        objclass <- class(get(inmemory[i]))
        for (j in 1:length(objclass)){
            objclasses[i] <- paste0(objclasses[i], paste(objclass[j], ' '))
        }
    }
    
    ordr <- order(objsizes, decreasing=T)
    return(data.frame('Name' = objnames[ordr],
                      'Class' = objclasses[ordr],
                      'MB' = objsizes[ordr],
                      row.names = 1:length(inmemory)))
}
#
# convTime - Return time difference in correct units
#            Necessary when estimating time periods.
# 
convTime <- function(x) {
    # takes and object of class time-difference and returns 
    # elapsed time in correct units
    if (attr(x, "units") == 'secs')
        if (x >= 60) {
            x <- round(x / 60, 2)
            attr(x, "units") <- 'mins'
        }
    if (attr(x, "units") == 'mins')
        if (x >= 60) {
            x <- round(x / 60, 2)
            attr(x, "units") <- 'hours'
        }
    return(round(x, 2))
}
#
# ngram.DF - Creates a data frame from a dict of term frequencies
#
ngram.DF <- function(x, encoded=TRUE) {
  # Creates a data frame from a dict of frequencies
  k <- names(x)
  lk <- strsplit(k, '::')
  w <- matrix(data=NA, nrow=length(x), ncol=length(lk[[1]]))
  for (i in 1:length(lk[[1]])){
    w[,i] <- unlist(lapply(lk, function(x){x[i]}))
  }
  count <- x
  df <- data.frame(Key=k, Count=count, row.names=NULL, stringsAsFactors = FALSE)
  for (i in 1:ncol(w)){
    col <- w[,i]
    if (encoded)
      col <- as.numeric(as.character(w[,i]))
    df <- data.frame(df, col, stringsAsFactors = FALSE)
    colnames(df)[2+i] <- paste0('W',i)
  }
  return(df)
}
# 
# getNgram - gets ngrams from an ngram data frame
# 
getNgram <- function(x, words, decoded=TRUE){
    # gets the correspondign n-grams 
    
    if (length(words) >= 4) {
        y <- with(x, x[W1 == words[1] & W2 == words[2] &
                           W3 == words[3] & W4 == words[4],])   # for 4-grams        
    } else {
        
        if (length(words) >= 3) {
            y <- with(x, x[W1 == words[1] & W2 == words[2] & W3 == words[3],])   # for trigrams        
        } else {
            
            if (length(words) >= 2) {
                y <- with(x, x[W1 == words[1] & W2 == words[2],])          # for bigrams                    
            } else {
                
                if(length(words) >= 1) {
                    y <- with(x, x[W1 == words[1],])                       # for words                    
                    # y <- with(x, x[W1 == words[1],])                     # for words        
                } else {
                    return(NULL)
                }
            }
        }
    }
    if(nrow(y) > 0) {
        if (decoded){
            for(i in 3:ncol(x)){
                y[,i] <- dCode(y[,i])
            }
        }
    }
            
    return(y[order(y$Count, decreasing=T),])
}
# 
repUnk <- function(x, vocab=WRDS, unkMark='<UNK>'){
    # converts a string of words to a character vector with 
    # words not in vocabulary replaced by the UNK mark
    y <- ngram(x, n=1)
    change <- !(y %in% vocab)
    y[change] <- unkMark
    return(y)
}
nCode <- function(x, vocab=WRDS, unkMark='<UNK>'){
    # encodes words in a string of words using a vocab hash table.
    # returns a numeric vector with words in the same order as input.
    # Unknown words are replaced by the UNK mark
    y <- repUnk(x, vocab, unkMark)
    z <- c()
    for (i in 1:length(y)){
        z <- append(z, which(y[i] == vocab))
    }
    return(z)
}
dCode <- function(x, vocab=WRDS){
    # decodes words in a numeric vector using a vocab hash table.
    # Returns a character vector.
    z <- c()
    for (i in 1:length(x)){
        if (x[i] > 0) {
            xd <- vocab[x[i]]
        } else {
            xd <- '<M>'          # Mark up symbol 
        }
        z <- append(z, xd)
    }
    return(z)
}
#
# nCode2 - Encodes / decodes a word vector using hash package's functions.
#          Replaces functions nCode and dCode.
#
nCode2 <- function(x, vocab=WRDS_H, unkMark='<UNK>', 
                   decode=FALSE, vocab_dec=WRDS_H_INV){
    #
    # Encodes or decodes a vector of words. If decode = TRUE, 
    # an inverted hash table must be specified as vocab.
    # Returns a vector of encoded / decoded words.
    #
    
    if(!decode)                                        # if encoding...
        x <- ngram(x, 1)                               # convert to word vector
    
    if (decode) {
        vocab <- vocab_dec                             # use inverted hash table and...
        x <- as.character(x)                           # convert to char for decoding
    }                                                  
    
    not_found <-!(has.key(x, vocab))                   # identify words without code and..,
    x[not_found] <- unkMark                            # replace unknown words with special tag
    res <- sapply(x, function(y){vocab[[y]]})          # get codes    
    names(res) <- NULL                                 # remove names
    return(res)                                        # and go home.
}
#
# skip.ngram - Creates bigrams within a specified window size
#
skip.ngram <- function(x, n=2, window=3, split=" ", sep="::"){
    # Takes a vector of strings, size of window, split char and separator
    # Returns a vector of bigrams within the specified window size
    
    # recursive ngram generator
    ngrm <- function(words, n, window, sep, ngrams){
        # processes all ngrams with first word and calls itself
        
        # check exit condition
        if (length(words) < n)                    # no more n-grams, end of story
            return(unlist(ngrams))
        
        # process ngrams and call 
        pairs <- min(window, length(words) - 1)   # number of pairs of ngrams to add
        for (i in 1:pairs){                       # append n-grams
            ngrams <- append(ngrams, 
                             list(paste(c(words[1], words[i+1]), collapse=sep))) 
        }
        return(ngrm(words[2:(length(words))], n, window, sep, ngrams)) # play it again, Sam
    }    
    
    # wrapper function
    if(split == " ")                              # if split by whitespaces
        split <- "\\s"                            # use it as split char but    
    x <- gsub(paste0(split,"+"), " ", x)          # make sure there's only one between words
    x <- gsub("^\\s+", "", x)                     # and none as first character
    words <- unlist(strsplit(x,split = split))    # create vector of words
    
    ngrams <- list()                              # list of ngrams
    if (n < 2)                                    # just return input words
        return(words)                             # or empty vector
    return(ngrm(words, n, window, sep, ngrams))   # not a trivial case. call generator.    
}
#
# tryNgram - Gets ngrams from n-gram data frame taking encoded or plain words
#
tryNgram <- function(words, n = 2,  df = NULL){
    if (class(words) == 'character')                      # encode words if necessary
        words <- nCode(words)
    if (is.null(df)) {
        df <- paste0('n', n, 'g_df')                      # point to n-gram data frame 
        df <- get(df)
    }
    return(getNgram(df, words))
}
#
# updt.DF - Updates ngram data frames
#
updt.DF <- function(x, y){
  # Takes a term frequency data frame and a term freq vector.
  # Returns a data frame with counts updated. Non existing keys are added.
  
  # Update existing keys
  for (i in 1:length(y)){
    x$Count[x$Key == names(y)[i]] <- x$Count[x$Key == names(y)[i]] + y[i]
  }
  
  # Add non existing keys
  toadd <- !(names(y) %in% x$Key)
  totadded <- sum(toadd)
  if (totadded > 0) {
    x <- rbind(x, ngram.DF(y[toadd]))
    cat('>>>', totadded, 'row(s) added to df. \n')    
  }

  # Return updated data frame in order
  return(x[order(x$Count, decreasing=T),])
}

#
# updt.DF - Updates ngram data frames - vectorized version - 
#
updt.DF <- function(x, y, test=FALSE){
    # Takes a ngram freq df of type nig_df (i=1,2,3...) and a term freq vector.
    # Returns a data frame of the right type with counts updated and new keys added.
    
    # to vectorize df update
    yinKeys <- names(y) %in% x$Key                        # y entries already in x
    xinKeys <- x$Key %in% names(y)                        # same from x point of view
    
    # Update existing keys
    x$Count[xinKeys] <- x$Count[xinKeys] + y[yinKeys]     # increment existing counters
    
    # Add non existing keys
    tot2add <- sum(!yinKeys)                              # Nr of rows to add
    if (tot2add > 0) {                                    # rbind without rows result in error
        x <- rbind(x, ngram.DF(y[!yinKeys]), row.names=NULL)
        rownames(x) <- NULL
        if (test)
            cat('>>>', tot2add, 'row(s) added to df. \n') # inform nr of rows added
    }
    
    # Return updated data frame in order
    return(x[order(x$Key, decreasing=F),])                # return new df in key order
}
#
# updt.Vocab - Updates Vocabulary
#
updt.Vocab <- function(x, Vocab=WRDS, test=FALSE) {
    #
    # Takes a character vector and adds inexisting words to Vocab
    #
    words <- ngram(x, 1)
    new_words <- !(words %in% Vocab)
    
    strt <- length(Vocab) + 1
    end <- strt + sum(new_words) - 1
    if (end >= strt)
        Vocab[strt:end] <- words[new_words]
    
    if(test)
        cat('>>>', sum(new_words), 'words added. \n')
    
    return(Vocab)
}
#
# updt.Vocab - Using hash package...
#
updt.Vocab <- function(x, Vocab=WRDS_H, Vocab_inv=WRDS_H_INV, test=FALSE) {
    #
    # Takes a character vector of words and adds inexisting ones 
    # to Vocab abd to the inverted Vocab hast table.
    # Returns the number of words actually added.
    #
    
    new_words <- !(has.key(x, Vocab))               # identify words to add  
    strt <- max(values(Vocab)) + 1                  # compute range of new values
    end <- strt + sum(new_words) - 1
    
    if (end >= strt) {                              # add new words 
        Vocab[x[new_words]] <- strt:end             # to Vocab
        Vocab_inv[strt:end] <- x[new_words]         # and inverted Vocab
    }                                
    
    if (test)                                       # useful for testing
        cat('>>> Words added:', paste(x[new_words], collapse='-'), '\n')
    
    return(sum(new_words))                          # Nr of words added
}

