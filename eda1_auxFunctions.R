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
        cat("    Step ", i,". ", sep="")
        init <- 1 + (i-1) * recs
        if (i == steps)
            recs = recs + length(x) %% steps
        y <- fun(x[init:(init + recs - 1)], ...)
        lst_y <- append(lst_y, list(y))
        cat(recs, "records processed in this step. \n")
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
dict_c <- function(x){
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
#' dict -  Creates a dictionary of elements and counts
#'         in matrix format.
#'
dict_n <- function(x){
    #
    # Takes a vector of ngrams codes and computes their frequencies.
    # Returns a matrix with Codes and Counts.
    # 
    x <- x[order(x)]                         # sort input vector
    
    y <- matrix(data = 0,                    # create output matrix 
                nrow = length(unique(x)),      # one row for each code
                ncol=2)                        # and two columns
    colnames(y) <- c('Code', 'Count')          # name columns
    
    j <- 1                                   # init first element
    y[1, 'Code'] <- x[1]                     # and get first ngram code
    
    for (i in 1:length(x)){                  # compute nr of elements for each code    
        if (x[i] != y[j, 'Code']){             # if code changes 
            j = j + 1                          # move code pointer
            y[j, 'Code'] <- x[i]               # and get new code
        }
        y[j, 'Count'] <- y[j, 'Count'] + 1     # count element
    }
    y                                        # return matrix
}
#
# dict - general dict function. Calls dict_c or dict_n as needed.
#
dict <- function(x){
    if (is.character(x)){
        return(dict_c(x))
    } else {
        return(dict_n(x))
    }
}
#
# vdict - creates a dictionary of elements and counts
#       - vectorized version
#
vdict <- function(x){
    y <- rep(0, length(unique(x)))      # create output vector
    
    for (i in 1:length(y)){             # process each output entry
        names(y)[i] <- x[1]             # assign entry name
        idxx <- names(y)[i] == x        # get index
        y[i] <- sum(idxx)               # add counts
        x <- x[-idxx]                   # remove processed entries from input
    }
    y                                   # return output vector
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
#            New version. Replace the one above.  
#
ngram.DF <- function(x, encoded=TRUE) {
    # Creates a data frame from a dict of frequencies
    
    k <- as.double(names(x))                # ngram code as key
    w <- sapply(k, dCodeNgram)              # get a matrix with ngram words as rows 
    
    if (class(w) != 'matrix'){              # but check if it's a vector
        n <- 1                              # yep. it's an 1-gram
    } else {                                # nope. bigram or greater ngram
        n <- nrow(w)                        # number of words in ngram
    }
    
    df <- data.frame(Key=k, Count=x,        # init data frame with key and frequencies
                     row.names=NULL, 
                     stringsAsFactors = FALSE)
    
    if (n == 1) {                           # for just words add a single column
        df <- data.frame(df, w, stringsAsFactors = FALSE)
        colnames(df)[3] <- 'W1'             # column name
    } else {                                # for high orded ngrams we need a loop
        for (i in 1:n){                     # add columns for each word in the ngram
            df <- data.frame(df, w[i,],     
                             stringsAsFactors = FALSE)
            colnames(df)[2+i] <- paste0('W',i)  # add name to data frame column
        }
    }
    return(df)                              # done, go home.
}
#
# ngram.DF - Creates a data frame from a dict of term frequencies
#            New version. Replace the one above.  
#
ngram.DF <- function(x, encoded=TRUE, sep='::') {
    
    # Creates a data frame from a dict of frequencies
    
    if (encoded) {
        k <- as.double(names(x))                # ngram code as key
        sk <- sapply(k, dCodeNgram,             # get n-1 ngrams 
                     subngram=T,                # to use as search keys
                     decode = F)
        w <- sapply(k, dCodeNgram)              # get a matrix with ngram words as rows 
        
        if (class(w) != 'matrix'){              # but check if it's a vector
            n <- 1                              # yep. it's an 1-gram
        } else {                                # nope. bigram or greater ngram
            n <- nrow(w)                        # number of words in ngram
        }
                
        if (n == 1) {                           # for just words add a single column
            df <- data.frame(Key=k,             # and init data frame
                             Count=x,           # with no sub key
                             row.names=NULL, 
                             stringsAsFactors = FALSE)            
            df <- data.frame(df, w, stringsAsFactors = FALSE) # add words
            colnames(df)[3] <- 'W1'             # and column name
            
        } else {                                # for high orded ngrams 
            df <- data.frame(Key=k,             # init data frame with sub key
                             Count=x, 
                             Skey=sk,    
                             row.names=NULL, 
                             stringsAsFactors = FALSE)
            for (i in 1:n){                     # add columns for each word in the ngram
                df <- data.frame(df, w[i,],     
                                 stringsAsFactors = FALSE)
                colnames(df)[3+i] <- paste0('W',i)  # add name to data frame column
            }
        }
        
        df$Prob <- x / sum(x)                   # add Probability column
        df$ProbWB <- x / (sum(x) + length(x))   # add Witten Belt discounted probability
        
    } else {                                    # ngrams are not encoded
        k <- names(x)                           # get keys
        lk <- strsplit(k, sep)                  # split words
        w <- matrix(data=NA, nrow=length(x),    # create matrix 
                    ncol=length(lk[[1]]))
        
        for (i in 1:length(lk[[1]])){           # unlist columns into matrix 
            w[,i] <- unlist(lapply(lk, function(x){x[i]}))
        }
        
        count <- x                              # init data frame with keys and freq
        df <- data.frame(Key=k, Count=count, 
                         row.names=NULL, 
                         stringsAsFactors = FALSE)
        
        for (i in 1:ncol(w)){                  # add columns to df
            col <- w[,i]
            if (encoded)                       # ensure word as numeric if encoded
                col <- as.numeric(as.character(w[,i]))
            df <- data.frame(df, col,          # add column to df 
                             stringsAsFactors = FALSE)
            colnames(df)[2+i] <- paste0('W',i) # give column a name
        }
    }
    
    return(df)                                 # done. go home.
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
# nCode2 - Encodes / decodes a word vector using hash package's functions.
#          Replaces functions nCode and dCode.
#
nCode2 <- function(x, vocab=WRDS_H, unkMark='<UNK>', 
                   decode=FALSE, vocab_dec=WRDS_I){
    #
    # Encodes or decodes a vector of words. If decode = TRUE, 
    # an inverted hash table must be specified as vocab.
    # Returns a vector of encoded / decoded words.
    #
    
    if(!decode){
        x <- ngram(x, 1)                                # convert to word vector
        not_found <-!(has.key(x, vocab))                # identify words without code and..,
        x[not_found] <- unkMark                         # replace unknown words with special tag
        res <- sapply(x, function(y){vocab[[y]]})       # get codes    
        names(res) <- NULL                              # remove names
        return(res)                                     # and go home.
    }
    
    if (decode) {
        unkMark <- vocab[[unkMark]]                    # get <UNK> code
        vocab <- vocab_dec                             # use inverted hash table and...
        not_found <- is.na(vocab[x])                   # identify words without code and..,
        x[not_found] <- unkMark                        # replace unknown words UNK tag code
        res <- sapply(x, function(y){vocab[[y]]})      # get words    
        names(res) <- NULL                             # remove names
        return(res)                                    # and go home.
    }
}

#
# for compatibility with old versions
nCode <- function(x, vocab=WRDS, unkMark='<UNK>')
    return(nCode2(x, vocab=WRDS_H, unkMark='<UNK>'))

# a shorthand version for decoding
dCode <- function(x) {nCode2(x, decode=T)}  

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
    if (tot2add > 0) {                                    # rbind without rows results in error
        x <- rbind(x, ngram.DF(y[!yinKeys]), row.names=NULL)
        rownames(x) <- NULL
        if (test)
            cat('>>>', tot2add, 'row(s) added to df. \n') # inform nr of rows added
    }
    
    # Return updated data frame in order
    return(list(added = tot2add, 
                updted = sum(yinKeys), 
                df = x[order(x$Key, decreasing=F),]))     # return new df in key order
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
updt.Vocab <- function(x, Vocab=WRDS_H, Vocab_inv=WRDS_I, test=FALSE) {
    #
    # Takes a character vector of words and adds inexisting ones 
    # to Vocab and to the inverted Vocab hast table.
    # Returns the number of words actually added.
    #
    
    new_words <- !(has.key(x, Vocab))               # identify words to add  
    strt <- max(values(Vocab)) + 1                  # compute range of new values
    end <- strt + sum(new_words) - 1
    
    if (end >= strt) {                              # add new words 
        Vocab[x[new_words]] <- strt:end             # to Vocab
        Vocab_inv[strt:end] <- x[new_words]         # and inverted Vocab
        inv_Vocab <- invertVocab(Vocab)             # create inv Vocab vector - new
    } else {
        inv_Vocab <- c()                            # nothing to return
    }                                
    
    if (test)                                       # useful for testing
        cat('>>> Words added:', paste(x[new_words], collapse='-'), '\n')
    
    return(list(sum(new_words), inv_Vocab))         # Nr of words added & inv vocab vector
}
#
# invertVocab - Creates an inverted vocabulary - vector format
#
invertVocab <- function(vocab = WRDS_H){
    #
    # Inverts hash table setting code as index
    # 
    
    invertedVocab <- rep(NA, length(vocab))    # create output vector
    vals <- values(vocab)                      # get values from Vocab
    invertedVocab[vals] <- names(vals)         # fill it with corresponding names
    return(invertedVocab)                      # return inverted Vocab
}

#
# Probability functions
#

# Standard Probability
getProb <- function(x) x / sum(x)

# Witten Belt discounted probability
getProbWB <- function(x) x / (sum(x) + length(x))

#
# nextWord -  Finds most probable word in sentence
#
nextWord <- function(sentence, n = 3, talkative = FALSE){
    #
    # Retrieves the most probable word using N-grams
    # Returns the word and its discounted probability
    # 
    if (talkative)                                         # debugging mode
        cat('n=', n, '\n')
    
    if (n == 1) 
        return(n1gn_df[which(max(n1gn_df$ProbWB) == n1gn_df$ProbWB), 'W1'])    
    
    df <- paste0('n', n, 'gn_df')                          # ngram data frame name
    
    srchNgrams <- ngram2(sentence, n = n-1, encode = T)    # convert sentence to n-grams
    srch <- srchNgrams[length(srchNgrams)]                 # get the last one
    candidates <- subset(get(df), Skey == srch)            # search the n-gram data frame 
    
    if (nrow(candidates) == 0) {                           # if no results 
        return(nextWord(sentence, n-1,                     # back off to n-1 n-gram
                        talkative = talkative)) 
    }
    
    col <- paste0('W', n)                                  # got it, return max prob(W)
                                                           # with Witten Belt discounting
    maxProb <- max(candidates$ProbWB)
    selWord <- candidates[which(candidates$ProbWB == maxProb), col] # get the word
    
    if (length(selWord) == 1)                              # No ties, return word
        return(selWord)
    
    selWord_n <- sapply(selWord, nCodeNgram)               # get keys
    maxProb <- max(n1gn_df[n1gn_df$Key %in% selWord_n, 'ProbWB'])
    # maxProb <- max(n1gn_df[n1gn_df$W1 %in% selWord, 'ProbWB']) # get max 1-gram probability
    selWord <- n1gn_df[which(n1gn_df$ProbWB == maxProb), 'W1'] # get most probable word(s)
    
    return(selWord[1])                                     # if more than one, return first one
}
#
# predict_word - Predicts next word in a sentence
#
predict_word <- function(sentence, silent = FALSE){
    #
    
    if (!silent){
        cat('\n --->', sentence, '<--- \n')
    }
    
    words <- ngram2(sentence, n = 1, encode = F, encoded = F)
    predicted <- nextWord(words)
    
    return(predicted)
}

#
# chooseSent - Cuts a sentence at random for prediction
#
chooseSent <- function(words, stopMark = 'xxstopxx'){
    # words is a vector of words
    # returns a list with a sentence and its next word
    
    if (length(words) == 0)                         # there's nothing to choose from 
        stop('ERROR. Void sentence. \n')
    
    idx <- sample(1:length(words), 1)               # choose one word at random
    
    if (idx == 1) {                                 # create a stop if first word chosen
        words <- c(stopMark, words)
        idx <- idx + 1
    }                                         
    sent <- words[1:(idx - 1)]                      # get sentence to predict  
                       
    return(list(sent, words[idx]))                  # return sentence and target word
}


#
# predictSentences -  Finds most probable word continuation in sentences
#
predictSentences <- function(sentences, MaxNgram = 3){
    #
    # predictSentences - Predicts sentences 
    #
    
    # clean sentences with standard transformations
    cat('clean sentences with standard transformations. \n')
    sents <- cleanSent(sentences)
    cat('Done. \n')
    
    # tokenize sentences
    cat('tokenize sentences. \n')
    tokenSents <- lapply(sents, FUN= ngram2, USE.NAMES = F, n=1, encode=F)
    cat('Done. \n')
    
    # choose random subsentence and target word 
    cat('choose random subsentence and target word. \n')
    sentList <- lapply(tokenSents, chooseSent)
    cat('Done. \n')
    
    # group all sentences to be predicted together as a char vector
    cat('group all sentences to be predicted together as a char vector. \n')
    toPredSents <- sapply(sentList, "[[", 1)
    cat('Done. \n')
    
    # group all target words
    cat('group all target words. \n')
    targets <- sapply(sentList, "[[", 2)
    cat('Done. \n')
    
    # predict next word for each sentence
    cat('predict next word for each sentence. \n')
    predicts <- sapply(toPredSents, nextWord, n = MaxNgram)
    cat('Done. \n')
    
    # rebuild predicted sentence
    cat('rebuild predicted sentence. \n')
    predSents <- sapply(toPredSents, paste, collapse = ' ')
    cat('Done. \n')
    
    # return a list with target and prediction
    return(data.frame(Target = targets, 
                      Prediction = predicts, 
                      Sentence = predSents, 
                      stringsAsFactors = FALSE))
}

#
# getWord - Retrieves a word from 1-gram dataframe.
#
getWord <- function(code, df = n1gn_df){
    #
    # retrieves word(s) by code.
    #
    
    if (missing(code))
        return(NULL)
    
    if (length(code) > 1)
        return(sapply(code, getWord, df))
    
    return(df[df$Key == code, 'W1'])    
}
#
# getCount - Retrieves a word's count from 1-gram dataframe.
#
getCount <- function(code, df = n1gn_df){
    #
    # retrieves word(s) by code.
    #
    
    if (missing(code))
        return(NULL)
    
    if (length(code) > 1)
        return(sapply(code, getCount, df))
    
    return(df[df$Key == code, 'Count'])    
}

#
# compactWords - Compacts Vocab and n1gram data frame
#

compactWords <- function(df = n1gn_df,
                         thresHold = 1,
                         talkative = FALSE){
    #
    # Gets rid of terms with frequencies below the specified threshold
    # Returns an n1gram data frame, a new Vocab and a new inverted Vocab.
    #
    
    # identify terms to be discarded and create new data frame with the rest
    unfreq <- df$Count <= thresHold          # identify low frequent terms
    df_new <- df[!unfreq,]                   # get rid of them
    nterms <- nrow(df_new)                   # compute new size of Vocab
    deleted <- nrow(df) - nterms
    
    # Abort process if no terms below threshold
    if (deleted <= 0){
        cat('Warning. No terms purged. Process terminated. \n')
        return()   
    }
    
    # Compact keys and replace old ones
    df_new <- df_new[order(df_new$Count, decreasing=T),]    # sort data frame by frequency
    df_new$Key <- 1:nrow(df_new)                            # assign new keys
    
    # Compute new probabilities; regular and discounted (Witten Bell)
    df_new$Prob <- getProb(df_new$Count)      # compute term probabilities
    df_new$ProbWB <- getProbWB(df_new$Count)  # and discounted probabilities
    prob_unk <- round((1 - sum(df_new$ProbWB)) * 100,2)
    
    # re-create hash table for Vocab and inverted vocab vector
    Vocab <- hash(df_new$W1, df_new$Key)      # new Vocabulary
    Vocab['<UNK>'] <- nrow(df_new) + 1        # add Unknown code to Vocab
    Vocab_inv <- invertVocab(vocab = Vocab)   # vocab to search by code
    
    if (talkative){
        cat('Vocab is now ', nterms, ' terms. \n', sep='')
        cat('Probability of unknown terms: ', prob_unk, '%. \n', sep='' )        
    }
    
    return(list(df_new, Vocab, Vocab_inv))
}
#
# compactWords - Compacts Vocab and n1gram data frame
#

compactNgramDF <- function(df,
                         thresHold = 1,
                         compact_keys = FALSE,
                         talkative = FALSE){
    #
    # Gets rid of terms with frequencies below the specified threshold
    # Returns an n1gram data frame, a new Vocab and a new inverted Vocab.
    #
    
    # identify terms to be discarded and create new data frame with the rest
    unfreq <- df$Count <= thresHold          # identify low frequent terms
    df_new <- df[!unfreq,]                   # get rid of them
    nterms <- nrow(df_new)                   # compute new size of Vocab
    deleted <- nrow(df) - nterms
    
    
    
    # Abort process if no terms below threshold
    if (deleted <= 0){
        cat('Warning. No terms purged. Process terminated. \n')
        return()   
    }
    
    # Compact keys and replace old ones
    if(compact_keys) {
        df_new <- df_new[order(df_new$Count, decreasing=T),]    # sort data frame by frequency
        df_new$Key <- 1:nrow(df_new)                            # assign new keys        
    }
    
    # Compute new probabilities; regular and discounted (Witten Bell)
    df_new$Prob <- getProb(df_new$Count)      # compute term probabilities
    df_new$ProbWB <- getProbWB(df_new$Count)  # and discounted probabilities
    prob_unk <- round((1 - sum(df_new$ProbWB)) * 100,2)
    
    if (talkative){
        cat(deleted, ' terms deleted from data frame. \n', sep='')
        cat('Data Frame has now ', nterms, ' terms. \n', sep='')        
        cat('Probability of unknown terms: ', prob_unk, '%. \n', sep='' )        
    }
    
    rownames(df_new) <- NULL
    return(df_new)
}

remove_unk <- function(df, unkMark = '<UNK>', talkative = TRUE){
    #
    # Removes unknown values from n-gram dataframes.
    #
    
    # to know ngram degree and get corresponding column
    n <- max(grep('[1-9]', colnames(df)[grep('^W.',  colnames(df))]))
    col_n <- which(colnames(df) == paste0('W',n))       
    
    # remove rows with unknown values at rightmost word
    to_delete <- df[, col_n] == unkMark
    df <- df[!to_delete,]

    # talk to them
    if (talkative) {
        cat(sum(to_delete), 'rows deleted from data frame. \n')
    }
    
    # recompute probabilities: standard and Witten-Bell
    df$Prob   <- getProb(df$Count)
    df$ProbWB <- getProbWB(df$Count)

    # go home
    return(df)
}


#
# where -  searches an object in all environments.
#
where <- function(name, env = parent.frame()) {
    if (identical(env, emptyenv())) {
        # Base case
        stop("Can't find ", name, call. = FALSE)
        
    } else if (exists(name, envir = env, inherits = FALSE)) {
        # Success case
        env
        
    } else {
        # Recursive case
        where(name, parent.env(env))
        
    }
}
#
# New ngram.DF and updt.DF due to change in dict output format (matrix)
#

# ngram.DF - Creates a data frame from a dict of term frequencies
#            New version. Replace the one above.  
#
ngram.DF <- function(x, encoded=TRUE, sep='::') {
    
    # Creates a data frame from a dict of frequencies
    
    if (encoded) {
        
        # k <- as.double(names(x))              # ngram code as key
        k <- x[, 'Code']                        # due to change in dict output format
        x <- x[, 'Count']                       # from here on, the same
        
        sk <- sapply(k, dCodeNgram,             # get n-1 ngrams 
                     subngram=T,                # to use as search keys
                     decode = F)
        w <- sapply(k, dCodeNgram)              # get a matrix with ngram words as rows 
        
        if (class(w) != 'matrix'){              # but check if it's a vector
            n <- 1                              # yep. it's an 1-gram
        } else {                                # nope. bigram or greater ngram
            n <- nrow(w)                        # number of words in ngram
        }
        
        if (n == 1) {                           # for just words add a single column
            df <- data.frame(Key=k,             # and init data frame
                             Count=x,           # with no sub key
                             row.names=NULL, 
                             stringsAsFactors = FALSE)            
            df <- data.frame(df, w, stringsAsFactors = FALSE) # add words
            colnames(df)[3] <- 'W1'             # and column name
            
        } else {                                # for high orded ngrams 
            df <- data.frame(Key=k,             # init data frame with sub key
                             Count=x, 
                             Skey=sk,    
                             row.names=NULL, 
                             stringsAsFactors = FALSE)
            for (i in 1:n){                     # add columns for each word in the ngram
                df <- data.frame(df, w[i,],     
                                 stringsAsFactors = FALSE)
                colnames(df)[3+i] <- paste0('W',i)  # add name to data frame column
            }
        }
        
        df$Prob <- x / sum(x)                   # add Probability column
        df$ProbWB <- x / (sum(x) + length(x))   # add Witten Belt discounted probability
        
    } else {                                    # ngrams are not encoded
        stop('Unsupported option. Ngrams must be encoded. \n')
    }
    
    return(df)                                 # done. go home.
}
# 
#
# updt.DF - Updates ngram data frames - vectorized version - 
#
updt.DF <- function(x, y, test=FALSE){
    # Takes a ngram freq df of type nig_df (i=1,2,3...) and a term freq vector.
    # Returns a data frame of the right type with counts updated and new keys added.
    
    # to vectorize df update
    yinKeys <- y[, 'Code'] %in% x$Key                     # y entries already in x
    xinKeys <- x$Key %in% y[, 'Code']                     # same from x point of view
    
    # Update existing keys
    x$Count[xinKeys] <- x$Count[xinKeys] + y[yinKeys, 'Count'] # increment existing counters
    
    # Add non existing keys
    tot2add <- sum(!yinKeys)                              # Nr of rows to add
    if (tot2add > 0) {                                    # rbind without rows results in error
        x <- rbind(x, ngram.DF(y[!yinKeys,]), row.names=NULL)
        rownames(x) <- NULL
        if (test)
            cat('>>>', tot2add, 'row(s) added to df. \n') # inform nr of rows added
    }
    
    # Return updated data frame in order
    return(list(added = tot2add, 
                updted = sum(yinKeys), 
                df = x[order(x$Key, decreasing=F),]))     # return new df in key order
}
#
df_degree <- function(df){
    # to know ngram degree and get corresponding column
    n <- max(grep('[1-9]', colnames(df)[grep('^W.',  colnames(df))]))
    return(n)    
}
#
word_prob <- function(word, unkMark = '<UNK>'){
    wcode <- nCode(word)
    if (wcode != nCode(unkMark)){
        return(n1gn_df[n1gn_df$Key == wcode, 'ProbWB'])   
    } else {
        return(1 - sum(n1gn_df$ProbWB))
    }
}
