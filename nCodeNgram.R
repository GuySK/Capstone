#
# nCodeNgram - Encodes an ngram (up to 3-grams) as a double
#
nCodeNgram <- function(words, encoded=FALSE){
    # 
    # Takes a charcter vector of words (up to 3) and returns an ngram
    # encoded as a double (floating point precision)
    #
    
    shiftVal = 2**16                         # Warning. This function works with Vocabs 
    ngram <- 0                               # with length up to 65536 only.       
    
    if (!encoded) {
        words <- nCode2(words)               # encode words if not already encoded        
    } else {
        words <- as.integer(words)           # or make sure they are numbers otherwise
    }
    
    for (i in 1:length(words)){
        ngram <- ngram + words[i] * shiftVal ** (i-1)
    }
    return(ngram)
}

dCodeNgram <- function(ngram, 
                       decode=TRUE, 
                       subngram=FALSE){
    #
    # Takes an ngram encoded with the nCodeNgram function.
    # Returns a character vector with the ngram words.
    #
    
    shiftVal = 2**16                        # Warning. it works with Vocabs up to 65536
    w <- c()                                # init output vector
    
    ngram <- as.double(ngram)               # Just in case
    
    if (subngram) {                               # (n-1)ngram required
        words <- dCodeNgram(ngram, decode=F)      # get words
        words <- words[1:(length(words) - 1)]     # take the first n-1
        if (decode)                               # return it decoded 
            return(dCode(words))              
        return(nCodeNgram(words, encoded = T))    # return (n-1)ngram encoded
    }
    
    while(ngram > 0){                       # loop until no more words to extract
        w <- append(w, ngram %% shiftVal)   # apply modulo to extract word
        ngram <- ngram %/% shiftVal         # shift ngram code and repeat
    }
        
    if (decode)
        w <- dCode(w)
    
    return(w)                               # done. go home.
}
#
#
# nCodeNgram - Encodes an ngram (up to 3-grams) as a double
#
nCodeNgram <- function(words, encoded=FALSE){
    # 
    # Takes a charcter vector of words (up to 3) and returns an ngram
    # encoded as a double (floating point precision)
    #
    
    shiftVal = 2**24                         # Warning. This function works with Vocabs 
    ngram <- 0                               # with length up to 16384 Kwords only.       
    
    if (!encoded) {
        words <- nCode2(words)               # encode words if not already encoded        
    } else {
        words <- as.double(words)            # or make sure they are numbers otherwise
    }
    
    for (i in 1:length(words)){
        ngram <- ngram + words[i] * shiftVal ** (i-1)
    }
    return(ngram)
}

dCodeNgram <- function(ngram, 
                       decode=TRUE, 
                       subngram=FALSE){
    #
    # Takes an ngram encoded with the nCodeNgram function.
    # Returns a character vector with the ngram words.
    #
    
    shiftVal = 2**24                        # Warning. it works with Vocabs up to 16384 Kwords
    w <- c()                                # init output vector
    
    ngram <- as.double(ngram)               # Just in case
    
    if (subngram) {                               # (n-1)ngram required
        words <- dCodeNgram(ngram, decode=F)     # get words
        words <- words[1:(length(words) - 1)]     # take the first n-1
        if (decode)                               # return it decoded 
            return(dCode(words))              
        return(nCodeNgram(words, encoded = T))   # return (n-1)ngram encoded
    }
    
    while(ngram > 0){                       # loop until no more words to extract
        w <- append(w, ngram %% shiftVal)   # apply modulo to extract word
        ngram <- ngram %/% shiftVal         # shift ngram code and repeat
    }
    
    if (decode)
        w <- dCode(w)
    
    return(w)                               # done. go home.
}
#
#
#
# nCodeNgram - Encodes an ngram (up to 3-grams) as a double
#
nCodeNgram <- function(words, encoded=FALSE, shift=2**17){
    # 
    # Takes a charcter vector of words (up to 3) and returns an ngram
    # encoded as a double (floating point precision)
    #
        
    shiftVal = shift                         # Warning. This function works with Vocabs 
    ngram <- 0                               # with length up to shiftVal-1 only.       
    
    if (!encoded) {
        words <- nCode2(words)               # encode words if not already encoded        
    } else {
        words <- as.double(words)            # or make sure they are numbers otherwise
    }
    
    for (i in 1:length(words)){
        ngram <- ngram + words[i] * shiftVal ** (i-1)
    }
    return(ngram)
}

dCodeNgram <- function(ngram, 
                       decode=TRUE, 
                       subngram=FALSE,
                       shift=2**17){
    #
    # Takes an ngram encoded with the nCodeNgram function.
    # Returns a character vector with the ngram words.
    #
    
    shiftVal = shift                        # Warning. works with Vocabs up to shiftVal-1 words
    w <- c()                                # init output vector
    
    ngram <- as.double(ngram)               # Just in case
    
    if (subngram) {                               # (n-1)ngram required
        words <- dCodeNgram(ngram, decode=F)     # get words
        words <- words[1:(length(words) - 1)]     # take the first n-1
        if (decode)                               # return it decoded 
            return(dCode(words))              
        return(nCodeNgram(words, encoded = T))   # return (n-1)ngram encoded
    }
    
    while(ngram > 0){                       # loop until no more words to extract
        w <- append(w, ngram %% shiftVal)   # apply modulo to extract word
        ngram <- ngram %/% shiftVal         # shift ngram code and repeat
    }
    
    if (decode)
        w <- dCode(w)
    
    return(w)                               # done. go home.
}
#