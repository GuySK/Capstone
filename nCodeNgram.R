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

dCodeNgram <- function(ngram){
    #
    # Takes an ngram encoded with the nCodeNgram function.
    # Returns a character vector with the ngram words.
    #
    
    shiftVal = 2**16                        # Warning. it works with Vocabs up to 65536
    w <- c()                                # init output vector
    
    ngram <- as.double(ngram)               # Just in case
    
    while(ngram > 0){                       # loop until no more words to extract
        w <- append(w, ngram %% shiftVal)   # apply modulo to extract word
        ngram <- ngram %/% shiftVal         # shift ngram code and repeat
    }
    return(dCode(w))                        # decode and go home.
}
#