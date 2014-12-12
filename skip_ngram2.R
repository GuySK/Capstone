#
# skip.ngram - Creates bigrams within a specified window size
#
skip.ngram2 <- function(x, 
                        n=2, 
                        window=3, 
                        split=" ", 
                        sep="::",
                        encoded=FALSE,
                        encode=TRUE){
    
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
        for (i in 1:pairs) {                      # append n-grams
            
            if (encode) {
                currpair <- c(words[1], words[i+1])
                ngrams <- append(ngrams, nCodeNgram(currpair, encoded=encoded))         
            } else {
                currpair <- paste(c(words[1], words[i+1]), collapse=sep)
                ngrams <- append(ngrams, list(currpair)) 
            }

        }
        return(ngrm(words[2:(length(words))], n, window, sep, ngrams)) # play it again, Sam
    }    
    
    # wrapper function
    if(split == " ")                              # if split by whitespaces
        split <- "\\s"                            # use it as split char but
    
    x <- gsub(paste0(split,"+"), " ", x)          # make sure there's only one between words
    x <- gsub("^\\s+", "", x)                     # and none as first character
    
    words <- unlist(strsplit(x,split = split))    # create vector of words
    
    # init output vector or list
    if (encoded) {
        ngrams <- c()                             # use vector for encoded ngrams
    } else {                                      # but use list for uncoded ngrams
        ngrams <- list()                             
    }
    
    if (n < 2) {                                  # one or no word. just return  words
        if (encode)                               # encoded or...
            return(nCode2(words))
        return(words)                             # plain english
    }                                    
    
    return(ngrm(words, n, window, sep, ngrams))   # not a trivial case. call generator.    
}