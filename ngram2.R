#'
#' ngram2 - Recursive n-gram generator with encoding capability
#'
ngram2 <- function(x, n=2, split=" ", sep="::", 
                  startMark='<s>', stopMark='</s>',
                  encoded=FALSE,
                  encode=TRUE, ...) {
    #
    # Takes a vector of strings, size of n-grams to generate, split and 
    # separator chars and encoding option.
    # Returns a vector of n-grams in plain english or encoded as numerics (doubles)
    #
    
    # recursive ngram generator function
    ngrm <- function(words, n, sep, ngrams){
        
        if (length(words) < n)                    # no more n-grams, end of the story
            return(unlist(ngrams))
        
        if (encode){
            ngrams <- append(ngrams, nCodeNgram(words[1:n],      # encode and append n-gram 
                                                encoded=encoded))              
        } else {
            ngrams <- append(ngrams, list(paste(words[1:n], collapse=sep))) # append n-gram            
        }
        
        # recursive call
        return(ngrm(words[2:(length(words))], n, sep, ngrams)) # again, without first word 
    }    
    
    # wrapper function
    if(split == " ")                              # if split by whitespaces
        split <- "\\s"                            # use it as split char but
    
    x <- gsub(paste0(split,"+"), " ", x)          # make sure there's only one between words
    x <- gsub("^\\s+", "", x)                     # and none as first character
    
    words <- unlist(strsplit(x,split = split))    # create vector of words
    
#     if (n > 1) {
#         words <- append(startMark, words)         # add start... 
#         words <- append(words, stopMark)          # and stop markers            
#     }
    
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
    
    return(ngrm(words, n, sep, ngrams))           # not a trivial case. call generator.    
}