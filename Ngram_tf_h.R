#
# Ngram.tfh - Ngram Term Frequency
#
Ngram.tfh <- function(x, n=2, fun=ngram, threshold=1, clk=0.05, chunkSize=0.1) {
    #
    # Takes a character vector and returns a list with two vectors 
    # with n-grams counts and probabilities using function dict2 instead of dict.
    #
    
    strtTime <- Sys.time()
    
    cat('>>> Creating', paste0(n, '-grams'), '\n')
    ngrams <- sapply(x, fun, n=n)                     # default ngram generator is ngram
    ngrams <- unlist(ngrams)
    names(ngrams) <- NULL
    cat('   ', length(ngrams), 'terms generated. \n')
    
    cat('>>> Creating term frequencies dicts. \n')
        nd <- dict2(ngrams)
        del(keys(nd)[values(nd) <= threshold], nd)
    
    elapsedTime <- round(Sys.time() - strtTime, 2)

    cat('>>> End of Job. Job time was:', elapsedTime, attr(elapsedTime, 'units'), '\n')
    return(nd)
}