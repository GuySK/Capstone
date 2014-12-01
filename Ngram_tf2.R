#
# Ngram.tf - Ngram Term Frequency
#
Ngram.tf <- function(x, n=2, fun=ngram, threshold=3, clk=0.1, chunkSize=0.1) {
    #
    # Takes a character vector and returns a list with two vectors 
    # with n-grams counts and probabilities.
    #
    
    strtTime <- Sys.time()

    cat('>>> Partitioning input file in', 
        as.integer(chunkSize ^ -1),
        'parts of size', as.integer(length(x) * chunkSize), '\n')
    xLst <- chop(x, n=as.integer(chunkSize ^ -1))
    
    cat('>>> Creating', paste0(n, '-grams'), '\n')
    ngramsLst <- list()
    for (i in 1:length(xLst)){
        cat('    Processing list', i, 'with', length(xLst[[i]]), 'vectors. \n' )
        ngrams <- sapply(xLst[[i]], fun, n=n)    
        ngramsLst <- append(ngramsLst, unlist(ngrams))
    }
    ngrams <- unlist(ngrams)
    names(ngrams) <- NULL
    cat('   ', length(ngrams), 'terms generated. \n')
    
    cat('>>> Estimating time for main process. \n')
    clkLst <- clock(dict, ngrams[1:(as.integer(length(ngrams) * clk))])
    tot_time <- convTime(clkLst[[1]] / clk)
    
    cat('    Estimated time for main process: ', 
        tot_time, attr(tot_time, 'units'), '\n')
    pause('>>> Continue???')

    cat('>>> Partitioning input file in', 
        as.integer(chunkSize ^ -1),
        'parts of size', as.integer(length(ngrams) * chunkSize), '\n')
    ngramsLst <- chop(ngrams, n=as.integer(chunkSize ^ -1))
    
    ltot = 0
    for (i in 1:length(ngramsLst)) {
        ltot = ltot + length(ngramsLst[[i]])
    }
    if (!(length(ngrams) - ltot == 0))
        stop(">>> Error: Number of recs do not match \n")    
    cat('>>> Terms integrity checked OK \n')
    
    cat('>>> Creating term frequencies dicts. \n')
    ndLst <- list()
    for (i in 1:length(ngramsLst)){
        cat('    Processing list', i, 'with', length(ngramsLst[[i]]), 'terms. \n' )
        nd <- dict(ngramsLst[[i]])
        nd2 <- nd[nd >= threshold]
        if (length(nd2) == 0) {
            cat('>>> No terms above threshold. Threshold downsized to 1. \n')
            nd2 <- nd
        }
        ndLst <- append(ndLst, list(nd2))
    }
    
    cat('>>> Clearing memory and unlisting dicts. \n')
    rm(ngramsLst); gc()
    nd <- unlist(ndLst)
    nd.prob <- nd / length(ngrams)
    
    elapsedTime <- round(Sys.time() - strtTime, 2)

    cat('>>> End of Job. Job time was:', elapsedTime, attr(elapsedTime, 'units'), '\n')
    return(list(nd, nd.prob))
}

