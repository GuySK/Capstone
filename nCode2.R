#
# nCode2 - Encodes / decodes a word vector using hash package's functions.
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
