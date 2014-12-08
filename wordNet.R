#
# WordNet - WordNet related functions
#

getPOS <- function(word) {
    # Looks up word in Wordnet.
    # Returns char vector with all word's POS. NULL if none. 

    pos <- c("ADJECTIVE", "ADVERB", "NOUN", "VERB")
    posCode <- c("A", "D", "N", "V")
    POS <- c()
    
    filter <- getTermFilter("ExactMatchFilter", word, TRUE)
    for (i in 1:length(pos)){
        term <- getIndexTerms(pos[i], 25, filter)
        if (!(is.null(term)))
            POS <- append(POS, posCode[i])
    }
    
    if (!(is.null(POS)))
        return(paste(POS, collapse=''))
    return(POS)
}

is.english <- function(x){
    return(!is.null(getPOS(x)))
}

# POS Tagger function
posTag <- function(words){
    tagged <- c()
    for (i in 1:length(words)){
        tagged <- append(tagged, paste0(words[i], '/', getPOS(words[i])))
    }
    return(tagged)
} 