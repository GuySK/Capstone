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

