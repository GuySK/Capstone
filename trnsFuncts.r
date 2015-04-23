# 
# trnsFuncts - Transformation functions
#

# Alter stop words list order to avoid leaving orphan quotes
myStopWords <- stopwords('en')[order(stopwords('en'), decreasing=T)]
myStopWords <- append(myStopWords, c('xxcommaxx', 'xxstopxx', 'xxcolon'))

#
# convertPunct - Keeps chars to be taken into account by ngrams gens
#
convertPunct <- function(x){
    # Takes a string and returns a string.    
    x <- gsub(pattern = ", |,$", " xxcommaxx ", x)
    x <- gsub(pattern = "\\. |; |! ", " xxstopxx ", x)
    x <- gsub(pattern = "\\.$", " xxstopxx ", x)
    x <- gsub(pattern = "\\;$", " xxstopxx ", x)
    x <- gsub(pattern = "\\!$", " xxstopxx ", x)
    x <- gsub(pattern = ": |:$", " xxcolonxx ", x)        
    return(x)
}


#
# cleanDoc - Applies transformations to documents
#
cleanDoc <- function(x, control=list(convertTolower=c(TRUE, 1),
                                     verbose=FALSE,
                                     convertToASCII=TRUE,
                                     removePunct=TRUE,
                                     removeNumbers=TRUE,
                                     removeStopWords=c(TRUE, 'myStopWords'))) {
    #
    # cleanDoc - standardizes several document cleaning tasks
    #
    
    defaults <- list(convertTolower=c(TRUE, 1), 
                     verbose=FALSE, 
                     convertToASCII=TRUE,
                     removePunct=TRUE,
                     removeNumbers=TRUE,
                     removeStopWords=c(TRUE, 'myStopWords'))
    
    for (i in 1:length(defaults)){
        if (!(names(defaults)[i] %in% names(control)))
            control <- append(control, defaults[i])
    }
    
    if (control$verbose)
        cat('>>> cleaning document. Control options are: \n')
    for (i in 1:length(names(control))){
        if (control$verbose)
            cat('---', names(control)[i], unlist(control[[i]]), '\n')        
    }
    
    if (control$convertToASCII) {
        if (control$verbose) 
            cat('>>> getting rid of strange characters. \n')
        x <- map(x, toascii, progress=T, encoding='UTF-8')        
    }
    
    if (control$verbose) 
        cat('>>> Converting special characters. \n')    
    x <- convertPunct(x)
    
    if(control$convertTolower[1]) {
        if (control$verbose) 
            cat('>>> converting to lowercase in steps. \n')
        stepLst <- step(x, tolower, steps=control$convertTolower[2])
        x <- unlist(stepLst)
        rm(stepLst); gc()    
    }

    if (control$removeStopWords[1]){
        if (control$verbose) 
            cat('>>> removing stop words. \n')
        x <- removeWords(x, get(control$removeStopWords[2]))        
    }
    
    if (control$removeNumbers) {
        if (control$verbose) 
            cat('>>> removing numbers. \n')
        x <- removeNumbers(x)
    }

    if (control$removePunct) {
        if (control$verbose) 
            cat('>>> removing punctuation  \n')
        x <- removePunctuation(x)
    }
        
    if (control$verbose) 
        cat('>>> End of Job. \n')
    return(x)
}

#
# cleanSent - Cleans a sentence according to options
#
cleanSent <- function(x, control=list(convertTolower=c(TRUE),
                                     convertToASCII=TRUE,
                                     removePunct=TRUE,
                                     removeNumbers=TRUE,
                                     removeStopWords=c(FALSE, NULL))) {
    #
    # cleanSent - Cleans a sentence according to options
    #
    
    defaults <- list(convertTolower=c(TRUE), 
                     convertToASCII=TRUE,
                     removePunct=TRUE,
                     removeNumbers=TRUE,
                     removeStopWords=c(FALSE, NULL))
    
    for (i in 1:length(defaults)){
        if (!(names(defaults)[i] %in% names(control)))
            control <- append(control, defaults[i])
    }
    
    if (control$convertToASCII) {
        x <- toascii(x, encoding='UTF-8')        
    }
    
    x <- convertPunct(x)
    
    if(control$convertTolower)
        x <- tolower(x)

    if (control$removeStopWords[1])
        x <- removeWords(x, get(control$removeStopWords[2]))        
    
    if (control$removeNumbers) {
        x <- removeNumbers(x)
    }

    if (control$removePunct) {
        x <- removePunctuation(x)
    }
        
    return(x)
}
#
