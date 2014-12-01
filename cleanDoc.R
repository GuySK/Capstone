#
#
#
cleanDoc <- function(x, control=list(convertTolower=c(TRUE, 1),
                                     verbose=FALSE,
                                     convertToASCII=TRUE,
                                     removePunct=TRUE,
                                     removeNumbers=TRUE,
                                     removeStopWords=c(TRUE,'en'))) {
    #
    # cleanDoc - standardizes several document cleaning tasks
    #
    
    defaults <- list(convertTolower=c(TRUE, 1), 
                     verbose=FALSE, 
                     convertToASCII=TRUE,
                     removePunct=TRUE,
                     removeNumbers=TRUE,
                     removeStopWords=c(TRUE,'en'))
    
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
        x <- removeWords(x, stopwords(control$removeStopWords[2]))        
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
    
    if (control$convertToASCII) {
        if (control$verbose) 
            cat('>>> getting rid of strange characters. \n')
        x <- map(x, toascii, progress=T, encoding='UTF-8')        
    }
    
    if (control$verbose) 
        cat('>>> End of Job. \n')
    return(x)
}
#

