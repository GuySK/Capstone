#'
#'
#' Capstone Project
#' 
#' Task 3 - Modeling - 7: blogs data set
#' Model based on n-grams. 
#'
#'
# ---

# --------                            ------------------------------------------------
# Settings                            ---> Review Carefully before running script <---
# --------                            ------------------------------------------------
AWS = FALSE;                          # Computer running script: AWS or own
DSET = 1;                             # Blogs=1, News = 2, Tweets=3
TRAINPROP = 0.05;                     # Proportion of data set to be used as training set
SEED_N = 191;                         # Seed for random operations
nCode_STEPS = 10;                     # Number of steps to encode training data
REC_LIMIT = 1;                        # Proportion of records to process in this run
EXPRS_N = 100000;                     # limit on number of nested expr to be evaluated
ngram_THRESH = 2;                     # Min frequency of ngrams to keep
CHUNK_PROP = 0.1;                     # Default chunk size for chopping function
UNK_LMT = 0.005;                      # quantile limit to replace for <UNK>
# --------                            ------------------------------------------------

cat('>>> Script settings. \n')
cat('    AWS =', AWS, '\n')
cat('    DSET =', DSET, '\n')
cat('    TRAINPROP =', TRAINPROP, '\n')
cat('    SEED_N =', SEED_N, '\n')
cat('    nCode_STEPS =', nCode_STEPS, '\n')
cat('    REC_LIMIT =', REC_LIMIT, '\n')
cat('    EXPRS_N =', EXPRS_N, '\n')
cat('    ngram_THRESH =', ngram_THRESH, '\n')
cat('    CHUNK_PROP =', CHUNK_PROP, '\n')

# ---
cat(">>> Basic setup. \n")
if (AWS) {
    setwd("~/gsk1")
} else {
    setwd("~/../Google Drive 2/Training/DataScience/CapstonePj")
}

source("./AWS/eda1_auxFunctions.R")
source("./AWS/AuxFunctions.R")
source("./AWS/trnsFuncts.R")
source("./AWS/Ngram_tf.R")

library(tm)
library(hash)

# --- increase options setting for recursive functions
options(expressions=EXPRS_N)

# ---
sourceDir = './DataSets'
LANG = 'eng'
cat(">>> Loading corpus. \n")
pause("Continue????")
# Script run continues 

if (file.access('Corpus_en', 4) == 0) {
  cat('>>> Loading object from disk. \n')
  load('Corpus_en')
} else {
  cat('>>> Creating Corpus from source directory. \n')
  crp <- Corpus(DirSource(sourceDir, encoding = "UTF-8"), 
                readerControl = list(language = LANG))
}

# --- Getting data set and training data
cat('>>> Getting data set and training data. \n')
dset <- crp[[DSET]]
if (AWS)                                    
    dset <- unlist(dset[1]) 

if (file.access('Corpus_en', 0) == -1)
  save(crp, file='Corpus_en')
rm(crp); gc()

# --- Creating Vocabulary
cat('>>> Creating Vocabulary. \n')
pause('Proceed?')
#
set.seed(SEED_N)
train <- sample(1:length(dset), length(dset) * TRAINPROP)
strt <- Sys.time()

ctrlList <- list(convertTolower=c(TRUE, 3),
                 verbose=TRUE,
                 convertToASCII=TRUE,
                 removePunct=TRUE,
                 removeNumbers=TRUE,
                 removeStopWords=c(FALSE, NULL))

source('createVocab.R')
#
(elapsed <- Sys.time() - strt)

# --- Encode training data
cat('>>> Encoding training data. \n')
pause('Proceed?')
# Script continues.

# Encoding data in steps 
recLimit = as.integer(length(trData) * REC_LIMIT)  
strt <- Sys.time()  
stepLst <- step(x=trData[1:recLimit], fun=lapply, steps=nCode_STEPS, nCode)
trData_n <- unlist(stepLst, recursive=F, use.names=F)
(elapsed <- Sys.time() - strt)

# --- bigrams
cat('>>> Creating bigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, n = 2, fun = ngramN, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n2g <- ngramLst[[1]]
n2gp <- ngramLst[[2]]
T2 <- length(n2g)        # Number of Types
N2 <- sum(n2g)           # Number of tokens
n2g_df <- ngram.DF(n2g)                   

# --- trigrams
cat('>>> Creating trigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, n = 3, fun = ngramN, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n3g <- ngramLst[[1]]
n3gp <- ngramLst[[2]]
T3 <- length(n3g)    # Number of Types
N3 <- sum(n3g)       # Number of tokens
n3g_df <- ngram.DF(n3g)

# --- skip 2-grams
cat('>>> Creating skip bigrams. \n')
ctrlList <- list(convertTolower=c(TRUE, 3),
                 verbose=TRUE,
                 convertToASCII=TRUE,
                 removePunct=TRUE,
                 removeNumbers=TRUE,
                 removeStopWords=c(TRUE, myStopWords))

cat('>>> Recreating vocab without stop words. \n')
source('createVocab.R')

ngramLst <- Ngram.tf(x = trData_n, n = 2, fun = skip.ngram, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n2s <- ngramLst[[1]]
n2sp <- ngramLst[[2]]
T2s <- length(n2s)        # Number of Types
N2s<- sum(n2s)            # Number of tokens
n2s_df <- ngram.DF(n2s) 

# End of script 

