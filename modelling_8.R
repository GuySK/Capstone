#'
#'
#' Capstone Project
#' 
#' Task 3 - Modeling - 8: blogs data set
#' Model based on n-grams. 
#'
#'
# ---

# --------                            ------------------------------------------------
# Settings                            ---> Review Carefully before running script <---
# --------                            ------------------------------------------------
AWS = FALSE;                          # Computer running script: AWS or own
DSET = 1;                             # Blogs=1, News = 2, Tweets=3
TRAINPROP = 0.01;                     # Proportion of data set to be used as training set
SEED_N = 191;                         # Seed for random operations
nCode_STEPS = 10;                     # Number of steps to encode training data
REC_LIMIT = 1;                        # Proportion of records to process in this run
EXPRS_N = 100000;                     # limit on number of nested expr to be evaluated
ngram_THRESH = 2;                     # Min frequency of ngrams to keep
CHUNK_PROP = 0.1;                     # Default chunk size for chopping function
UNK_LMT = 0.005;                      # quantile limit to replace for <UNK>
CREATE_VOCAB = TRUE;                  # Create new Vocabulary from scratch?
VOCAB = 'WRDS_H'                      # Name of hash table used as Vocab
VOCAB_INV = 'WRDS_H_INV'              # Name of inverted hash table vocabulary
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
cat('    CREATE_VOCAB =', CREATE_VOCAB, '\n')
cat('    VOCAB =', VOCAB, '\n')
cat('    VOCAB_INV =', VOCAB_INV, '\n')

# ---
cat(">>> Basic setup. \n")
if (AWS) {
    setwd("~/gsk1")
} else {
    setwd("~/../Google Drive 2/Training/DataScience/CapstonePj")
}

library(tm)
library(hash)

source("./AWS/eda1_auxFunctions.R")
source("./AWS/AuxFunctions.R")
source("./AWS/trnsFuncts.R")
source("./AWS/Ngram_tf.R")

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

# --- Creating or updating Vocabulary
cat('>>> Creating / updating Vocabulary. \n')
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

source('./AWS/createVocab.R')
#
(elapsed <- Sys.time() - strt)

# --- Encode training data
cat('>>> Encoding training data. \n')
pause('Proceed?')
# Script continues.

# Encoding data in steps
nCode_STEPS = 1; # override global parameter
recLimit = as.integer(length(trData) * REC_LIMIT)  
strt <- Sys.time()  
stepLst <- step(x=trData[1:recLimit], fun=lapply, steps=nCode_STEPS, nCode2)
trData_n <- unlist(stepLst, recursive=F, use.names=F)
(elapsed <- Sys.time() - strt)

# --- words
cat('>>> Creating 1-grams. \n')
ngramLst <- Ngram.tf(x = trData_n, n = 1, fun = ngramN, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n1gn <- ngramLst[[1]]
n1gnp <- ngramLst[[2]]
T1n <- length(n1gn)        # Number of Types
N1n <- sum(n1gn)           # Number of tokens
n1gn_df <- ngram.DF(n1gn)                   

# --- bigrams
cat('>>> Creating bigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, n = 2, fun = ngramN, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n2gn <- ngramLst[[1]]
n2gnp <- ngramLst[[2]]
T2n <- length(n2gn)        # Number of Types
N2n <- sum(n2gn)           # Number of tokens
n2gn_df <- ngram.DF(n2gn)                   

# --- trigrams
cat('>>> Creating trigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, n = 3, fun = ngramN, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)
#
n3gn <- ngramLst[[1]]
n3gnp <- ngramLst[[2]]
T3n <- length(n3gn)    # Number of Types
N3n <- sum(n3gn)       # Number of tokens
n3gn_df <- ngram.DF(n3gn)

# --- skip 2-grams
cat('>>> Creating skip bigrams. \n')

cat('    Removing stopwords. \n')
myStopWords_n <- nCode2(myStopWords)                         
trData_n_nstp <- lapply(trData_n, 
                        function(x, rmList) {x[!(x %in% rmList)]}, 
                        myStopWords_n)

cat('    creating skip bigrams. \n')
ngramLst <- Ngram.tf(x = trData_n_nstp, n = 2, 
                     fun = skip.ngram, 
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)

n2sn <- ngramLst[[1]]
n2snp <- ngramLst[[2]]
T2sn <- length(n2sn)        # Number of Types
N2sn<- sum(n2sn)            # Number of tokens
n2sn_df <- ngram.DF(n2sn) 

# End of script 

