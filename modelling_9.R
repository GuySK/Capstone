#'
#'
#' Capstone Project
#' 
#' Task 3 - Modeling - 9: blogs data set
#' Model based on n-grams. 
#'
#'
# ---

# --------                            ------------------------------------------------
# Settings                            ---> Review Carefully before running script <---
# --------                            ------------------------------------------------
AWS = FALSE;                          # Computer running script: AWS or own
DSET = 1;                             # Blogs=1, News = 2, Tweets=3
TRAINPROP = 0.60;                     # Proportion of data set to be used as training set
SEED_N = 191;                         # Seed for random operations
nCode_STEPS = 10;                     # Number of steps to encode training data
REC_LIMIT = 1;                        # Proportion of records to process in this run
EXPRS_N = 100000;                     # limit on number of nested expr to be evaluated
ngram_THRESH = 2;                     # Min frequency of ngrams to keep
CHUNK_PROP = 0.1;                     # Default chunk size for chopping function
UNK_LMT = 0.005;                      # quantile limit to replace for <UNK>
VOCAB = 'WRDS_H'                      # Name of hash table used as Vocab
VOCAB_INV = 'WRDS_H_INV'              # Name of inverted hash table vocabulary
TOT_RUNS = 100;                       # Total number of training runs
RUN_NR = 6;                           # Current run number       
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
cat('    VOCAB =', VOCAB, '\n')
cat('    VOCAB_INV =', VOCAB_INV, '\n')
cat('    TOT_RUNS =', TOT_RUNS, '\n')
cat('    RUN_NR =', RUN_NR, '\n')

# --- Check if Params ok
pause("Continue?")     

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
source("./AWS/ngram2.R")
source("./AWS/skip_ngram2.R")
source("./AWS/nCodeNgram.R")

# --- increase options setting for recursive functions
options(expressions=EXPRS_N)

# ---
sourceDir = './DataSets'
LANG = 'eng'
cat(">>> Loading corpus. \n")

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

# --- Creating / updating training data set
if(RUN_NR == 1) {
    set.seed(SEED_N)
    train <- sample(1:length(dset), length(dset) * TRAINPROP)
    trSet <- dset[train]    
}

RunSize <- length(trSet) %/% TOT_RUNS
strt_rec <- 1 + (RUN_NR - 1) * RunSize
end_rec <- RUN_NR * RunSize
trData <- trSet[strt_rec:end_rec]

cat('\n>>> Run Nr: ', RUN_NR, '. From Rec ', 
    strt_rec, ' To ', end_rec, '. \n\n', sep='')

# --- Cleaning training set
ctrlList <- list(convertTolower=c(TRUE, 3),
                 verbose=TRUE,
                 convertToASCII=TRUE,
                 removePunct=TRUE,
                 removeNumbers=TRUE,
                 removeStopWords=c(FALSE, NULL))

trData <- cleanDoc(x=trData, control=ctrlList)

# --- 
strt <- Sys.time()
action <- 'Updating'
if(RUN_NR == 1)
    action <- 'Creating'
cat('>>> ', action, 'Vocabulary. \n')

# --- Creating word list
ngramLst <- Ngram.tf(x = trData,
                     fun = ngram2,
                     n = 1,
                     encoded = FALSE,
                     encode = FALSE,
                     threshold = 2, 
                     chunkSize = 0.1)

n1g <- ngramLst[[1]]
n1gp <- ngramLst[[2]]
T1 <- length(n1g)    # Number of Types
N1 <- sum(n1g)       # Number of tokens

# --- Replacing unfrequent terms as unknown
cat('    Replacing unfrequent terms as unknown. \n')

if (UNK_LMT > 0) {
    qlmt <- qxdist(n1g, UNK_LMT)                            # quantile computed here
} else {
    qlmt <- 0                                               # no replacements
}

n1g <- n1g[order(n1g)]                                      # sort words in asc order 
n1g_u <- n1g[as.integer(qlmt * length(n1g) +1):length(n1g)] # get rid of low frq terms
unkcount <- as.integer(UNK_LMT * sum(n1g))                  # compute <UNK> count
n1g_u <- append(unkcount, n1g_u)                            # add corresponding count
names(n1g_u)[1] <- '<UNK>'                                  # add name as <UNK>
n1g_u <- n1g_u[order(n1g_u)]                                # restore order

# --- Creating / updating vocab 
cat('    Creating / updating hash table VOCAB. \n')

WRDS <- names(n1g_u[order(n1g_u, decreasing=T)])            # word character vector  
if(RUN_NR == 1){                                            # Create Vocab from scratch
    WRDS_H <- hash(WRDS, 1:length(WRDS))
    WRDS_H_INV <- hash(values(WRDS_H), keys(WRDS_H))
    cat('    New Vocab created with', length(WRDS), 'types. \n')
} else {                                                    # Update existing Vocab
    new_words <- updt.Vocab(WRDS)
    cat('    Vocab updated with', new_words, 'new types. \n')
}
(elapsed <- Sys.time() - strt)
# --- End of Vocab creation / update

# --- Encode training data
cat('>>> Encoding training data. \n')

# Encoding data in steps
recLimit = as.integer(length(trData) * REC_LIMIT)  
strt <- Sys.time()  

stepLst <- step(x=trData[1:recLimit], 
                fun=lapply, 
                steps=nCode_STEPS, 
                nCode2)

trData_n <- unlist(stepLst, recursive=F, 
                   use.names=F)
(elapsed <- Sys.time() - strt)

# --- words
cat('>>> Creating 1-grams. \n')
ngramLst <- Ngram.tf(x = trData,                    # input data
                     n = 1,                         # ngram type
                     fun = ngram2,                  # ngram generator function
                     encoded = FALSE,               # input not encoded
                     encode = TRUE,                 # encode ngram
                     threshold = ngram_THRESH,      # discard ngrams with low frequency
                     chunkSize = CHUNK_PROP)        # chunk size to process by steps

n1gn <- ngramLst[[1]]                               # extract ngram dict from lst
n1gnp <- ngramLst[[2]]                              # and ngram probabilities
T1n <- length(n1gn)                                 # Number of Types
N1n <- sum(n1gn)                                    # Number of tokens

if (RUN_NR == 1) {
    n1gn_df <- ngram.DF(n1gn_df, n1gn)              # create data frame    
} else {
    res <- updt.DF(n1gn_df, n1gn)                   # update data frame
    n1gn_df <- res$df            
    n1gn_added <- res$added
    n1gn_updted <- res$updted
}

# --- bigrams
cat('>>> Creating bigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, 
                     n = 2, 
                     fun = ngram2,
                     encoded = TRUE,
                     encode = TRUE,
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)

n2gn <- ngramLst[[1]]
n2gnp <- ngramLst[[2]]
T2n <- length(n2gn)        # Number of Types
N2n <- sum(n2gn)           # Number of tokens

if (RUN_NR == 1) {
    n2gn_df <- ngram.DF(n2gn)                       # create data frame    
} else {
    res <- updt.DF(n2gn_df, n2gn)                   # update data frame
    n2gn_df <- res$df
    n2gn_added <- res$added
    n2gn_updted <- res$updted
}               

# --- trigrams
cat('>>> Creating trigrams. \n')
ngramLst <- Ngram.tf(x = trData_n, 
                     n = 3, 
                     fun = ngram2,
                     encoded = TRUE,
                     encode = TRUE,
                     threshold = ngram_THRESH, 
                     chunkSize = CHUNK_PROP)

n3gn <- ngramLst[[1]]
n3gnp <- ngramLst[[2]]
T3n <- length(n3gn)    # Number of Types
N3n <- sum(n3gn)       # Number of tokens

if (RUN_NR == 1) {
    n3gn_df <- ngram.DF(n3gn)                       # create data frame
} else {
    res <- updt.DF(n3gn_df, n3gn)                   # update data frame
    n3gn_df <- res$df
    n3gn_added <- res$added
    n3gn_updted <- res$updted
}

# --- skip 2-grams
cat('>>> Creating skip bigrams. \n')

cat('    Removing stopwords. \n')
myStopWords_n <- nCode2(myStopWords)                         
trData_n_nstp <- lapply(trData_n, 
                        function(x, rmList) {x[!(x %in% rmList)]}, 
                        myStopWords_n)

cat('    creating skip bigrams. \n')
ngramLst <- Ngram.tf(x = trData_n_nstp,               # input data        
                     n = 2,                           # bigrams
                     fun = skip.ngram2,               # use skip function
                     window = 3,                      # window size for skip bigrams
                     encoded = TRUE,                  # data is already encoded
                     encode = TRUE,                   # encode bigrams
                     threshold = ngram_THRESH,        # discard low frequency ngrams 
                     chunkSize = CHUNK_PROP)          # process data in chunks 

n2sn <- ngramLst[[1]]
n2snp <- ngramLst[[2]]
T2sn <- length(n2sn)        
N2sn<- sum(n2sn)            

if (RUN_NR == 1) {
    n2sn_df <- ngram.DF(n2sn)                       # create data frame
} else {
    res <- updt.DF(n2sn_df, n2sn)                   # update data frame
    n2sn_df <- res$df
    n2sn_added <- res$added
    n2sn_updted <- res$updted
}           

# --- End of process data
cat('\n>>> Run Nr. ', RUN_NR, ' completed OK. (Recs ', 
    strt_rec, ' - ', end_rec, '). \n', sep='')

cat('Types added: ', n1gn_added,
    '. Updated: ', n1gn_updted,
    '. Total: ', nrow(n1gn_df), '. \n', sep='')

cat('Bigrams added: ', n2gn_added,
    '. Updated: ', n2gn_updted,
    '. Total: ', nrow(n2gn_df), '. \n', sep='')

cat('Trigrams added: ', n3gn_added,
    '. Updated: ', n3gn_updted,
    '. Total: ', nrow(n3gn_df), '. \n', sep='')

cat('Skip Bigrams added: ', n2sn_added,
    '. Updated: ', n2sn_updted,
    '. Total: ', nrow(n2sn_df), '. \n', sep='')

# --- End of script 
