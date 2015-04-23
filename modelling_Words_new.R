#'
#'
#' Capstone Project
#' 
#' Task 3 - Modeling - version C: blogs data set
#' Model based on n-grams. 
#'
#'
# ---

# --------                            ------------------------------------------------
# Settings                            ---> Review Carefully before running script <---
# --------                            ------------------------------------------------
AWS = FALSE;                          # Computer running script: AWS or own
DSET = 1;                             # Blogs=1, News = 2, Tweets=3
TRAINPROP = 0.50;                     # Proportion of data set to be used as training set
SEED_N = 191;                         # Seed for random operations
nCode_STEPS = 10;                     # Number of steps to encode training data
REC_LIMIT = 1;                        # Proportion of records to process in this run
EXPRS_N = 100000;                     # limit on number of nested expr to be evaluated
ngram_THRESH = 1;                     # Min frequency of ngrams to keep
CHUNK_PROP = 0.1;                     # Default chunk size for chopping function
UNK_LMT = 0.000;                      # quantile limit to replace for <UNK>
VOCAB = 'WRDS_H';                     # Name of hash table used as Vocab
VOCAB_INV = 'WRDS_I';                 # Name of inverted hash table vocabulary
TOT_RUNS = 100;                       # Total number of training runs
SAVE_ENV = TRUE;                      # Save environment when done?
RUN_NR = 22;                          # Current run number       
FROM_REC =  end_rec + 1;              # Process from this point on. Defined by Run Nr if 0.
N_RECS =  29644;                      # Used if FROM_REC not zero.
COMPACT_VOCAB = TRUE;                 # To get rid of low frequency terms
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
cat('    UNK_LMT =', UNK_LMT, '\n')
cat('    VOCAB =', VOCAB, '\n')
cat('    VOCAB_INV =', VOCAB_INV, '\n')
cat('    TOT_RUNS =', TOT_RUNS, '\n')
cat('    SAVE_ENV =', SAVE_ENV, '\n')
cat('    RUN_NR =', RUN_NR, '\n')
cat('    FROM_REC =', FROM_REC, '\n')
cat('    N_RECS =', N_RECS, '\n')

# ---
cat("\n>>> Setting up environment and directories. \n")

if (AWS) {
    setwd("~/gsk1")
} else {
    setwd("~/../Google Drive 2/Training/DataScience/CapstonePj")
}

cat("    Running on AWS: ", AWS, '.\n', sep = '')
cat("    Current dir is: ", getwd(), '.\n', sep = '')

library(tm)
library(hash)

source("./AWS/eda1_auxFunctions.R")
source("./AWS/AuxFunctions.R")
source("./AWS/trnsFuncts.R")
source("./AWS/Ngram_tf.R")
source("./AWS/ngram2.R")
source("./AWS/skip_ngram2.R")
source("./AWS/nCodeNgram.R")

# --- Check if Run Nr and Params ok
if (exists('LAST_RUN'))
    cat('>>> Last RUN Found is ', LAST_RUN, 
        '. Current RUN Nr is: ', RUN_NR, '.\n', sep='')
pause("Continue?")     

# --- increase options setting for recursive functions
options(expressions=EXPRS_N)

# --- Getting data set and training data
sourceDir = './DataSets'
LANG = 'eng'
LOAD_CORP = TRUE

if (exists('trSet')){
    cat('>>> Using existing data set for training. \n')
    LOAD_CORP = FALSE
}

if (LOAD_CORP){
    cat(">>> Loading corpus. \n")
    
    if (file.access('Corpus_en', 4) == 0) {
        cat('>>> Loading object from disk. \n')
        load('Corpus_en')
    } else {
        cat('>>> Creating Corpus from source directory. \n')
        crp <- Corpus(DirSource(sourceDir, encoding = "UTF-8"), 
                      readerControl = list(language = LANG))
    }   

    cat('>>> Getting data set and training data. \n')
    dset <- crp[[DSET]]
    if (AWS)                                    
        dset <- unlist(dset[1]) 
    if (file.access('Corpus_en', 0) == -1)
        save(crp, file='Corpus_en')
    rm(crp); gc()
}

# --- Creating / updating training data set
if(RUN_NR == 1) {
    set.seed(SEED_N)
    train <- sample(1:length(dset), length(dset) * TRAINPROP)
    trSet <- dset[train]    
}

RunSize <- length(trSet) %/% TOT_RUNS                       # Size of chunk of data to train
if (FROM_REC == 0) {
    strt_rec <- 1 + (RUN_NR - 1) * RunSize                  # Determine first record in chunk
    run_end_rec <- RUN_NR * RunSize                         # and final record
} else {
    strt_rec <- FROM_REC
    run_end_rec <- strt_rec + N_RECS - 1
}

trData <- trSet[strt_rec:run_end_rec]                       # set up data to train

cat('\n>>> Run Nr: ', RUN_NR, '. From Rec ',                # Log it
    strt_rec, ' To ', run_end_rec, '. \n\n', sep='')
pause('Continue?')                                          # Wait for ok
end_rec <- run_end_rec                                      # if ok update end record indicator

# --- Cleaning training set                            
ctrlList <- list(convertTolower=c(TRUE, 3),                 # data cleaning parameters
                 verbose=TRUE,
                 convertToASCII=TRUE,
                 removePunct=TRUE,
                 removeNumbers=TRUE,
                 removeStopWords=c(FALSE, NULL))

trData <- cleanDoc(x=trData, control=ctrlList)             # Clean data

# --- Vocabulary creation or update
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
                     threshold = ngram_THRESH, 
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
cat('    Creating / updating hash table ', VOCAB, '. \n', sep='')

WRDS <- names(n1g_u[order(n1g_u, decreasing=T)])            # word character vector  
if(RUN_NR == 1){                                            # Create Vocab from scratch
    WRDS_H <- hash(WRDS, 1:length(WRDS))
    WRDS_I <- invertVocab(WRDS_H)
    cat('    New Vocab created with', length(WRDS), 'types. \n')
} else {                                                    # Update existing Vocab
    # new_words <- updt.Vocab(WRDS)
    updtList <- updt.Vocab(WRDS, Vocab_inv = WRDS_I)        # update vocab and inv vocab
    new_words <- updtList[[1]]                              # get nr of words added
    if (new_words > 0)
        WRDS_I <- updtList[[2]]                             # and inverted Vocab vector
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
n1gnp <- ngramLst[[2]]   
# and ngram probabilities
T1n <- length(n1gn)                                 # Number of Types
N1n <- sum(n1gn)                                    # Number of tokens

if (RUN_NR == 1) {
    n1gn_df <- ngram.DF(n1gn)                       # create data frame
    n1gn_added <- length(n1gn)                      # term statistics. terms added.
    n1gn_updted <- 0                                # no terms updated
} else {
    res <- updt.DF(n1gn_df, n1gn)                   # update data frame
    n1gn_df <- res$df                               # get data frame
    n1gn_added <- res$added                         # total terms added
    n1gn_updted <- res$updted                       # total terms updated
    n1gn_df$Prob <- getProb(n1gn_df$Count)          # update probabilities
    n1gn_df$ProbWB <- getProbWB(n1gn_df$Count)      # update Witten Belt discounted probs
}

# --- End of process data
cat('\n>>> Run Nr. ', RUN_NR, ' completed OK. (Recs ', 
    strt_rec, ' - ', end_rec, '). \n', sep='')

cat('Types added: ', n1gn_added,
    '. Updated: ', n1gn_updted,
    '. Total: ', nrow(n1gn_df), '. \n', sep='')

# Compacting Vocabs
if (COMPACT_VOCAB){
    cat('\n>>> Compacting Vocabulary and 1-gram data frame. \n')
    cList <- compactWords(df = n1gn_df, thresHold = 1, talk=T)
    n1gn_df <- cList[[1]]
    WRDS_H <- cList[[2]]
    WRDS_I <- cList[[3]]
}

if (length(WRDS_H) > 1e+5)
    cat('WARNING. Vocab is >1e+5 and should be compacted. \n')

# --- Save run and environment
LAST_RUN <- RUN_NR
if (SAVE_ENV) {
    env_name <- paste0('./n1gn_env_run_', RUN_NR, '.RData')
    cat('Saving environment as ', env_name, 
        '. This may take some time. \n', sep='')
    save.image(env_name)    
} else {
    cat('WARNING. Environment will not be saved.\n')
}

# --- End of script 
