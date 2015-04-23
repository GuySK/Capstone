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
EXPRS_N = 120000;                     # limit on number of nested expr to be evaluated
ngram_THRESH = 1;                     # Min frequency of ngrams to keep
CHUNK_PROP = 0.1;                     # Default chunk size for chopping function
UNK_LMT = 0.000;                      # quantile limit to replace for <UNK>
VOCAB = 'WRDS_H';                     # Name of hash table used as Vocab
VOCAB_INV = 'WRDS_I';                 # Name of inverted hash table vocabulary
TOT_RUNS = 100;                       # Total number of training runs
SAVE_ENV = FALSE;                      # Save environment when done?
RUN_NR = 23;                          # Current run number       
FROM_REC =  end_rec + 1;              # Process from this point on. Defined by Run Nr if 0.
N_RECS =  20000;                      # Used if FROM_REC not zero.
COMPACT_BIGRAMS = FALSE;              # To get rid of low frequency terms
COMPACT_TRIGRAMS = FALSE;             # To get rid of low frequency terms
# --------                           ------------------------------------------------

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
    run_end_rec <- min(run_end_rec, length(trSet))          # do not go past eof
    ERR_rec_range = ifelse(strt_rec <= 0, T, F)             # check start rec is valid
    ERR_rec_range = ifelse(strt_rec > run_end_rec, T, F)    # and starting point beyond limits
}

if (ERR_rec_range){                                         # check range 
    stop('\n>>> Record range is wrong. Run Nr: ',             # something's wrong with it
         RUN_NR, '. From Rec ', strt_rec,                     # cancel run
         ' To ', run_end_rec, '. \n\n', sep='')    
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

# --- bigrams
cat('>>> Creating bigrams. \n')
n2gn <- Ngram.tf(x = trData_n, 
                 n = 2, 
                 fun = ngram2,
                 encoded = TRUE,
                 encode = TRUE,
                 threshold = ngram_THRESH, 
                 chunkSize = CHUNK_PROP)

if (RUN_NR == 1) {
    n2gn_df <- ngram.DF(n2gn)                       # create data frame    
    n2gn_added <- length(n2gn)
    n2gn_updted <- 0    
} else {
    res <- updt.DF(n2gn_df, n2gn)                   # update data frame
    n2gn_df <- res$df                               # get data frame
    n2gn_added <- res$added                         # terms added
    n2gn_updted <- res$updted                       # and updated
    n2gn_df$Prob <- getProb(n2gn_df$Count)          # update probabilities
    n2gn_df$ProbWB <- getProbWB(n2gn_df$Count)      # update Witten Belt discounted probs
}               

# --- trigrams
cat('>>> Creating trigrams. \n')
n3gn <- Ngram.tf(x = trData_n, 
                 n = 3, 
                 fun = ngram2,
                 encoded = TRUE,
                 encode = TRUE,
                 threshold = ngram_THRESH, 
                 chunkSize = CHUNK_PROP)

if (RUN_NR == 1) {
    n3gn_df <- ngram.DF(n3gn)                       # create data frame
    n3gn_added <- length(n3gn)
    n3gn_updted <- 0
} else {
    res <- updt.DF(n3gn_df, n3gn)                   # update data frame
    n3gn_df <- res$df                               # get data frame
    n3gn_added <- res$added                         # statistics. terms added
    n3gn_updted <- res$updted                       # and updated
    n3gn_df$Prob <- getProb(n3gn_df$Count)          # update probabilities
    n3gn_df$ProbWB <- getProbWB(n3gn_df$Count)      # update Witten Belt discounted probs
}

# --- End of process data
cat('\n>>> Run Nr. ', RUN_NR, ' completed OK. (Recs ', 
    strt_rec, ' - ', end_rec, '). \n', sep='')

cat('Bigrams added: ', n2gn_added,
    '. Updated: ', n2gn_updted,
    '. Total: ', nrow(n2gn_df), '. \n', sep='')

cat('Trigrams added: ', n3gn_added,
    '. Updated: ', n3gn_updted,
    '. Total: ', nrow(n3gn_df), '. \n', sep='')

# Compacting ngram data frames
if (COMPACT_BIGRAMS){
    cat('\n>>> Compacting 2-gram data frame. \n')
    n2gn_df <- compactNgramDF(df = n2gn_df, thresHold = 1, talk=T)
}

if (COMPACT_TRIGRAMS){
    cat('\n>>> Compacting 3-gram data frame. \n')
    n3gn_df <- compactNgramDF(df = n3gn_df, thresHold = 1, talk=T)
}

# --- Save run and environment
LAST_RUN <- RUN_NR
if (SAVE_ENV) {
    env_name <- paste0('./n23gn_env_run_', RUN_NR, '.RData')
    cat('Saving environment as ', env_name, 
        '. This may take some time. \n', sep='')
    save.image(env_name)    
} else {
    cat('WARNING. Environment will not be saved.\n')
}

# --- End of script 
