#'
#'
#' Capstone Project
#' 
#' Test n-gram Model 
#' Validation of n-gram model
#'
#'
# ---

# --------                            ------------------------------------------------
# Settings                            ---> Review Carefully before running script <---
# --------                            ------------------------------------------------
AWS = FALSE;                          # Computer running script: AWS or own
DSET = 1;                             # Blogs=1, News = 2, Tweets=3
TRAINPROP = 0.50;                     # Proportion of data set to be used as training set
VALPROP =   0.50;                     # Prop of NOT training data set used for validation purposes
TRAIN_DATA = 'trainVector';           # Name of vector with training data
DATA_STR = 'dataStructures';          # Name of data structures
PRD_RESULTS = 'prd_val_df32';         # Data frame accumulating predictions
SEED_N = 191;                         # Seed for random operations
EXPRS_N = 100000;                     # limit on number of nested expr to be evaluated
VOCAB = 'WRDS_H';                     # Name of hash table used as Vocab
VOCAB_INV = 'WRDS_I';                 # Name of inverted hash table vocabulary
RUN_NR = 1;                           # Current run number       
FROM_REC = 1;                         # Process from this point on. Use last rec if zero. 
N_RECS =  100;                        # Number of records to process.
ACUM_RESULTS = TRUE;                  # add run results to data frame?
SAVE_ENV = FALSE;                     # Save environment when done?
LOAD_RESULTS = FALSE;                 # Load results from disk?
SAVE_RESULTS = FALSE;                 # Save results data frame to disk?
N_PREDS = 3;                          # Number of words to predict
# --------                            ------------------------------------------------

cat('>>> Script settings. \n')
cat('    AWS =', AWS, '\n')
cat('    DSET =', DSET, '\n')
cat('    TRAINPROP =', TRAINPROP, '\n')
cat('    VALPROP =', VALPROP, '\n')
cat('    TRAIN_DATA =', TRAIN_DATA, '\n')
cat('    DATA_STR =', DATA_STR, '\n')
cat('    PRD_RESULTS =', PRD_RESULTS, '\n')
cat('    SEED_N =', SEED_N, '\n')
cat('    EXPRS_N =', EXPRS_N, '\n')
cat('    VOCAB =', VOCAB, '\n')
cat('    VOCAB_INV =', VOCAB_INV, '\n')
cat('    RUN_NR =', RUN_NR, '\n')
cat('    FROM_REC =', FROM_REC, '\n')
cat('    N_RECS =', N_RECS, '\n')
cat('    ACUM_RESULTS =', ACUM_RESULTS, '\n')
cat('    SAVE_ENV =', SAVE_ENV, '\n')
cat('    LOAD_RESULTS =', LOAD_RESULTS, '\n')
cat('    SAVE_RESULTS =', SAVE_RESULTS, '\n')
cat('    N_PREDS =', N_PREDS, '\n')

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
source("./AWS/predictFuncts_2.R")

# --- Check if Run Nr and Params ok
if (exists('LAST_RUN')){
    cat('>>> Last RUN Found is ', LAST_RUN, 
        '. Current RUN Nr is: ', RUN_NR, '.\n', sep='')
    } else {
        cat('>>> No LAST_RUN found. \n')    
}
pause("Continue?")     

# Checking for necessary data structures
data_needed <- list('n1gn_df', 'n2gn_df', 'n3gn_df', 'WRDS_H', 'WRDS_I')
if (!all(data_needed %in% ls())){
    cat('>>> Some necessary data structures are missing. \n')
    cat('    Trying to load them from disk. \n')
    if (!file.exists(DATA_STR)){
        stop('ERROR. Data structures not found.')
    } else {
        load(file = DATA_STR)
    }
}

# --- increase options setting for recursive functions
options(expressions=EXPRS_N)

# --- Getting data set and validation data
sourceDir = './DataSets'
LANG = 'eng'
LOAD_CORP = TRUE

if (exists('valSet')){
    cat('>>> Using existing data set for testing. \n')
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

    cat('>>> Getting data set and validation data. \n')
    dset <- crp[[DSET]]
    if (AWS)                                    
        dset <- unlist(dset[1]) 
    if (file.access('Corpus_en', 0) == -1)
        save(crp, file='Corpus_en')
    rm(crp); gc()
}

# --- Creating / updating validation data set
if(RUN_NR == 1 || !exists('valSet')) {
    set.seed(SEED_N)
    load(file = TRAIN_DATA)
    tstSet <- dset[-train]
    validation <- sample(1:length(tstSet), length(tstSet) * VALPROP)
    valSet <- dset[validation]
}

if (LOAD_RESULTS){
    cat('>>> Loading Results data frame from disk. \n')
    load(file = PRD_RESULTS)
}

if (FROM_REC == 0) {
    FROM_REC <- end_rec + 1
}

strt_rec <- FROM_REC
end_rec <- strt_rec + N_RECS - 1
cat('>>> Current run from rec nr ', strt_rec, ' to ', end_rec, '. \n', sep='')

# --- Predicting Sentences
cat('>>> Predicting Sentences. \n')
prd_df <- predictSentences(valSet[strt_rec:end_rec], 
                           maxwords = N_PREDS, 
                           talk = FALSE,
                           heartbeat = TRUE)
run_acc <- get_accuracy(prd_df)
cat('    Accuracy of current run is', run_acc, '\n')

if (ACUM_RESULTS){
    if (exists(PRD_RESULTS)){
        assign(PRD_RESULTS, rbind(get(PRD_RESULTS), prd_df))
        cat('    Total accuracy is', get_accuracy(get(PRD_RESULTS)), '\n')
    } else {
        assign(PRD_RESULTS, prd_df)
    }
}

if (SAVE_RESULTS){
    cat('>>> Saving Results data frame to disk. \n')
    save(list = c(PRD_RESULTS, 'end_rec'), file = PRD_RESULTS)
}

# --- Save run and environment
LAST_RUN <- RUN_NR
if (SAVE_ENV) {
    env_name <- paste0('./val3_env_run_', RUN_NR, '.RData')
    cat('>>> Saving environment as ', env_name, 
        '. This may take some time. \n', sep='')
    save.image(env_name)    
} else {
    cat('>>> WARNING. Environment will not be saved.\n')
}

# --- End of script 
