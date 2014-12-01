#'
#'
#' Capstone Project
#' 
#' Task 3 - Modeling - 5: blogs data set
#' Model based on n-grams. 
#'
#'
# ---

cat(">>> Basic setup. \n")
setwd("~/gsk1")

source("./eda1_auxFunctions.R")
source("./AuxFunctions.R")
source("cleanDoc.R")
source("Ngram_tf.R")

library(tm)

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

# ---
# Comment / uncomment corresponding line
DSET = 1;    # Blogs.
# DSET = 2;  # News.
# DSET = 3;  # Tweets.
#

# Proportion of data set to be used as training set
TRAINPROP = 0.5;

# --- Getting data set and training data
cat('>>> Getting data set and training data. \n')
dset <- crp[[DSET]]
dset <- unlist(dset[1]) 

if (file.access('Corpus_en', 0) == -1)
  save(crp, file='Corpus_en')
rm(crp); gc()

# --- Creating Vocabulary
set.seed(191)
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
pause()
# Script continues.

recLimit = length(trData);                    # all data
strt <- Sys.time()
trData_n <- sapply(trData[1:recLimit], nCode) # encode data as numeric
(elapsed <- Sys.time() - strt)
names(trData_n) <- NULL                       # get rid of annoying names in list

# --- increase options setting for recursive functions
options(expressions=100000)

# --- bigrams
ngramLst <- Ngram.tf(x = trData_n, n = 2, fun = ngramN, 
                     threshold = 2, chunkSize = 0.1)
#
n2g <- ngramLst[[1]]
n2gp <- ngramLst[[2]]
T2 <- length(n2g)    # Number of Types
N2 <- sum(n2g)       # Number of tokens
n2g_df <- ngram.DF(n2g)                   

# --- trigrams
ngramLst <- Ngram.tf(x = trData_n, n = 3, fun = ngramN, 
                     threshold = 2, chunkSize = 0.1)
#
n3g <- ngramLst[[1]]
n3gp <- ngramLst[[2]]
T3 <- length(n3g)    # Number of Types
N3 <- sum(n3g)       # Number of tokens
n3g_df <- ngram.DF(n3g)

# --- skip 2-grams
ctrlList <- list(convertTolower=c(TRUE, 3),
                 verbose=TRUE,
                 convertToASCII=TRUE,
                 removePunct=TRUE,
                 removeNumbers=TRUE,
                 removeStopWords=c(TRUE, 'en'))
source('createVocab.R')

ngramLst <- Ngram.tf(x = trData_n, n = 2, fun = skip.ngram, 
                     threshold = 2, chunkSize = 0.1)
#
n2s <- ngramLst[[1]]
n2sp <- ngramLst[[2]]
T2s <- length(n2s)    # Number of Types
N2s<- sum(n2s)       # Number of tokens
n2s_df <- ngram.DF(n2s) 


