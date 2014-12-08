# Vocab Creation script
# Assumes data set in variable 'dset'

# --- Cleaning training set

trData <- dset[train]
trData <- cleanDoc(x=trData, control=ctrlList)

# --- Creating word list
ngramLst <- Ngram.tf(x = trData, n = 1, threshold = 2, chunkSize = 0.1)
n1g <- ngramLst[[1]]
n1gp <- ngramLst[[2]]
T1 <- length(n1g)    # Number of Types
N1 <- sum(n1g)       # Number of tokens
# n1d <- hash(names(n1g), n1g) # convert to dict format

# --- Replacing unfrequent terms as unknown
cat('>>> Replacing unfrequent terms as unknown. \n')
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

# --- Creating hash table
cat('>>> Creating hash table. \n')
if(ctrlList$removeStopWords[1]) {
  WRDS_NS <- new.env(hash=T)                                  # create hash table
  WRDS_NS <- names(n1g_u[order(n1g_u, decreasing=T)])         # add elements to table
} else {
  WRDS <- new.env(hash=T)                                     # create hash table
  WRDS <- names(n1g_u[order(n1g_u, decreasing=T)])            # add elements to table  
}

cat('>>> End of script --createVocab--. \n')
# --- End of Script


