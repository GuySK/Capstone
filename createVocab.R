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

# --- Replacing unfrequent terms as unknown
cat('>>> Replacing unfrequent terms as unknown. \n')
q003 <- qxdist(n1g, 0.03) # 3% quantile computed here
n1g <- n1g[order(n1g)]    # sort words in counts asc order 
n1g_u <- n1g[as.integer(q003 * length(n1g)):length(n1g)]    # get rid of terms of low freq
n1g_u <- append(0.03 * sum(n1g), n1g_u)                     # add corresponding count
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


