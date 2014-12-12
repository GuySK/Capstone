# Vocab Creation script
# Assumes data set in variable 'dset'

action <- 'Updating'
if(CREATE_VOCAB)
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

# --- Creating / updating vocab 
cat('>>> Creating / updating hash table VOCAB. \n')

WRDS <- names(n1g_u[order(n1g_u, decreasing=T)])            # word character vector  
if(CREATE_VOCAB){                                           # Create Vocab from scratch
    WRDS_H <- hash(WRDS, 1:length(WRDS))
    WRDS_H_INV <- hash(values(WRDS_H), keys(WRDS_H))
    cat('    New Vocab created with', length(WRDS), 'types. \n')
} else {                                                    # Update existing Vocab
    new_words <- updt.Vocab(WRDS)
    cat('    Vocab updated with', new_words, 'new types. \n')
}

cat('>>> End of script --createVocab--. \n')
# --- End of Script


