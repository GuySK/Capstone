#
# EDA of contextual impact on prediction
#

guessed <- with(prd_val_df3, ((Target == W1) | (Target == W2) | (Target == W3)))
guessed[is.na(guessed)] <- FALSE
nf_df <- prd_val_df3[!guessed,]

present <- rep(NA, nrow(nf_df))
for (i in 1:nrow(nf_df)){
    present[i] <- nf_df$Target[i] %in% ngram2(nf_df$Sentence[i], n = 1, encode = F)
}
sum(present) / nrow(nf_df)
View(nf_df[present, ])
wp_dict <- dict(as.character(nf_df[present, 'Target']))
wp_dict <- wp_dict[wp_dict > 1]
wp_dict <- wp_dict[order(wp_dict, decreasing = T)]
barplot(height = wp_dict, names.arg = names(wp_dict), 
        cex.names = 0.75,
        xlab = 'Targets', 
        main = 'Not found targets previously present in sentence')
codes <- nCode(names(wp_dict))
probs <- n1gn_df[n1gn_df$Key %in% codes, 'ProbWB']
names(probs) <- names(wp_dict)
probs

# 
present <- rep(NA, nrow(prd_val_df3))
for (i in 1:nrow(prd_val_df3)){
    present[i] <- prd_val_df3$Target[i] %in% ngram2(prd_val_df3$Sentence[i], n = 1, encode = F)
}
sum(present) / nrow(prd_val_df3)
View(prd_val_df3[present, ])

wp_dict <- dict(as.character(prd_val_df3[present, 'Target']))
wp_dict <- wp_dict[order(wp_dict, decreasing = T)]

barplot(height = wp_dict, names.arg = names(wp_dict), 
        cex.names = 0.75,
        xlab = 'Targets', 
        main = 'Targets already used in sentence')

# encode and replace unknown words in dict
codes <- nCode(names(wp_dict))
unks <- which(codes == 85572)
names(wp_dict)[unks] <- '<UNK>'

# get probability of unknown terms
unk_prob <- 1 - sum(n1gn_df$ProbWB)

probs <- rep(0, length(wp_dict))
probs[-unks] <- n1gn_df[n1gn_df$Key %in% codes[-unks], 'ProbWB']
probs[unks]  <- unk_prob

names(probs) <- names(wp_dict)
probs
barplot(height = probs, names.arg = names(probs), 
        cex.names = 0.75,
        xlab = 'Targets', 
        main = 'Probabilities of present words')

#' We should take into account only the words that have low probabilities, since there is
#' no reason to take as candidates those words that are very frequent. If they're frequent
#' it is only normal that they are being repeated.
#' How to differentiate ones from the others?
#' 
#' If words are variables of a multinomial distribution, their expected values in n tries
#' are E[Wk] = n * Pk for word k with k = 1,2, ... T the number of types in our vocab.
#' 
#' lets consider one case in which target is a repeated word
#' .

sent <- prd_val_df3[present,]$Sentence[1]
target <- prd_val_df3[present,]$Target[1]
tokens <- ngram2(sent, encode = F, n = 1)
nwords <- length(tokens)
wt <- dict(tokens)
pt <- sapply(names(wt), word_prob)
expected <- nwords * pt

names(wt)[order(log2(wt / expected), decreasing = T)]
n3gn_df[n3gn_df$Key == nCodeNgram('read the original'),]

w1 <- prd_val_df3[present,]$W1[1]
trigram <- paste('read the', w1, sep = ' ')
n3gn_df[n3gn_df$Key == nCodeNgram(trigram),]



