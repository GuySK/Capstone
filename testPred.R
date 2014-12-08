#
# Test predictions
#
source("./AWS/eda1_auxFunctions.R")
source("./AWS/predictFuncts.r")
# --- Params ---
SEED_NO = 193;
TEST_SIZE = 100;

# --- Take predictions ---
set.seed(SEED_NO)
testData <- dset[-train]
testData <- testData[sample(1:length(testData), TEST_SIZE)]
tst_df <- trainer(testData, tst_df)
View(tst_df)

# --- Evaluation ---
# End of Script