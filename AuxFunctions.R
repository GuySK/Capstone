#'
#' Aux functions
#'
na.pct <- function(x){
    #' returns a vector with the percentage of NA values 
    #' for each feature in a data frame
    round((apply(is.na(x),2,sum) / nrow(x)) * 100,2)
}
#'   
summ.na <- function(x, thresh=95){
    #' Prints out the list of features with a number of NAs that
    #' is beyond the specified threshold. Default is 95%. 
    prted <- 0
    na.percent <- na.pct(x)
    for (i in 1:ncol(x)){
        if (na.percent[i] >= thresh){
            cat(names(x)[i], na.percent[i], "\n")
            prted <- prted + 1
        }
    }
    if (!prted) 
        cat("No NAs above", thresh, "\n" )
    else
        cat(prted, "var(s) above threshold of", paste0(thresh,"%"), "\n")
}
#'
#'
#'
colclass <- function(x, type="factor"){
    cols <- rep(FALSE, ncol(x))
    for (i in 1:ncol(x)) {
        cols[i] <- (class(x[,i]) == type)
    }
    which(cols)
}
summ.void <- function(x) {
    cols <- colclass(x, "factor")
    sum <- rep(0, length(cols))
    name <- rep("", length(cols))
    for (i in 1:length(cols)){
        colch <- as.character(x[cols][,i])
        name[i] <- names(x[cols])[i]
        sum[i] <- round(sum(colch == "") / nrow(x) * 100,2)
    }
    cbind(name, sum)
}
#'
void.pct <- function(x) {
    res <- rep(0, ncol(x))          # init result
    cols <- colclass(x, "factor")   # get columns with factors
    colch <- apply(x[,cols],2, as.character) # convert them to char
    logcolch <- colch == ""         # turn them into logical
    res[cols] <- round((apply(logcolch,2,sum)) / nrow(colch) * 100,2) # compute percentage
    res                             # return results
}

#'
val.pct <- function(x, val="na"){
    #' returns a vector with the percentage of values of the 
    #' specified type for each feature in the data frame
    #' 
    if (val == "void") return(void.pct(x))
    if (val == "na") return(na.pct(x))
}
#'
dscat <- function(m) {
    # Returns a matrix's data scatter
    tot <- 0
    for(i in 1:ncol(m)) {
        tot <- sum(t(m[,i]) %*% m[,i]) + tot
    }
    tot
}
#' 
lds <- function(m,n) {
    # Computes n loadings of a matrix
    s <- svd(m)
    loads <- matrix(0, nrow=ncol(m), ncol=n)
    for (i in 1:n) {
        loads[,i] <- round(((s$d[i]** 0.5) * s$v[,i]),4)
    }
    loads
}
#'
contrib <- function(x,n) {
    # Computes an nD model contribution
    round((sum(svd(x)[[1]][1:n]**2) / dscat(x) * 100),4)
}
#'
#' Contingency table for confusion matrix
#'   
ct <- function(m) {
    #'
    #' Takes a confusion matrix and returns a list 
    #' with three matrices: a contingency table, 
    #' another computing sensitivity and a third one
    #' computing Positive predicting value [TP/(TP + FP)]   
    #'
    tr <- apply(m, 1, sum)
    m2 <- cbind(m, "Tot Prd" = tr)
    tc <- apply(m2, 2, sum)
    m2 <- rbind(m2, "Tot Ref"= tc)
    # 
    m3 <- sweep(m, 2, apply(m, 2, sum), "/")
    tc <- apply(m3, 2, sum)
    m3 <- rbind(m3, "Tot" = tc)
    m3 <- round(m3, 2)
    dimnames(m3)[1] <- list(c(dimnames(m)[[2]], "Ref"))
    #
    m4 <- sweep(m, 1, apply(m, 1, sum), "/")
    tr <- apply(m4, 1, sum)
    m4 <- cbind(m4, "Tot" = tr)
    m4 <- round(m4, 2)
    dimnames(m4)[2] <- list(c(dimnames(m)[[1]], "Prd"))
    #
    list(m2, m3, m4)
}
#
getAcc <- function(confMatrix, accMatrix, mdlName="") {
    # Accumulates accuracy and Kappa from a caret confusion matrix
    # for comparison. Returns a new or updated matrix.
    if (missing(accMatrix)) {
        if (mdlName == "") {
            mdlName <- "mdl1" 
        }
        accMatrix2 <- matrix(c(confMatrix$overall[1], confMatrix$overall[2]), 
                            ncol=2, byrow= T)
        dimnames(accMatrix2)[[1]] <- list(mdlName)
        dimnames(accMatrix2)[[2]] <- list("Accuracy", "Kappa")
    } else {
        if (mdlName == "") {
            mdlName <- paste("mdl", nrow(accMatrix)+1, sep="")
        }
        accMatrix2 <- rbind(accMatrix, c(confMatrix$overall[1], 
                                        confMatrix$overall[2]))
        dimnames(accMatrix2)[[1]] <- c(dimnames(accMatrix)[[1]], mdlName)
    }
    accMatrix2
}
# 
pause <- function(txt) {
    NMSG = 4
    msg <- rep("", NMSG)
    msg[1] <- "Script stopped. \n"
    msg[2] <- "Press 'C/c' to cancel script execution or 'Enter' to continue.\n"
    msg[3] <- "Script execution cancelled.\n"
    msg[4] <- "Script execution continues. \n"
    if (missing(txt)) # check if user text present
        txt <- msg[1] # user text defaults to msg[1]
    cat(txt, "\n")    # user text here       
    resp <- readline(msg[2]) # get user's response
    if (tolower(resp) == "c") # C/c needed to cancel script
        stop(msg[3])  # execution aborted
    cat(msg[4])       # execution continues
}
#
histsum <- function(x, units="", tit, ...) {
    if (!(missing(units)))
        units <- paste0("[", units, "]")
    varname <- deparse(substitute(x))
    varunit <- paste(varname, units)
    if (missing(tit))
        tit <- paste0("Histogram of ", varname)
    mycols <- c("red", "blue", "green")
    mynames <- c("Median", "Mean", "IQR")
    s <- summary(x)
    hist(x, main=tit, xlab=varunit, ...)
    abline(v=s[3], col=mycols[1])
    abline(v=s[4], col=mycols[2])
    abline(v=s[2], col=mycols[3])
    abline(v=s[5], col=mycols[3])
    legend("topright", legend=mynames, lwd=1, col=mycols, bty="n", cex=0.75)
    cat(">> ", tit, "\n")
    print(summary(x))
}
#'
#' sampCI function. Computes a sample CI for the Mean
#'
sampCI <- function(x, cilevel=0.95, method="percent"){
    #
    # computes CI of sample distribution
    # using percentile or std error methods
    # Takes a vector, ci level and method parameters
    # returns the sample confidence interval for the Mean
    #
    methods <- c("percent", "se")               # list of supported methods
    if (pmatch(method, methods) == "NA"){       # check if method is valid
        cat("Error. Unsupported method.")
        ci <- NA
    }
        
    if (method == "percent"){                   # percent method
        xord <- x[order(x)]                     # first order vector
        q1 <-  ceiling(((1 - cilevel) / 2) * length(xord)) # compute quantile
        ci <- c(xord[q1+1], xord[length(xord) - q1])       # compute conf interval
    }
    
    if (method == "se"){                        # std error method 
        se <- sd(x)                             # compute std error            
        ci <- mean(x) + c(-1,1) * qnorm(cilevel) * se   # calculate ci  
    }
    ci                                          # return confidence interval
}
#
#
grpsumm <- function(x, y){
    n <- length(levels(y))
    grp <- rep(NA, n)
    grpsd <- rep(0, n)
    grpmean <- rep(0, n)
    grpmedian <- rep(0, n)
    grpsize <- rep(0, n)
    for (i in 1:n) {
        grp[i] <- levels(y)[i]
        grpsd[i] <- sd(x[y == grp[i]])
        grpmean[i] <- mean(x[y == grp[i]])
        grpmedian[i] <- median(x[y == grp[i]])        
        grpsize[i] <- length(x[y == grp[i]])
    }
    df <- data.frame(Group=grp, Size=grpsize, Median=grpmedian, Mean=grpmean, Sd=grpsd, 
                     row.names=1)
    round(df,2)
}
# 
# same.df: Check if two data frames are equal 
#
same.df <- function(x,y){
    # checks if two data frames are equal
    # Takes two data frames and returns logic
    if ((sum(dim(x) == dim(y)) / 2) != 1)             # df dimensions must be the same
        return(FALSE)
    if ((sum(x == y) / (dim(x)[1] * dim(x)[2]) == 1)) # and everything must be equal
        return(TRUE)
    return(FALSE)                                     
}
#

