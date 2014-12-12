#
# tryTags - Gets POS tags from POS data frame
#
tryTags <- function(tags, n = 2,  df = NULL){

    if (is.null(df)) {
        df <- paste0('t', n, 'g_df')                      # point to n-tag data frame 
        df <- get(df)
    }
    return(getNgram(df, tags, decoded=F))
}